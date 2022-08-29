{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Indexed qualified as Indexed
import Control.Monad (when)
import Data.Default qualified as Default
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (CardanoTx, Redeemer (Redeemer))
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut,
  TxOutRef,
  ciTxOutAddress,
 )
import Ledger.Value qualified as Value
import Plutus.ChainIndex (
  PageQuery (PageQuery, pageQueryLastItem, pageQuerySize),
 )
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (Datum, getDatum), MintingPolicy)
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (TokenName, unTokenName))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (RootHash (unRootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams, amount, recipient, sidechainParams),
  MintParams (MintParams, amount, entryHash, index, merkleProof, recipient, sidechainEpoch, sidechainParams),
  SidechainParams (genesisUtxo),
 )
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (Ds, dsConf),
  DsConfMint (DsConfMint, dscmTxOutRef),
  DsMint,
  Node (nPrefix),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OnChain.FUELMintingPolicy (FUELMint (FUELMint, fmMptRootTokenCurrencySymbol, fmSidechainParams))
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (
  FUELRedeemer (MainToSide, SideToMain),
  MerkleTreeEntry (MerkleTreeEntry, mteAmount, mteHash, mteIndex, mteRecipient, mteSidechainEpoch),
 )

import Prelude qualified

{- | 'findMPTRootToken sc mp me' constructs the merkle root from the merkle
 proof @mp@ and the merkle entry @me@; and looks for the utxo with this
 MPTRootToken which corresponds to the given sidechain parameters @sc@ which is
 paid to an MPTRootTokenValidator.
-}
findMPTRootToken ::
  AsContractError e =>
  SidechainParams ->
  RootHash ->
  Contract w s e [(TxOutRef, ChainIndexTxOut)]
findMPTRootToken sc rh =
  filter go . Indexed.itoList <$> Utils.utxosWithCurrency pq assetClass
  where
    pq = PageQuery {pageQuerySize = Default.def, pageQueryLastItem = Nothing}

    assetClass = Value.assetClass (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sc) $ TokenName $ unRootHash rh

    -- TODO: as an optimization, I don't think testing if it is paid to the
    -- MPTRootTokenValidator is really necessary since the existence of the
    -- token is enough to imply that the sidechain has initiated this
    -- transaction.
    go :: (TxOutRef, ChainIndexTxOut) -> Bool
    go (_oref, o) = maybe False (== MPTRootTokenValidator.address sc) $ o Fold.^? ciTxOutAddress

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient} = do
  let -- Variables for the distributed set
      --------------------------------------------------
      oref :: TxOutRef
      oref = genesisUtxo sidechainParams

      ds :: Ds
      ds = Ds {dsConf = dsconf}

      dsconf :: CurrencySymbol
      dsconf = DistributedSet.dsConfCurSymbol $ DsConfMint {dscmTxOutRef = oref}

      dsm :: DsMint
      dsm = DistributedSet.dsToDsMint ds

      -- Variables for the FUEL minting (burning) policy
      --------------------------------------------------
      fm =
        FUELMint
          { fmSidechainParams = sidechainParams
          , fmMptRootTokenCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams
          , fmDsLeafCurrencySymbol = DistributedSet.dsLeafCurSymbol dsm
          }
      policy = FUELMintingPolicy.mintingPolicy fm
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraints.mintingPolicy policy)
    (Constraints.mustMintValueWithRedeemer redeemer value)

{- | 'mint' does the following

      1. Locates the utxo with the MPTRootToken, which proves that the
      transaction happened on the sidechain.

      2. Build the transaction by

          * Providing a reference input to the transaction with the
          MPTRootToken

          TODO: it doesn't do this: we need PlutusV2 for reference inputs.
          For now, we consume and force it pay the value back (this isn't
          checked on chain since we will remove this feature later)

          * Calling the FUELMintingPolicy as usual.
-}
mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint MintParams {amount, index, sidechainEpoch, sidechainParams, recipient, merkleProof, entryHash} =
  Contract.ownPaymentPubKeyHash >>= \ownPkhHash ->
    let -- Variables for the FUEL minting policy..
        -----------------------------------
        mte =
          MerkleTreeEntry
            { mteIndex = index
            , mteAmount = amount
            , mteRecipient = getPubKeyHash $ unPaymentPubKeyHash recipient
            , mteSidechainEpoch = sidechainEpoch
            , mteHash = entryHash
            }
        cborMte = MPTRootTokenMintingPolicy.serialiseMte mte
        root = MerkleTree.rootMp cborMte merkleProof

        -- Variables for the  distributed set..
        -----------------------------------
        oref :: TxOutRef
        oref = genesisUtxo sidechainParams

        ds :: Ds
        ds = Ds {dsConf = dsconf}

        dsconf :: CurrencySymbol
        dsconf = DistributedSet.dsConfCurSymbol $ DsConfMint {dscmTxOutRef = oref}

        dsm :: DsMint
        dsm = DistributedSet.dsToDsMint ds

        pmp :: MintingPolicy
        pmp = DistributedSet.dsPrefixPolicy dsm

        lmp :: MintingPolicy
        lmp = DistributedSet.dsLeafPolicy dsm

        -- > cborMteHashed = Builtins.blake2b_256 cborMte
        -- > cborMteHashedTn = TokenName cborMteHashed
        cborMteHashed = cborMte
        cborMteHashedTn = TokenName cborMteHashed

        -- this is the string we are actually checking if it is in the
        -- distributed set.
        str = cborMte
     in findMPTRootToken sidechainParams root >>= \case
          [] -> Contract.throwError "error FUELMintingPolicy: no MPT root token UTxO found"
          mptUtxo : _ ->
            DistributedSet.findDsConfOutput ds >>= \(confRef, confO, confDat) ->
              DistributedSet.findDsOutput ds cborMteHashedTn >>= \case
                Nothing -> Contract.throwError "error FUELMintingPolicy: no distributed set output found"
                Just (nodeRef, oNode, datNode, tnNode) -> do
                  Contract.logInfo @Prelude.String $ "mpt"
                  Contract.logInfo @Prelude.String $ Prelude.show mptUtxo
                  Contract.logInfo @Prelude.String $ "conf ds output"
                  Contract.logInfo @Prelude.String $ Prelude.show confRef
                  Contract.logInfo @Prelude.String $ "ds output"
                  Contract.logInfo @Prelude.String $ Prelude.show nodeRef

                  let -- Variables for the FUEL minting policy
                      -----------------------------------
                      fm =
                        FUELMint
                          { fmSidechainParams = sidechainParams
                          , fmMptRootTokenCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams
                          , fmDsLeafCurrencySymbol = DistributedSet.dsLeafCurSymbol dsm
                          }
                      fuelPolicy = FUELMintingPolicy.mintingPolicy fm
                      value = Value.singleton (Ledger.scriptCurrencySymbol fuelPolicy) "FUEL" amount
                      fuelRedeemer = Redeemer $ toBuiltinData $ SideToMain mte merkleProof

                      -- TODO: the following line should be removed with reference inputs:
                      mptRootTokenValidator = MPTRootTokenValidator.validator sidechainParams
                      mptRootTokenValue =
                        Value.singleton
                          (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams)
                          (TokenName $ unRootHash root)
                          1
                      -- TODO: end of lines that should be removed with reference inputs

                      -- Variables for the distributed set
                      -----------------------------------
                      node = DistributedSet.mkNode (unTokenName tnNode) datNode
                      nodes = DistributedSet.insertNode cborMteHashed node
                      nodeRedeemer = Redeemer $ Class.toBuiltinData ()

                      -- Building the transaction...
                      -----------------------------------
                      lookups =
                        Constraints.mintingPolicy fuelPolicy
                          Prelude.<> Constraints.unspentOutputs (uncurry Map.singleton mptUtxo)
                          --  lookups for the distributed set
                          -----------------------------------
                          Prelude.<> Constraints.unspentOutputs (Map.singleton nodeRef oNode)
                          Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
                          Prelude.<> Constraints.mintingPolicy pmp
                          Prelude.<> Constraints.mintingPolicy lmp
                          Prelude.<> Constraints.otherScript (DistributedSet.insertValidator ds)
                          -- TODO: the following line should be removed with reference inputs:
                          Prelude.<> Constraints.otherScript mptRootTokenValidator
                          Prelude.<> Constraints.unspentOutputs (Map.singleton confRef confO)
                          Prelude.<> Constraints.otherScript (DistributedSet.dsConfValidator ds)
                      -- TODO: end of line that should be removed with reference inputs

                      tx =
                        Prelude.mconcat
                          [ -- Constraints for the minting policy
                            Constraints.mustMintValueWithRedeemer fuelRedeemer value
                          , Constraints.mustPayToPubKey recipient value
                          , Constraints.mustBeSignedBy ownPkhHash
                          , -- Constraints for the distributed set
                            Constraints.mustSpendScriptOutput nodeRef nodeRedeemer
                          , Constraints.mustMintValue
                              ( Value.singleton
                                  (DistributedSet.dsLeafCurSymbol dsm)
                                  (TokenName {unTokenName = str})
                                  1
                              )
                          , flip (Fold.foldMapOf Fold.folded) nodes $
                              \n ->
                                let nTn = TokenName $ nPrefix n
                                    val = Value.singleton (DistributedSet.dsPrefixCurSymbol dsm) nTn 1
                                 in if unTokenName nTn == nPrefix node
                                      then
                                        Constraints.mustPayToTheScript
                                          (DistributedSet.nodeToDatum n)
                                          val
                                      else
                                        Constraints.mustPayToOtherScript
                                          (Scripts.validatorHash (DistributedSet.insertValidator ds))
                                          (Datum (Class.toBuiltinData (DistributedSet.nodeToDatum n)))
                                          val
                                          Prelude.<> Constraints.mustMintValue val
                          , -- TODO: the following line should be removed with reference inputs:
                            Constraints.mustSpendScriptOutput (fst mptUtxo) Scripts.unitRedeemer
                          , Constraints.mustPayToOtherScript (MPTRootTokenValidator.hash sidechainParams) Scripts.unitDatum mptRootTokenValue
                          , Constraints.mustSpendScriptOutput confRef Scripts.unitRedeemer
                          , Constraints.mustPayToOtherScript
                              (DistributedSet.dsConfValidatorHash ds)
                              (Datum {getDatum = Class.toBuiltinData confDat})
                              (Value.assetClassValue (Value.assetClass dsconf DistributedSet.dsConfTokenName) 1)
                              -- TODO: end of lines that should be removed with reference inputs
                          ]

                  Contract.logInfo @Prelude.String $ "Original node"
                  Contract.logInfo $ Prelude.show node
                  Contract.logInfo @Prelude.String $ "New nodes:"
                  Contract.logInfo $ Prelude.show nodes

                  ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups tx

                  Contract.logInfo @Prelude.String $ "Transaction: "
                  Contract.logInfo $ Prelude.show ledgerTx

                  return ledgerTx
