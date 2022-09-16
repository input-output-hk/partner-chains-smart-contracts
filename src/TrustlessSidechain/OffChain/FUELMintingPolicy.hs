{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy (burn, mint, mintWithUtxos) where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Indexed qualified as Indexed
import Control.Monad (when)
import Data.Default qualified as Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (CardanoTx, ChainIndexTxOut, Redeemer (Redeemer), TxOutRef, scriptCurrencySymbol)
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut,
  TxOutRef,
 )
import Ledger.Value qualified as Value
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum (Datum, getDatum), MintingPolicy)
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol, unCurrencySymbol), TokenName (TokenName, unTokenName))
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
  SidechainParams (genesisMint, genesisUtxo),
 )
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (Ds, dsConf),
  DsConfDatum (dscFUELPolicy, dscKeyPolicy),
  DsConfMint (DsConfMint, dscmTxOutRef),
  DsKeyMint,
  Node (nKey),
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
findMPTRootToken sc rh = Indexed.itoList <$> Utils.utxosWithCurrency Default.def assetClass
  where
    assetClass =
      Value.assetClass
        ( MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol $
            MPTRootTokenMintingPolicy.signedMerkleRootMint sc
        )
        $ TokenName $ unRootHash rh

{- | 'burn' will burn the given amount of FUEL in 'BurnParams'. Note that this
 expects the 'amount' in 'BurnParams' to be negative -- see 'BurnParams' for
 more details.
-}
burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient} = do
  let -- Variables for the distributed set
      --------------------------------------------------
      dsconf :: CurrencySymbol
      dsconf =
        DistributedSet.dsConfCurrencySymbol $
          DsConfMint {dscmTxOutRef = genesisUtxo sidechainParams}

      dskm :: DsKeyMint
      dskm = DistributedSet.dsToDsKeyMint (Ds {dsConf = dsconf})

      -- Variables for the FUEL minting (burning) policy
      --------------------------------------------------
      fm =
        FUELMint
          { fmSidechainParams = sidechainParams
          , fmMptRootTokenCurrencySymbol =
              MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol $
                MPTRootTokenMintingPolicy.signedMerkleRootMint
                  sidechainParams
          , fmDsKeyCurrencySymbol = DistributedSet.dsKeyCurrencySymbol dskm
          }
      policy = FUELMintingPolicy.mintingPolicy fm
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraints.mintingPolicy policy)
    (Constraints.mustMintValueWithRedeemer redeemer value)

{-
      1. Locates a utxo with the MPTRootToken, which proves that the
      transaction happened on the sidechain.

      2. Build the transaction by

          * Providing a reference input to the transaction with the
          MPTRootToken

          TODO: it doesn't do this: we need PlutusV2 for reference inputs.
          For now, we consume and force it pay the value back (this isn't
          checked on chain since we will remove this feature later)

          * Providing the reference input for the configuration of the
          distributed set (i.e., the utxo which saves the validator hash /
          policy hashes associated with the distributed set).

          * Verifying that the hash of the 'MerkleTreeEntry' was just inserted
          in the distributed set (by finding the unique utxo [see
          'TrustlessSidechain.OnChain.DistributedSet'] which proves that the
          hash is not in the set)

          * Calling the FUELMintingPolicy

  Alternatively, if we are not using the distributed set (i.e., 'genesisMint'
  is @Just _@), then we use the @Map TxOutRef ChainIndexTxOut@ argument to use
  the legacy one shot minting policy. Once we can replace with the distributed
  set, we can remove that extra parameter.

  TODO: remove the one shot minting policy.
-}
mintWithUtxos :: Map TxOutRef ChainIndexTxOut -> MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mintWithUtxos utxos MintParams {amount, index, sidechainEpoch, sidechainParams, recipient, merkleProof, entryHash} =
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
        dsconf :: CurrencySymbol
        dsconf =
          DistributedSet.dsConfCurrencySymbol $
            DsConfMint {dscmTxOutRef = genesisUtxo sidechainParams}

        ds :: Ds
        ds = Ds {dsConf = dsconf}

        dskm :: DsKeyMint
        dskm = DistributedSet.dsToDsKeyMint (Ds {dsConf = dsconf})

        kmp :: MintingPolicy
        kmp = DistributedSet.dsKeyPolicy dskm

        -- > cborMteHashed = Builtins.blake2b_256 cborMte
        -- > cborMteHashedTn = TokenName cborMteHashed
        cborMteHashed = cborMte
        cborMteHashedTn = TokenName cborMteHashed
     in findMPTRootToken sidechainParams root >>= \case
          [] -> Contract.throwError "error FUELMintingPolicy: no MPT root token UTxO found"
          mptUtxo : _ -> case genesisMint sidechainParams of
            -- TODO Remove the oneshot minting policy later... the distributed
            -- set superceeds and replaces the one shot minting policy.
            Just gutxo -> do
              let smrmParams = MPTRootTokenMintingPolicy.signedMerkleRootMint sidechainParams
                  fm =
                    FUELMint
                      { fmSidechainParams = sidechainParams
                      , fmMptRootTokenCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol smrmParams
                      , fmDsKeyCurrencySymbol = CurrencySymbol {unCurrencySymbol = ""}
                      }
                  policy = FUELMintingPolicy.mintingPolicy fm
                  value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
                  redeemer = Redeemer $ toBuiltinData $ SideToMain mte merkleProof
                  -- TODO: the following line should be removed with reference inputs:
                  mptRootTokenValidator = MPTRootTokenValidator.validator sidechainParams
                  -- TODO: the following line should be removed with reference inputs:
                  mptRootTokenValue =
                    Value.singleton
                      (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol smrmParams)
                      (TokenName $ unRootHash root)
                      1

                  lookups =
                    Constraints.mintingPolicy policy
                      Prelude.<> Constraints.unspentOutputs (utxos Prelude.<> uncurry Map.singleton mptUtxo)
                      -- TODO: the following line should be removed with reference inputs:
                      Prelude.<> Constraints.otherScript mptRootTokenValidator
                  tx =
                    Constraints.mustMintValueWithRedeemer redeemer value
                      Prelude.<> Constraints.mustPayToPubKey recipient value
                      Prelude.<> Constraints.mustSpendScriptOutput (fst mptUtxo) Scripts.unitRedeemer
                      Prelude.<> Constraints.mustSpendPubKeyOutput gutxo
                      -- TODO: the following line should be removed with reference inputs:
                      Prelude.<> Constraints.mustPayToOtherScript (MPTRootTokenValidator.hash sidechainParams) Scripts.unitDatum mptRootTokenValue
              when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
              Contract.submitTxConstraintsWith @FUELRedeemer lookups tx
            Nothing ->
              DistributedSet.findDsConfOutput ds >>= \(confRef, confO, confDat) ->
                DistributedSet.findDsOutput ds cborMteHashedTn >>= \case
                  Nothing -> Contract.throwError "error FUELMintingPolicy: no distributed set output found"
                  Just ((nodeRef, oNode, datNode, tnNode), nodes) -> do
                    -- These lines are useful for debugging..

                    let -- Variables for the FUEL minting policy
                        -----------------------------------
                        fm =
                          FUELMint
                            { fmSidechainParams = sidechainParams
                            , fmMptRootTokenCurrencySymbol = smrmCurrencySymbol
                            , fmDsKeyCurrencySymbol = DistributedSet.dsKeyCurrencySymbol dskm
                            }
                        fuelPolicy = FUELMintingPolicy.mintingPolicy fm
                        value = Value.singleton (Ledger.scriptCurrencySymbol fuelPolicy) "FUEL" amount
                        fuelRedeemer = Redeemer $ toBuiltinData $ SideToMain mte merkleProof

                        mptParam = sidechainParams
                        -- TODO: the following line should be removed with reference inputs:
                        mptRootTokenValidator =
                          MPTRootTokenValidator.validator mptParam
                        mptRootTokenValue =
                          Value.singleton
                            smrmCurrencySymbol
                            (TokenName $ unRootHash root)
                            1
                        -- TODO: end of lines that should be removed with reference inputs

                        smrmParam = MPTRootTokenMintingPolicy.signedMerkleRootMint sidechainParams
                        smrmCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol smrmParam

                        -- Variables for the distributed set
                        -----------------------------------
                        node = DistributedSet.mkNode (unTokenName tnNode) datNode
                        nodeRedeemer = Redeemer $ Class.toBuiltinData ()

                        -- Building the transaction...
                        -----------------------------------
                        lookups =
                          Constraints.mintingPolicy fuelPolicy
                            Prelude.<> Constraints.unspentOutputs (uncurry Map.singleton mptUtxo)
                            --  lookups for the distributed set
                            -----------------------------------
                            Prelude.<> Constraints.unspentOutputs (Map.singleton nodeRef oNode)
                            Prelude.<> Constraints.mintingPolicy kmp
                            Prelude.<> Constraints.otherScript (DistributedSet.insertValidator ds)
                            -- TODO: the following lines should be removed with reference inputs:
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
                            , {-
                              , -- Constraints for the oneshot minting policy (we
                                -- assume that if the oneshot mintnig policy is inplace,
                                -- then we definetly want to use it
                                maybe mempty Constraints.mustSpendPubKeyOutput $ genesisMint sidechainParams
                                -}
                              -- Constraints for the distributed set
                              Constraints.mustSpendScriptOutput nodeRef nodeRedeemer
                            , flip (Fold.foldMapOf Fold.folded) nodes $
                                \n ->
                                  let nTn = TokenName $ nKey n
                                      val = Value.singleton (DistributedSet.dsKeyCurrencySymbol dskm) nTn 1
                                   in if unTokenName nTn == nKey node
                                        then
                                          Constraints.mustPayToOtherScript
                                            (Scripts.validatorHash (DistributedSet.insertValidator ds))
                                            (Datum (Class.toBuiltinData (DistributedSet.nodeToDatum n)))
                                            val
                                        else
                                          Constraints.mustPayToOtherScript
                                            (Scripts.validatorHash (DistributedSet.insertValidator ds))
                                            (Datum (Class.toBuiltinData (DistributedSet.nodeToDatum n)))
                                            val
                                            Prelude.<> Constraints.mustMintValue val
                            , -- TODO: the following line should be removed with reference inputs:
                              Constraints.mustSpendScriptOutput (fst mptUtxo) Scripts.unitRedeemer
                            , Constraints.mustPayToOtherScript
                                (MPTRootTokenValidator.hash mptParam)
                                Scripts.unitDatum
                                mptRootTokenValue
                            , Constraints.mustSpendScriptOutput confRef Scripts.unitRedeemer
                            , Constraints.mustPayToOtherScript
                                (DistributedSet.dsConfValidatorHash ds)
                                (Datum {getDatum = Class.toBuiltinData confDat})
                                (Value.assetClassValue (Value.assetClass dsconf DistributedSet.dsConfTokenName) 1)
                                -- TODO: end of lines that should be removed with reference inputs
                            ]

                    -- This check is important so all participants can
                    -- independently verify that the system has been set up
                    -- correctly.... This follows from the fact that the utxo
                    -- with the config is unspendable, and we assume that all
                    -- participants know the protocol.
                    unless
                      ( dscKeyPolicy confDat == DistributedSet.dsKeyCurrencySymbol dskm
                          && dscFUELPolicy confDat == FUELMintingPolicy.currencySymbol fm
                      )
                      $ Contract.throwError "error FUELMintingPolicy: misconfigured distributed set"

                    Contract.submitTxConstraintsWith @Void lookups tx

-- | see 'mintWithUtxos'
mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint = mintWithUtxos Prelude.mempty
