module TrustlessSidechain.OffChain.InitSidechain where

import Control.Lens.Indexed qualified as Indexed
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (TxOutRef)
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Datum (Datum, getDatum))
import Ledger.Tx qualified as Tx
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (AssetClass (unAssetClass), TokenName (TokenName))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusPrelude (void)
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  InitSidechainParams (initChainId, initCommittee, initGenesisHash, initMint, initUtxo),
  SidechainParams (SidechainParams, chainId, genesisHash, genesisMint, genesisUtxo),
 )
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (Ds, dsConf),
  DsConfDatum (DsConfDatum, dscKeyPolicy),
  DsConfMint (DsConfMint, dscmTxOutRef),
  Node (nKey),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OnChain.FUELMintingPolicy (FUELMint (FUELMint, fmMptRootTokenCurrencySymbol, fmSidechainParams))
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy (
  SignedMerkleRootMint (
    SignedMerkleRootMint,
    smrmSidechainParams,
    smrmUpdateCommitteeHashCurrencySymbol
  ),
 )
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.UpdateCommitteeHash (
  InitCommitteeHashMint (
    InitCommitteeHashMint,
    icTxOutRef
  ),
  UpdateCommitteeHash (UpdateCommitteeHash, cToken),
  UpdateCommitteeHashDatum (UpdateCommitteeHashDatum, committeeHash),
  UpdatingCommitteeHash,
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude qualified

{- | 'initSidechain' creates the 'SidechainParams' of a new sidechain which
 parameterize validators and minting policies in order to uniquely identify
 them. See the following notes for what 'initSidechain' must initialize.

 Note [Initializing the Committee Hash]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The intialization step of the committee hash is done as follows.

  (1) Create an NFT which identifies the committee hash / spend the NFT to the
  script output which contains the committee hsah

 Note [Initializing the Distributed Set]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The intialization step of the distributed set is done as follows.

  (1) Create an NFT and pay this to a script which holds 'DsConfDatum' which is
  the minting policy of all the scripts.

  (2) Mint node which corresponds to the root of the distributed set
  'DistributedSet.rootNode'

 Here, we create a transaction which executes both of these steps with a single
 transaction.
-}
initSidechain :: InitSidechainParams -> Contract () TrustlessSidechainSchema Text SidechainParams
initSidechain isp =
  let oref = initUtxo isp
   in Contract.txOutFromRef oref
        >>= \case
          Nothing -> Contract.throwError "bad 'initUtxo'"
          Just o -> do
            let sc =
                  SidechainParams
                    { chainId = initChainId isp
                    , genesisHash = initGenesisHash isp
                    , genesisUtxo = oref
                    , genesisMint = initMint isp
                    }

                -- Variables for initializing the committee hash
                -------------------------------------------------
                ichm = InitCommitteeHashMint {icTxOutRef = oref}
                nftCommittee = UpdateCommitteeHash.committeeHashAssetClass ichm
                valCommittee = Value.assetClassValue nftCommittee 1
                uchCommittee = UpdateCommitteeHash {cToken = nftCommittee}
                datCommitee =
                  UpdateCommitteeHashDatum
                    { committeeHash =
                        UpdateCommitteeHash.aggregateKeys $
                          initCommittee isp
                    }

                -- Variables for initializing the distributed set
                -------------------------------------------------
                ds = Ds {dsConf = dsconf}
                dsconf = DistributedSet.dsConfCurrencySymbol $ DsConfMint {dscmTxOutRef = oref}
                dskm = DistributedSet.dsToDsKeyMint ds

                pmp = DistributedSet.dsKeyPolicy dskm

                -- the prefix policy of the distributed set
                smDsKey = DistributedSet.dsKeyCurrencySymbol dskm
                tnDsKey = TokenName $ nKey DistributedSet.rootNode
                astDsKey = Value.assetClass smDsKey tnDsKey
                valDsKey = Value.assetClassValue astDsKey 1
                datDsKey = DistributedSet.nodeToDatum DistributedSet.rootNode

                -- the config policy of the distributed set
                cast = Value.assetClass dsconf DistributedSet.dsConfTokenName
                valConfDs = Value.assetClassValue cast 1
                datConfDs =
                  DsConfDatum
                    { dscKeyPolicy = DistributedSet.dsKeyCurrencySymbol dskm
                    , dscFUELPolicy =
                        FUELMintingPolicy.currencySymbol
                          FUELMint
                            { fmMptRootTokenCurrencySymbol =
                                MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol
                                  SignedMerkleRootMint
                                    { smrmSidechainParams = sc
                                    , smrmUpdateCommitteeHashCurrencySymbol =
                                        fst $ unAssetClass nftCommittee
                                    }
                            , fmSidechainParams = sc
                            , fmDsKeyCurrencySymbol = DistributedSet.dsKeyCurrencySymbol dskm
                            }
                    }
                cmp = DistributedSet.dsConfPolicy DsConfMint {dscmTxOutRef = oref}

                -- Building the transaction
                -------------------------------------------------
                lookups =
                  Constraints.unspentOutputs (Map.singleton oref o)
                    -- lookups for the update committee hash...
                    Prelude.<> Constraints.mintingPolicy (UpdateCommitteeHash.committeeHashPolicy ichm)
                    Prelude.<> Constraints.typedValidatorLookups
                      (UpdateCommitteeHash.typedUpdateCommitteeHashValidator uchCommittee)
                    -- lookups for the distributed set...
                    Prelude.<> Constraints.otherScript (DistributedSet.insertValidator ds)
                    Prelude.<> Constraints.mintingPolicy pmp
                    Prelude.<> Constraints.mintingPolicy cmp

                tx =
                  Constraints.mustSpendPubKeyOutput oref
                    -- minting the committee hash
                    Prelude.<> Constraints.mustMintValue valCommittee
                    Prelude.<> Constraints.mustPayToTheScript datCommitee valCommittee
                    -- minting the distributed set
                    Prelude.<> Constraints.mustMintValue valDsKey
                    Prelude.<> Constraints.mustPayToOtherScript
                      (DistributedSet.insertValidatorHash ds)
                      (Datum {getDatum = IsData.toBuiltinData datDsKey})
                      valDsKey
                    Prelude.<> Constraints.mustMintValue valConfDs
                    Prelude.<> Constraints.mustPayToOtherScript
                      (DistributedSet.dsConfValidatorHash ds)
                      (Datum {getDatum = IsData.toBuiltinData datConfDs})
                      valConfDs

            ledgerTx <- Contract.submitTxConstraintsWith @UpdatingCommitteeHash lookups tx

            void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

            return sc

{- | 'ownTxOutRef' gets a 'TxOutRef' from 'Contract.ownPaymentPubKeyHash'. This
 is used in the test suite for convience to make intializing the sidechain a
 bit more terse.

 Note: This assumes that we are not interested in having a 'StakePubKeyHash'
-}
ownTxOutRef :: Contract w s Text TxOutRef
ownTxOutRef =
  Contract.ownPaymentPubKeyHash
    >>= fmap Indexed.itoList . Contract.utxosAt . flip Address.pubKeyHashAddress Nothing
    >>= \case
      (oref, _) : _ -> return oref
      _ -> Contract.throwError "error 'ownTxOutRef': no UTxO available "
