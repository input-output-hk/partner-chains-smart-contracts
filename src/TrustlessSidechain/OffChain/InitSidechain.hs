module TrustlessSidechain.OffChain.InitSidechain where

import Control.Lens.Indexed qualified as Indexed
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (Datum (Datum), TxOutRef, validatorHash)
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusPrelude (void)
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  InitSidechainParams (initChainId, initCommittee, initGenesisHash, initMint, initUtxo),
  SidechainParams (SidechainParams, chainId, genesisHash, genesisMint, genesisUtxo),
 )
import TrustlessSidechain.OnChain.Types (
  UpdateCommitteeHash (UpdateCommitteeHash, cToken),
  UpdateCommitteeHashDatum (UpdateCommitteeHashDatum, committeeHash),
  UpdatingCommitteeHash,
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash (
  InitCommitteeHashMint (
    InitCommitteeHashMint,
    icTxOutRef
  ),
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude qualified

{- | 'initSidechain' creates the 'SidechainParams' of a new sidechain which
 parameterize validators and minting policies in order to uniquely identify
 them. See the following notes for what 'initSidechain' must initialize.

 Note [Initializing the Committee Hash]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The intialization step of the committee hash is done in two steps.

  (1) Create an NFT which identifies the committee hash

  (2) Spend that NFT to a script output which contains the committee hash

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
            let ichm = InitCommitteeHashMint {icTxOutRef = oref}

                nft = UpdateCommitteeHash.committeeHashAssetClass ichm

                val = Value.assetClassValue nft 1

                uch = UpdateCommitteeHash {cToken = nft}

                ndat =
                  Datum $
                    PlutusTx.toBuiltinData $
                      UpdateCommitteeHashDatum
                        { committeeHash =
                            UpdateCommitteeHash.aggregateKeys $
                              initCommittee isp
                        }

                validator = UpdateCommitteeHash.updateCommitteeHashValidator uch

                lookups =
                  Constraints.mintingPolicy (UpdateCommitteeHash.committeeHashPolicy ichm)
                    Prelude.<> Constraints.unspentOutputs (Map.singleton oref o)
                    Prelude.<> Constraints.otherScript validator

                tx =
                  Constraints.mustSpendPubKeyOutput oref
                    Prelude.<> Constraints.mustMintValue val
                    Prelude.<> Constraints.mustPayToOtherScript
                      (validatorHash validator)
                      ndat
                      val

            ledgerTx <- Contract.submitTxConstraintsWith @UpdatingCommitteeHash lookups tx

            void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

            Contract.logInfo $ "Minted " <> Prelude.show val <> " and paid to script validator"

            return
              SidechainParams
                { chainId = initChainId isp
                , genesisHash = initGenesisHash isp
                , genesisUtxo = oref
                , genesisMint = initMint isp
                }

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
