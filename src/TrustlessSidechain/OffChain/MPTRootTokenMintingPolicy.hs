{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.MPTRootTokenMintingPolicy where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (CardanoTx)
import Ledger qualified
import Ledger.Constraints qualified as Constraint
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Scripts (Datum (Datum, getDatum), Redeemer (Redeemer, getRedeemer))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  SaveRootParams (SaveRootParams, committeePubKeys, merkleRoot, sidechainParams, signatures, threshold),
  SidechainParams (genesisUtxo),
 )
import TrustlessSidechain.OffChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (
  SignedMerkleRoot (SignedMerkleRoot, committeePubKeys, merkleRoot, signatures, threshold),
  UpdateCommitteeHashRedeemer (UpdateCommitteeHashRedeemer, committeePubKeys, committeeSignatures, newCommitteeHash),
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash (
  InitCommitteeHashMint (InitCommitteeHashMint, icTxOutRef),
  UpdateCommitteeHash (UpdateCommitteeHash, cToken),
  UpdateCommitteeHashDatum (committeeHash),
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude qualified

{- | 'saveRoot' will save the given Merkle root on chain provided that the
 committee hash has signed the new root, and the committee hash which has
 signed the new merkle root correpsonds to the hash saved on chain.

 See the minting policy for more details in
 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy'

 TODO: this should have the committee hash as a reference input, but that's a
 PlutusV2 feature! We must change this later
-}
saveRoot :: SaveRootParams -> Contract () TrustlessSidechainSchema Text CardanoTx
saveRoot SaveRootParams {sidechainParams, merkleRoot, threshold, signatures, committeePubKeys} =
  let ichm =
        InitCommitteeHashMint
          { icTxOutRef = genesisUtxo sidechainParams
          }
      uch =
        UpdateCommitteeHash
          { cToken = UpdateCommitteeHash.committeeHashAssetClass ichm
          }
   in UpdateCommitteeHash.findCommitteeHashOutput uch
        >>= \case
          Nothing -> Contract.throwError "error 'saveRoot' no committee hash found."
          Just (choref, cho, chd) ->
            let param = MPTRootTokenMintingPolicy.signedMerkleRootMint sidechainParams
                policy = MPTRootTokenMintingPolicy.mintingPolicy param
                value = Value.singleton (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol param) (Value.TokenName merkleRoot) 1
                redeemer = Redeemer $ toBuiltinData SignedMerkleRoot {merkleRoot, signatures, threshold, committeePubKeys}

                lookups =
                  Constraint.mintingPolicy policy
                    -- TODO: the following line should be removed with reference
                    -- inputs.
                    Prelude.<> Constraint.unspentOutputs (Map.singleton choref cho)
                    -- TODO: the following line should be removed with reference
                    -- inputs.
                    Prelude.<> Constraint.otherScript (UpdateCommitteeHash.updateCommitteeHashValidator uch)

                tx =
                  Constraint.mustMintValueWithRedeemer redeemer value
                    Prelude.<> Constraint.mustPayToOtherScript
                      (MPTRootTokenValidator.hash sidechainParams)
                      Ledger.unitDatum
                      value
                    -- TODO: the following line should be removed with reference
                    -- inputs
                    Prelude.<> Constraint.mustSpendScriptOutput
                      choref
                      ( Redeemer
                          { getRedeemer =
                              Class.toBuiltinData
                                UpdateCommitteeHashRedeemer
                                  { committeeSignatures = []
                                  , committeePubKeys = committeePubKeys
                                  , newCommitteeHash = committeeHash chd
                                  }
                          }
                      )
                    -- TODO: the following line should be removed with reference
                    -- inputs
                    Prelude.<> Constraint.mustPayToOtherScript
                      (Scripts.validatorHash (UpdateCommitteeHash.typedUpdateCommitteeHashValidator uch))
                      (Datum {getDatum = Class.toBuiltinData chd})
                      ( Value.singleton
                          (UpdateCommitteeHash.committeeHashCurSymbol ichm)
                          UpdateCommitteeHash.initCommitteeHashMintTn
                          1
                      )
             in Contract.submitTxConstraintsWith @Void lookups tx
