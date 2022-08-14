{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.MPTRootTokenMintingPolicy where

import Data.Text (Text)
import Ledger (CardanoTx, Redeemer (Redeemer))
import Ledger qualified
import Ledger.Constraints qualified as Constraint
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  SaveRootParams (SaveRootParams, committeePubKeys, merkleRoot, sidechainParams, signatures, threshold),
 )
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator (MPT)
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (SignedMerkleRoot, committeePubKeys, merkleRoot, signatures, threshold))
import Prelude qualified

saveRoot :: SaveRootParams -> Contract () TrustlessSidechainSchema Text CardanoTx
saveRoot SaveRootParams {sidechainParams, merkleRoot, threshold, signatures, committeePubKeys} = do
  let policy = MPTRootTokenMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams) (Value.TokenName merkleRoot) 1
      redeemer = Redeemer $ toBuiltinData SignedMerkleRoot {merkleRoot, signatures, threshold, committeePubKeys}
      lookups = Constraint.mintingPolicy policy

      tx =
        Constraint.mustMintValueWithRedeemer redeemer value
          Prelude.<> Constraint.mustPayToOtherScript
            (MPTRootTokenValidator.hash sidechainParams)
            Ledger.unitDatum
            value

  Contract.submitTxConstraintsWith @MPT lookups tx
