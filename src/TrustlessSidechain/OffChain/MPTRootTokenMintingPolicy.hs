{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.MPTRootTokenMintingPolicy where

import Data.Text (Text)
import Data.Void (Void)
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
import TrustlessSidechain.OnChain.MPTRootTokenValidator (Mpt (Mpt, mptCurrencySymbol, mptSidechainParams))
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (SignedMerkleRoot, committeePubKeys, merkleRoot, signatures, threshold))
import Prelude qualified

saveRoot :: SaveRootParams -> Contract () TrustlessSidechainSchema Text CardanoTx
saveRoot SaveRootParams {sidechainParams, merkleRoot, threshold, signatures, committeePubKeys} = do
  let mptCurSym = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams
      policy = MPTRootTokenMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton mptCurSym (Value.TokenName merkleRoot) 1
      redeemer = Redeemer $ toBuiltinData SignedMerkleRoot {merkleRoot, signatures, threshold, committeePubKeys}
      lookups = Constraint.mintingPolicy policy

      tx =
        Constraint.mustMintValueWithRedeemer redeemer value
          Prelude.<> Constraint.mustPayToOtherScript
            ( MPTRootTokenValidator.hash
                Mpt {mptSidechainParams = sidechainParams, mptCurrencySymbol = mptCurSym}
            )
            Ledger.unitDatum
            value

  Contract.submitTxConstraintsWith @Void lookups tx
