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
  SaveRootParams (..),
 )
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (..))

saveRoot :: SaveRootParams -> Contract () TrustlessSidechainSchema Text CardanoTx
saveRoot SaveRootParams {sidechainParams, merkleRoot, signature, committeePubKeys} = do
  let policy = MPTRootTokenMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) (Value.TokenName merkleRoot) 1
      redeemer = Redeemer $ toBuiltinData SignedMerkleRoot {merkleRoot, signature, committeePubKeys}
  Contract.submitTxConstraintsWith @SignedMerkleRoot
    (Constraint.mintingPolicy policy)
    ( Constraint.mustMintValueWithRedeemer redeemer value
    -- <> Constraint.mustPayToPubKey recipient value
    )