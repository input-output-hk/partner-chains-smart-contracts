{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (MintingPolicy)
import Plutus.Script.Utils.V2.Scripts qualified as Script
import Plutus.V2.Ledger.Api (getLedgerBytes)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (..),
  ScriptPurpose (Minting),
  TxInfo (..),
 )
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (..))

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> SignedMerkleRoot -> ScriptContext -> Bool
mkMintingPolicy
  _
  SignedMerkleRoot
    { merkleRoot
    , signature
    , committeePubKeys
    }
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    verifyTokenAmount (traceIfFalse "Amount must be 1" . (== 1))
      && any
        ( \pubKey ->
            verifyEd25519Signature (getLedgerBytes $ Ledger.getPubKey pubKey) merkleRoot signature
        )
        committeePubKeys
    where
      verifyTokenAmount verify =
        case Value.flattenValue txInfoMint of
          [(sym, name, amount)] ->
            verify amount
              && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
              && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
          _ -> False
      ownTokenName = Value.TokenName merkleRoot
mkMintingPolicy _ _ _ = False

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.mkUntypedMintingPolicy
