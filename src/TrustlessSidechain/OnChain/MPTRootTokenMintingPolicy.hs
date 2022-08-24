{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (MintingPolicy)
import Plutus.Script.Utils.V2.Scripts qualified as Script
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (getLedgerBytes)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (..),
  ScriptPurpose (Minting),
  TxInfo (..),
 )
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (PassiveBrdgSidechainParams)
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (..))
import TrustlessSidechain.OnChain.Utils (verifyMultisig)

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: PassiveBrdgSidechainParams -> SignedMerkleRoot -> ScriptContext -> Bool
mkMintingPolicy
  _
  SignedMerkleRoot
    { merkleRoot
    , signatures
    , committeePubKeys
    , threshold
    }
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    verifyTokenAmount (traceIfFalse "Amount must be 1" . (== 1))
      && verifyMultisig (map (getLedgerBytes . Ledger.getPubKey) committeePubKeys) threshold merkleRoot signatures
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

mintingPolicy :: PassiveBrdgSidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.mkUntypedMintingPolicy

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkMintingPolicy . unsafeFromBuiltinData

serialisableMintingPolicy :: Ledger.Script
serialisableMintingPolicy = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
