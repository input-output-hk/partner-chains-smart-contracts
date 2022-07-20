{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import Ledger (
  MintingPolicy,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInfo (TxInfo),
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Bytes (getLedgerBytes)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (..))
import TrustlessSidechain.OnChain.Utils (verifyMultisig)

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> SignedMerkleRoot -> ScriptContext -> Bool
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
      -- && any ( \pubKey -> verifySignature (getLedgerBytes $ Ledger.getPubKey pubKey) merkleRoot signature) committeePubKeys
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

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.wrapMintingPolicy
