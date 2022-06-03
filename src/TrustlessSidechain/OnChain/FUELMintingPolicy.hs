{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (MintingPolicy)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInfo (TxInfo, txInfoMint),
 )
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  _
  mode
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    case mode of
      MainToSide _ ->
        verifyTokenAmount $ traceIfFalse "Can't burn a positive amount" . (< 0)
      SideToMain ->
        verifyTokenAmount $ traceIfFalse "Can't mint a negative amount" . (> 0)
    where
      verifyTokenAmount verify =
        case Value.flattenValue txInfoMint of
          [(sym, name, amount)] ->
            verify amount
              && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
              && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
          _ -> False
      ownTokenName = Value.TokenName "FUEL"
mkMintingPolicy _ _ _ = False

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Scripts.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = mkUntypedMintingPolicy

script :: SidechainParams -> Scripts.Script
script = Scripts.unMintingPolicyScript . mintingPolicy

scriptSBS :: SidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

policyScript :: SidechainParams -> PlutusScript PlutusScriptV2
policyScript = PlutusScriptSerialised . scriptSBS
