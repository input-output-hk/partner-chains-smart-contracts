{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Ledger qualified
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (MintingPolicy)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (mkMintingPolicyScript, txInInfoOutRef)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInfo (TxInfo, txInfoInputs, txInfoMint),
 )
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (PassiveBrdgSidechainParams (..))
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: PassiveBrdgSidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  PassiveBrdgSidechainParams {genesisMint}
  mode
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint, txInfoInputs}
    } =
    let hasUTxO utxo = any (\i -> txInInfoOutRef i == utxo) txInfoInputs
        oneshotMintAndUTxOPresent = maybe True hasUTxO genesisMint
        verifyTokenAmount verify = case Value.flattenValue txInfoMint of
          [(sym, name, amount)] ->
            verify amount
              && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
              && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
          _ -> False
        ownTokenName = Value.TokenName "FUEL"
     in case mode of
          MainToSide _ ->
            verifyTokenAmount (traceIfFalse "Can't burn a positive amount" . (< 0))
          SideToMain ->
            verifyTokenAmount (traceIfFalse "Can't mint a negative amount" . (> 0))
              && traceIfFalse "Oneshot Mintingpolicy utxo not present" oneshotMintAndUTxOPresent
mkMintingPolicy _ _ _ = False

mintingPolicy :: PassiveBrdgSidechainParams -> MintingPolicy
mintingPolicy param =
  mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = mkUntypedMintingPolicy

script :: PassiveBrdgSidechainParams -> Scripts.Script
script = Scripts.unMintingPolicyScript . mintingPolicy

scriptSBS :: PassiveBrdgSidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

policyScript :: PassiveBrdgSidechainParams -> PlutusScript PlutusScriptV2
policyScript = PlutusScriptSerialised . scriptSBS

-- ctl hack
-- https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutus-comparison.md#applying-arguments-to-parameterized-scripts
{-# INLINEABLE mkMintingPolicyUntyped #-}
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkMintingPolicy . unsafeFromBuiltinData

serialisableMintingPolicy :: Ledger.Script
serialisableMintingPolicy = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
