module Main (main) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetConfig, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftContractAffM, runContract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript(..), mintingPolicyHash)
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Value (scriptCurrencySymbol)

foreign import fuelMintingPolicy ∷ String
foreign import inBrowser ∷ Boolean

asMintingPolicy ∷ ByteArray -> MintingPolicy
asMintingPolicy = PlutusScript >>> MintingPolicy

config ∷ ConfigParams ()
config = if inBrowser then testnetNamiConfig else testnetConfig

main ∷ Effect Unit
main = launchAff_ $ runContract config do
  pol <- asMintingPolicy <$> textEnvelopeBytes fuelMintingPolicy PlutusScriptV1
  sym <- liftContractAffM "Currency Symbol Error" $ scriptCurrencySymbol pol
  mph <- liftContractAffM "Couldn't Hash Script" $ mintingPolicyHash pol
  logInfo' $ "Policy: " <> show pol
  logInfo' $ "Policy Symbol: " <> show sym
  logInfo' $ "Policy Hash: " <> show mph
