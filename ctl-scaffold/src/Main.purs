module Main (main , exampleMain) where
import Data.BigInt as BigInt
import Plutus.Types.Value as Value
import Contract.Prelude
import Contract.Address -- (ownPaymentPubKeyHash)
import Contract.Monad -- (defaultContractConfig, runContract_, launchAff_)
import Contract.Wallet
import Contract.ScriptLookups (ScriptLookups , validator , mkUnbalancedTx) as ScriptLookups
import Contract.TxConstraints
import Contract.Transaction (balanceAndSignTx, submit)
import Deserialization.PlutusData
import Serialization.Types
import Types.Scripts (Validator , ValidatorHash , MintingPolicy , MintingPolicyHash)
import Effect.Aff (launchAff_)

-- load a plutus script
--foreign import myscript :: String
--parseValidator :: Contract () Validator
--parseValidator = wrap <<< wrap <$> Contract.TextEnvelope.textEnvelopeBytes myscript PlutusScriptV1

exampleMain :: Effect Unit
exampleMain = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg $ log <<< show =<< ownPaymentPubKeyHash

main :: Effect Unit
main = launchAff_ $ do
  wallet <- mkNamiWalletAff
  cfg <- mkContractConfig $ ConfigParams
    -- The server defaults below are also exported from `Contract.Monad`
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Trace
    , wallet
--  , extraConfig: { apiKey: "foo" }
    }
  runContract_ cfg contract1

contract1 = let
--constraints :: TxConstraints Unit Unit
--constraints = mustPayToScript vhash unitDatum (Value.lovelaceValueOf (BigInt.fromInt 2_000_000))
  constraints = mempty

--lookups :: ScriptLookups.ScriptLookups PlutusData
--lookups = ScriptLookups.validator validator
  lookups = mempty
  in do
  ubTx <- liftedE (ScriptLookups.mkUnbalancedTx lookups constraints)
  bsTx <- liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId <- submit bsTx
  logInfo' ("Tx ID: " <> show txId)

{-
customOgmiosWsConfig :: ServerConfig
customOgmiosWsConfig =
  { port: UInt.fromInt 80
  , host: "localhost"
  , secure: false
  , path: Just "/api/ogmios"
  }
-}
