module Main where -- (main , exampleMain) where
import Data.BigInt as BigInt
import Plutus.Types.Value as Value
import Contract.Prelude
import Contract.Address -- (ownPaymentPubKeyHash)
import Contract.Monad -- (defaultContractConfig, runContract_, launchAff_)
import Contract.Wallet
import Contract.ScriptLookups (ScriptLookups , validator , mkUnbalancedTx) as ScriptLookups
import Contract.TxConstraints
import Contract.Transaction (balanceAndSignTx, submit)
import Contract.TextEnvelope
import Deserialization.PlutusData
import Serialization.Types
import Types.Scripts (Validator , ValidatorHash , MintingPolicy , MintingPolicyHash)
import Effect.Aff (launchAff_)

--exports.fuelMintingPolicy = require("Scripts/FUELMintingPolicy.plutus");
foreign import fuelMintingPolicy :: String

parseFuelMintingPolicy :: Contract () Validator
parseFuelMintingPolicy = wrap <<< wrap <$> textEnvelopeBytes fuelMintingPolicy PlutusScriptV1

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
    , wallet: Just wallet
    , extraConfig: { apiKey: "foo" }
    }
  runContract_ cfg contract1

contract1 :: Contract () Unit
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
