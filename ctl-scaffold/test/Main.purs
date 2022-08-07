module Test.Main (main) where
import SidechainParams (SidechainParams(..))
import RunFuelMintingPolicy (FuelParams(..) , runFuelMP)

import Contract.Prelude (Effect, LogLevel(..), Maybe(..), Unit, bind, ($), (/\))
import Contract.Monad (Contract, launchAff_, liftedM)
import Contract.Wallet (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Data.UInt (fromInt)
import Data.BigInt as BigInt
--import Contract.Test.E2E (publishTestFeedback)
import Contract.Address (ownPaymentPubKeyHash)

testMint ∷ Contract () Unit
testMint = let sp = SidechainParams { chainId: "" , genesisHash: "" , genesisMint: Nothing }
  in do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
--pkStake ← liftedM "cannot get stakepubkeyhash" ownStakePubKeyHash -- plutip limitations
  runFuelMP (Mint { amount: 1 , recipient: pk }) sp

config ∷ PlutipConfig
config =
  { host: "127.0.0.1"
  , port: fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = launchAff_ $ do
  let distribute = [ BigInt.fromInt 1_000_000_000 , BigInt.fromInt 2_000_000_000 ] /\ [ BigInt.fromInt 2_000_000_000 ]
-- https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md
-- * Plutip does not currently provide staking keys. However, arbitrary staking keys can be used if the application does not depend on staking (because payment keys and stake keys don't have to be connected in any way). It's also possible to omit staking keys in many cases by using mustPayToPubKey instead of mustPayToPubKeyAddress.
  runPlutipContract config distribute \(alice /\ _bob) → do
    withKeyWallet alice
      testMint

--cfg ← defaultTestnetContractConfig
--runContract_ cfg go

--publishTestFeedback true
