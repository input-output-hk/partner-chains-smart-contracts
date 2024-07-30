module Test.Config (config) where

import Contract.Prelude

import Ctl.Internal.Testnet.Types (Era(Babbage), TestnetConfig)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt

config ∷ TestnetConfig
config =
  { logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 8081
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: mempty
  , clusterConfig:
      { slotLength: Seconds 0.05
      , epochSize: Nothing
      , testnetMagic: 2
      , era: Babbage
      }
  }
