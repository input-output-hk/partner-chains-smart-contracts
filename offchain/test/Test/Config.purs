module Test.Config (config) where

import Contract.Prelude

import Contract.Test.Plutip (PlutipConfig)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt

config ∷ PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 1337
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
      { slotLength: Seconds 0.05 }
  }
