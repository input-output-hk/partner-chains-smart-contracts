module Test.Config (config) where

import Contract.Prelude

import Contract.Test.Plutip (PlutipConfig)
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
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 8088
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      Just
        { port: UInt.fromInt 8081
        , host: "127.0.0.1"
        , secure: false
        , path: Nothing
        }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5432
      , user: "postgres"
      , password: "password"
      , dbname: "ctl"
      }
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: mempty
  }
