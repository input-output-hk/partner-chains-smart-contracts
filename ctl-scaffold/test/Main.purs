module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Data.UInt (fromInt)
import Main (contract)

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

main ∷ Effect Unit
main = launchAff_ $ runPlutipContract config unit \_ -> contract
