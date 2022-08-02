module Test.Main (main) where

import Contract.Prelude

import Contract.Config
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Data.UInt (fromInt)
import Main (contract)

config ∷ PlutipConfig
config =
  { host: "localhost"
  , port: fromInt 9080
  , logLevel: Debug
  , ogmiosConfig: defaultOgmiosWsConfig
  , ogmiosDatumCacheConfig: defaultDatumCacheWsConfig
  , ctlServerConfig: defaultServerConfig
  , postgresConfig: -- `defaultConfig.postgres` in `flake.nix`
      { dbname: "ctxlib"
      , host: "localhost"
      , port: fromInt 5432
      , user: "ctxlib"
      , password: "ctxlib"
      }
  }

main ∷ Effect Unit
main = launchAff_ $ runPlutipContract config unit \_ -> do
  logInfo' "Hello World"
  contract
