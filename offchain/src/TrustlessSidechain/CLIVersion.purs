module TrustlessSidechain.CLIVersion (getVersionString) where

import Prelude

import Contract.Prelude (Aff, liftEffect)
import Data.Either (Either(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (resolve)
import Simple.JSON as JSON

getVersionString ∷ Aff String
getVersionString = do
  packageJsonPath ← liftEffect $ resolve [] filename
  content ← readTextFile UTF8 packageJsonPath
  case JSON.readJSON content of
    Left err → pure $ errString <> show err
    Right (pkg ∷ { version ∷ String }) → pure ("Version: " <> pkg.version)
  where
  filename = "package.json"
  errString = "Error: Unable to parse " <> filename <> ": "
