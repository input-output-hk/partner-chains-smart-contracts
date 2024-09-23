module TrustlessSidechain.CLIVersion (versionString) where

import Prelude

semVer :: String
semVer = "__semVer" -- replaced by esbuild-plugin-replace to correct version

versionString :: String
versionString = "Version: " <> semVer
