{ name = "ctl-package-example"
, dependencies = [ "bigints", "cardano-transaction-lib", "uint" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
