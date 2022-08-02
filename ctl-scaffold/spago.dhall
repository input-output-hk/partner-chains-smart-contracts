{ name = "ctl-package-example"
, dependencies = [ "cardano-transaction-lib", "uint" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
