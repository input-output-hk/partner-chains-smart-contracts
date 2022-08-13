{ name = "ctl-package-example"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "ordered-collections"
  , "parallel"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
