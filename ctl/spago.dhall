{ name = "ctl-package-example"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "profunctor-lenses"
  , "strings"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
