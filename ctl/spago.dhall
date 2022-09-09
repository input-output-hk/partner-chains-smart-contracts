{ name = "ctl-test"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
