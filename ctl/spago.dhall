{ name = "ctl-package-example"
, dependencies =
  [ "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "exceptions"
  , "monad-logger"
  , "node-path"
  , "ordered-collections"
  , "parallel"
  , "profunctor-lenses"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
