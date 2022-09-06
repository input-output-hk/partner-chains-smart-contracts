{ name = "ctl-test"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "codec-argonaut"
  , "control"
  , "exceptions"
  , "monad-logger"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "profunctor"
  , "profunctor-lenses"
  , "strings"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
