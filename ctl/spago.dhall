{ name = "ctl-test"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "codec-argonaut"
  , "control"
  , "exceptions"
  , "node-buffer"
  , "node-fs"
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
