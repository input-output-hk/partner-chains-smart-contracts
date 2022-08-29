{ name = "ctl-test"
, dependencies =
  [ "aeson"
  , "aeson-helpers"
  , "argonaut-core"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "codec-argonaut"
  , "control"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "optparse"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "strings"
  , "transformers"
  , "uint"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
