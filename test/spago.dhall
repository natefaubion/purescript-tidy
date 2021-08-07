{ name = "purescript-tidy-test"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argparse-basic"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "dodo-printer"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "purescript-language-cst-parser"
  , "node-glob-basic"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/*.purs", "bin/Bin/*.purs" ]
}
