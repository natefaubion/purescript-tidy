{ name = "purescript-tidy-test"
, dependencies =
  [ "aff"
  , "avar"
  , "ansi"
  , "console"
  , "debug"
  , "dodo-printer"
  , "effect"
  , "node-fs-aff"
  , "node-process"
  , "node-child-process"
  , "purescript-language-cst-parser"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/*.purs" ]
}
