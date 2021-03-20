{ name = "purescript-tidy-cli"
, dependencies =
  [ "aff"
  , "console"
  , "dodo-printer"
  , "effect"
  , "node-fs-aff"
  , "node-process"
  , "psci-support"
  , "purescript-language-cst-parser"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "bin/**/*.purs" ]
}
