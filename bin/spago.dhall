{ name = "purescript-tidy-cli"
, dependencies =
  [ "aff"
  , "argparse-basic"
  , "console"
  , "dodo-printer"
  , "effect"
  , "node-fs-aff"
  , "node-glob-basic"
  , "node-process"
  , "psci-support"
  , "purescript-language-cst-parser"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "bin/**/*.purs" ]
}
