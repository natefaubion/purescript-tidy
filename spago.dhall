{ name = "purescript-tidy"
, dependencies =
  [ "dodo-printer"
  , "purescript-language-cst-parser"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
