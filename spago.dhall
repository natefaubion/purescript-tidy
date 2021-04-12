{ name = "purescript-tidy"
, dependencies =
  [ "arrays"
  , "dodo-printer"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "purescript-language-cst-parser"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
