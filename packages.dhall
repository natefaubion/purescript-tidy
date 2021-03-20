let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let overrides = {=}

let additions =
  { purescript-language-cst-parser =
      { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
      , repo =
          "https://github.com/natefaubion/purescript-language-cst-parser.git"
      , version =
          "v0.5.1"
      }
  , dodo-printer =
      { dependencies =
          [ "ansi"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "strings"
          ]
      , repo =
          "https://github.com/natefaubion/purescript-dodo-printer.git"
      , version =
          "v1.1.0"
      }
  }

in  upstream // overrides // additions
