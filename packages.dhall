let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210713/packages.dhall sha256:654c3148cb995f642c73b4508d987d9896e2ad3ea1d325a1e826c034c0d3cd7b

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
        , version = "v0.9.0"
        }
      , dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.0.0"
        }
      , node-glob-basic =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "lists"
          , "maybe"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
        , version = "v1.2.0"
        }
      , node-workerbees =
        { dependencies =
          [ "aff"
          , "argonaut-core"
          , "arraybuffer-types"
          , "avar"
          , "effect"
          , "either"
          , "exceptions"
          , "maybe"
          , "newtype"
          , "parallel"
          , "variant"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-workerbees.git"
        , version = "v0.1.2"
        }
      , argparse-basic =
        { dependencies =
          [ "either"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "record"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-argparse-basic.git"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
