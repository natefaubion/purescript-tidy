let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210324/packages.dhall sha256:b4564d575da6aed1c042ca7936da97c8b7a29473b63f4515f09bb95fae8dddab

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
        , version = "v0.7.1"
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
        , version = "v1.1.0"
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
        , version = "v0.1.1"
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
