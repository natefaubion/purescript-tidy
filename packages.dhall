let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220513/packages.dhall
        sha256:1ed784f37ae6131d99acd542d058d5ce39954ccaacc3adba5cc7cf1549d2bffa

in upstream
  with node-glob-basic =
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
    , version = "v1.2.2"
    }

  with node-workerbees =
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
    , version = "node-esm"
    }

  with language-cst-parser =
    (upstream.language-cst-parser with version = "type-applications")
