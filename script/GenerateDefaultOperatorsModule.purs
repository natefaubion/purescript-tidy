module GenerateDefaultOperatorsModule where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path as Path
import Node.Process (cwd)

foreign import tmpdir :: String -> Effect String

main :: Effect Unit
main = do
  cwdPath <- cwd
  tmpPath <- tmpdir "purs-tidy-generate-default-operators-"

  let opts = ChildProcess.defaultExecSyncOptions { cwd = Just tmpPath }
  let genCmd = "node -e \"require('" <> cwdPath <> "/output/Main/index.js').main()\" generate-operators '.spago/*/*/src/**/*.purs'"

  writeTextFile UTF8 (Path.concat [ tmpPath, "spago.dhall" ]) defaultSpagoDhall
  _ <- ChildProcess.execSync "spago install" opts
  output <- Buffer.toString UTF8 =<< ChildProcess.execSync genCmd opts

  let
    header =
      [ "--------------------------------------------"
      , "-- This module is generated. DO NOT EDIT! --"
      , "--------------------------------------------"
      , "module DefaultOperators where"
      , ""
      , "defaultOperators :: Array String"
      , "defaultOperators ="
      ]

    lines = output # String.trim # String.split (Pattern "\n") # mapWithIndex \ix line ->
      if ix == 0 then
        "  [ \"\"\"" <> line <> "\"\"\""
      else
        "  , \"\"\"" <> line <> "\"\"\""

    footer =
      [ "  ]"
      , ""
      ]

    contents =
      Array.intercalate "\n" (header <> lines <> footer)

  writeTextFile UTF8 (Path.concat [ cwdPath, "bin", "DefaultOperators.purs" ]) contents

defaultSpagoDhall :: String
defaultSpagoDhall =
  """
  { name = "purs-tidy-generate-default-operators"
  , dependencies =
    [ "ace"
    , "aff"
    , "aff-bus"
    , "aff-coroutines"
    , "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "argonaut-core"
    , "argonaut-generic"
    , "argonaut-traversals"
    , "arraybuffer-types"
    , "arrays"
    , "assert"
    , "avar"
    , "bifunctors"
    , "catenable-lists"
    , "concurrent-queues"
    , "console"
    , "const"
    , "contravariant"
    , "control"
    , "coroutines"
    , "datetime"
    , "distributive"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "exists"
    , "filterable"
    , "fixed-points"
    , "foldable-traversable"
    , "foreign"
    , "foreign-object"
    , "fork"
    , "form-urlencoded"
    , "formatters"
    , "free"
    , "freet"
    , "functions"
    , "functors"
    , "gen"
    , "github-actions-toolkit"
    , "graphs"
    , "http-methods"
    , "identity"
    , "integers"
    , "invariant"
    , "js-date"
    , "js-timers"
    , "js-uri"
    , "lazy"
    , "lcg"
    , "lists"
    , "machines"
    , "math"
    , "matryoshka"
    , "maybe"
    , "media-types"
    , "minibench"
    , "newtype"
    , "nonempty"
    , "now"
    , "nullable"
    , "numbers"
    , "options"
    , "ordered-collections"
    , "orders"
    , "parallel"
    , "parsing"
    , "partial"
    , "pathy"
    , "precise"
    , "prelude"
    , "profunctor"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "quickcheck-laws"
    , "random"
    , "react"
    , "react-dom"
    , "record"
    , "refs"
    , "routing"
    , "safe-coerce"
    , "semirings"
    , "st"
    , "string-parsers"
    , "strings"
    , "strings-extra"
    , "tailrec"
    , "these"
    , "transformers"
    , "tuples"
    , "type-equality"
    , "typelevel-prelude"
    , "unfoldable"
    , "unicode"
    , "unsafe-coerce"
    , "unsafe-reference"
    , "uri"
    , "validation"
    ]
  , packages = https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca
  , sources = [] : List Text
  }
"""
