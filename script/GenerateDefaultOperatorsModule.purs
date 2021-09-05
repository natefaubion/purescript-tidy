module GenerateDefaultOperatorsModule where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (catchException, throwException)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Unsafe.Coerce (unsafeCoerce)

foreign import tmpdir :: String -> Effect String

main :: Effect Unit
main = do
  cwdPath <- cwd
  tmpPath <- tmpdir "purs-tidy-generate-default-operators-"

  let opts = ChildProcess.defaultExecSyncOptions { cwd = Just tmpPath }
  let genCmd = Path.concat [ cwdPath, "bin", "index.js" ] <> " generate-operators '.spago/*/*/src/**/*.purs'"

  writeTextFile UTF8 (Path.concat [ tmpPath, "spago.dhall" ]) defaultSpagoDhall
  _ <- ChildProcess.execSync "spago install" opts
  output <- Buffer.toString UTF8 =<< catchException
    ( \err -> do
        stdout <- Buffer.toString UTF8 ((unsafeCoerce err).stdout :: Buffer)
        stderr <- Buffer.toString UTF8 ((unsafeCoerce err).stderr :: Buffer)
        Console.log stdout
        Console.error stderr
        throwException err
    )
    (ChildProcess.execSync genCmd opts)

  let
    header =
      [ "--------------------------------------------"
      , "-- This module is generated. DO NOT EDIT! --"
      , "--------------------------------------------"
      , "module Tidy.Operators.Defaults where"
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

  writeTextFile UTF8 (Path.concat [ cwdPath, "src", "PureScript", "CST", "Tidy", "Operators", "Defaults.purs" ]) contents

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
  , packages = https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210811/packages.dhall sha256:a2de7ef2f2e753733eddfa90573a82da0c7c61d46fa87d015b7f15ef8a6e97d5
  , sources = [] : List Text
  }
"""
