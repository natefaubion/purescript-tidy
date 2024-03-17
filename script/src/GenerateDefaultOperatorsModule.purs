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
  Console.log $ "Working in " <> tmpPath

  let optsModifier x = x { cwd = Just tmpPath }
  let genCmd = Path.concat [ cwdPath, "bin", "index.js" ] <> " generate-operators '.spago/*/*/src/**/*.purs'"

  writeTextFile UTF8 (Path.concat [ tmpPath, "spago.yaml" ]) defaultSpagoYaml
  writeTextFile UTF8 (Path.concat [ tmpPath, "package.json" ]) defaultPackageJson
  _ <- ChildProcess.execSync' "npm install" optsModifier
  Console.log $ "Using command " <> genCmd
  output <- Buffer.toString UTF8 =<< catchException
    ( \err -> do
        stdout <- Buffer.toString UTF8 ((unsafeCoerce err).stdout :: Buffer)
        stderr <- Buffer.toString UTF8 ((unsafeCoerce err).stderr :: Buffer)
        Console.log "Caught exception:"
        Console.log stdout
        Console.error stderr
        throwException err
    )
    (ChildProcess.execSync' genCmd optsModifier)

  Console.log "output:"
  Console.log output

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

  writeTextFile UTF8 (Path.concat [ cwdPath, "lib", "src", "Tidy", "Operators", "Defaults.purs" ]) contents

defaultPackageJson :: String
defaultPackageJson =
  """
  {
    "private": true,
    "type": "module",
    "dependencies": {
      "purescript": "latest",
      "spago": "next"
    },
    "scripts": {
      "postinstall": "spago install"
    }
  }
  """

-- TODO: can use `spago package ls` with `dependencies: []`
defaultSpagoYaml :: String
defaultSpagoYaml =
  """
package:
  name: purs-tidy-generate-default-operators
  dependencies:
    - ace
    - aff
    - aff-bus
    - aff-coroutines
    - affjax
    - argonaut
    - argonaut-codecs
    - argonaut-core
    - argonaut-generic
    - argonaut-traversals
    - arraybuffer-types
    - arrays
    - assert
    - avar
    - bifunctors
    - catenable-lists
    - concurrent-queues
    - console
    - const
    - contravariant
    - control
    - coroutines
    - datetime
    - distributive
    - effect
    - either
    - enums
    - exceptions
    - exists
    - filterable
    - fixed-points
    - foldable-traversable
    - foreign
    - foreign-object
    - fork
    - form-urlencoded
    - formatters
    - free
    - freet
    - functions
    - functors
    - gen
    # - github-actions-toolkit # TODO: fix `The following packages do not exist in your package set`
    - graphs
    - http-methods
    - identity
    - integers
    - invariant
    - js-date
    - js-timers
    - js-uri
    - lazy
    - lcg
    - lists
    - machines
    - matryoshka
    - maybe
    - media-types
    - minibench
    - newtype
    - nonempty
    - now
    - nullable
    - numbers
    - options
    - ordered-collections
    - orders
    - parallel
    - parsing
    - partial
    - pathy
    - precise
    - prelude
    - profunctor
    - profunctor-lenses
    - psci-support
    - quickcheck
    - quickcheck-laws
    - random
    - react
    - react-dom
    - record
    - refs
    - routing
    - safe-coerce
    - semirings
    - st
    - string-parsers
    - strings
    - strings-extra
    - tailrec
    - these
    - transformers
    - tuples
    - type-equality
    - typelevel-prelude
    - unfoldable
    - unicode
    - unsafe-coerce
    - unsafe-reference
    - uri
    - validation
workspace:
  package_set:
    registry: 50.4.0
"""
