module GenerateDefaultOperatorsModule where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode as Data.Argonaut.Decode
import Data.Array (foldMap, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.String (Pattern(..))
import Data.String as Str
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as Exec
import Node.ChildProcess.Types as Node.ChildProcess.Types
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readdir, stat, writeTextFile)
import Node.FS.Stats as FS
import Node.Library.Execa as Node.Library.Execa
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Node.Process

foreign import tmpdir :: String -> Effect String

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  cwdPath <- liftEffect Node.Process.cwd
  tmpPath <- liftEffect $ tmpdir "purs-tidy-generate-default-operators-"
  liftEffect $ Console.log $ "Working in " <> tmpPath

  writeTextFile UTF8 (Path.concat [ tmpPath, "spago.yaml" ]) defaultSpagoYaml
  writeTextFile UTF8 (Path.concat [ tmpPath, "package.json" ]) defaultPackageJson
  liftEffect $ Console.log $ "running 'npm install'"
  _ <- liftEffect $ ChildProcess.execSync' "npm install" (_ { cwd = Just tmpPath })

  liftEffect $ Console.log $ "running 'spago ls packages --json'"
  s <- liftEffect $ Buffer.toString UTF8 =<< Exec.execSync' "spago ls packages --json" (_ { cwd = Just tmpPath })

  packages <- case Data.Argonaut.Decode.decodeJson =<< Data.Argonaut.Decode.parseJson s of
    Left err -> throwError $ error $ Data.Argonaut.Decode.printJsonDecodeError err
    Right (object :: Object Json) -> pure $ Foreign.Object.keys object

  liftEffect $ Console.log $ "running 'spago install " <> Str.joinWith " " packages <> "'"
  _ <- liftEffect $ Exec.execSync' ("spago install " <> Str.joinWith " " packages) (_ { cwd = Just tmpPath })

  pursFiles <- getPursFiles tmpPath

  let genCmdFile = Path.concat [ cwdPath, "cli", "index.js" ]
  let genCmdArgs = [ "generate-operators" ] <> pursFiles

  liftEffect $ Console.log $ "running " <> Str.joinWith " " ([ genCmdFile, "..."]) <> "\n"
  -- liftEffect $ Console.log $ "running " <> Str.joinWith " " ([ genCmdFile ] <> genCmdArgs) <> "\n"

  genCmdResult <- liftEffect $ Node.Library.Execa.execaSync genCmdFile genCmdArgs
    ( _
        { cwd = Just tmpPath
        , maxBuffer = Just infinity -- The Nothing means 100MB, will result in empty stdout (but Nothing should mean Infinity https://github.com/JordanMartinez/purescript-node-execa/issues/20)
        }
    )
  output <-
    case genCmdResult.exit of
      Node.ChildProcess.Types.Normally 0 -> pure genCmdResult.stdout
      _ -> liftEffect do
        Console.error genCmdResult.message
        Node.Process.exit' 1

  -- liftEffect $ Console.log "output:"
  -- liftEffect $ Console.log output

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

getPursFiles :: FilePath -> Aff (Array FilePath)
getPursFiles tmpPath = do
  s <- liftEffect $ Buffer.toString UTF8 =<< Exec.execSync' ("spago sources --json --quiet") (_ { cwd = Just tmpPath })
  case Data.Argonaut.Decode.decodeJson =<< Data.Argonaut.Decode.parseJson s of
    Left err -> throwError $ error $ Data.Argonaut.Decode.printJsonDecodeError err
    Right globs -> pure globs

defaultPackageJson :: String
defaultPackageJson =
  """
  {
    "private": true,
    "type": "module",
    "dependencies": {
      "purescript": "latest",
      "spago": "next"
    }
  }
  """

defaultSpagoYaml :: String
defaultSpagoYaml = Array.intercalate "\n"
  [ "package:"
  , "  name: test-parser"
  , "  dependencies: []"
  , "workspace:"
  , "  package_set:"
  , "    registry: 60.5.1"
  , "  extra_packages: {}"
  ]
