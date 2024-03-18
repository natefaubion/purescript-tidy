module GenerateDefaultOperatorsModule where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode as Data.Argonaut.Decode
import Data.Array (foldMap, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
-- import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (catchException, throwException)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Node.Buffer (Buffer)
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
import Sunde as Sunde
import Unsafe.Coerce (unsafeCoerce)

foreign import tmpdir :: String -> Effect String

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  cwdPath <- liftEffect Node.Process.cwd
  tmpPath <- liftEffect $ tmpdir "purs-tidy-generate-default-operators-"
  liftEffect $ Console.log $ "Working in " <> tmpPath

  writeTextFile UTF8 (Path.concat [ tmpPath, "spago.yaml" ]) defaultSpagoYaml
  writeTextFile UTF8 (Path.concat [ tmpPath, "package.json" ]) defaultPackageJson
  _ <- liftEffect $ ChildProcess.execSync' "npm install" (_ { cwd = Just tmpPath })

  s <- liftEffect $ Buffer.toString UTF8 =<< Exec.execSync' "spago ls packages --json" (_ { cwd = Just tmpPath })

  packages <- case Data.Argonaut.Decode.decodeJson =<< Data.Argonaut.Decode.parseJson s of
    Left err -> throwError $ error $ Data.Argonaut.Decode.printJsonDecodeError err
    Right (object :: Object Json) -> pure $ Foreign.Object.keys object

  _ <- liftEffect $ Exec.execSync' ("spago install " <> Str.joinWith " " packages) (_ { cwd = Just tmpPath })
  pursFiles <- getPursFiles 0 (tmpPath <> "/.spago")

  let genCmdFile = Path.concat [ cwdPath, "bin", "index.js" ]
  let genCmdArgs = [ "generate-operators" ] <> pursFiles

  liftEffect $ Console.log $ "Using command " <> Array.intercalate " " ([ genCmdFile ] <> genCmdArgs) <> "\n"

  -- | genCmdResult <- Node.Library.Execa.execa genCmdFile genCmdArgs (\x -> Debug.spy "options" (x { cwd = Just tmpPath, stripFinalNewline = Just true, maxBuffer = Nothing })) >>= \spawned -> spawned.getResult
  -- genCmdResult <- liftEffect $ Node.Library.Execa.execaSync genCmdFile genCmdArgs (\x -> Debug.spy "options" (x { cwd = Just tmpPath, stripFinalNewline = Just true, maxBuffer = Just 99999999999.0 }))
  -- | Console.log (unsafeCoerce genCmdResult)
  -- | output <-
  -- |   case genCmdResult.exit of
  -- |     Node.ChildProcess.Types.Normally 0 -> pure genCmdResult.stdout
  -- |     -- Node.ChildProcess.Types.Normally 0 -> pure genCmdResult.message
  -- |     _ -> liftEffect do
  -- |       Console.error genCmdResult.message
  -- |       Node.Process.exit' 1

  genCmdResult <- Sunde.spawn { cmd: genCmdFile, args: genCmdArgs, stdin: Nothing } identity
  -- | Console.log (unsafeCoerce genCmdResult)
  output <-
    case genCmdResult.exit of
      Node.ChildProcess.Types.Normally 0 -> pure genCmdResult.stdout
      _ -> liftEffect do
        Console.error genCmdResult.stdout
        Console.error genCmdResult.stderr
        Node.Process.exit' 1

  -- file:///home/srghma/projects/purescript-tidy/output/Effect.Aff/foreign.js:530
  -- throw util.fromLeft(step);
  -- ^
  -- TypeError: Cannot read properties of null (reading 'toString')
  -- at Module.toStringImpl (file:///home/srghma/projects/purescript-tidy/output/Node.Buffer.Immutable/foreign.js:37:49)
  -- at file:///home/srghma/projects/purescript-tidy/output/Node.Buffer.Immutable/index.js:25:25
  -- at file:///home/srghma/projects/purescript-tidy/output/Control.Monad/index.js:70:33
  -- at file:///home/srghma/projects/purescript-tidy/output/Effect/foreign.js:10:14
  -- at file:///home/srghma/projects/purescript-tidy/output/Effect/foreign.js:10:20
  -- at __do (file:///home/srghma/projects/purescript-tidy/output/GenerateDefaultOperatorsModule/index.js:104:124)
  -- at file:///home/srghma/projects/purescript-tidy/output/Effect.Exception/foreign.js:38:22
  -- at file:///home/srghma/projects/purescript-tidy/output/Effect/foreign.js:10:16
  -- at runSync (file:///home/srghma/projects/purescript-tidy/output/Effect.Aff/foreign.js:88:20)
  -- at run (file:///home/srghma/projects/purescript-tidy/output/Effect.Aff/foreign.js:326:22)
  --
  -- catchException
  -- ( \err -> do
  --     Console.log "Caught exception:"
  --     stdout <- Buffer.toString UTF8 ((unsafeCoerce err).stdout :: Buffer)
  --     stderr <- Buffer.toString UTF8 ((unsafeCoerce err).stderr :: Buffer)
  --     Console.log stdout
  --     Console.error stderr
  --     throwException err
  -- )
  -- (ChildProcess.execSync' genCmd optsModifier)

  liftEffect $ Console.log "output:"
  liftEffect $ Console.log output

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

-- copied from purescript-language-cst-parser/parse-package-set/src/Main.purs
getPursFiles :: Int -> FilePath -> Aff (Array FilePath)
getPursFiles depth root = do
  readdir root >>= foldMap \file -> do
    let path = root <> "/" <> file
    stats <- stat path
    if FS.isDirectory stats then
      if depth == 2 && file /= "src" then do
        pure []
      else
        getPursFiles (depth + 1) path
    else if Regex.test pursRegex path then
      pure [ path ]
    else pure []
  where
  pursRegex = unsafeRegex "\\.purs$" noFlags

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
  , "    registry: 50.4.0"
  , "  extra_packages: {}"
  ]
