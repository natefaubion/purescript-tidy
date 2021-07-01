module Test.Snapshot where

import Prelude

import Control.MonadZero (guard)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.Set as Set
import Data.String (Pattern(..), stripSuffix)
import Data.String as String
import Data.String.Regex as Regex
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Dodo (PrintOptions)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError, effectCanceler, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Buffer (Buffer, freeze)
import Node.Buffer as Buffer
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, defaultExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readdir, writeFile)
import Node.Path (FilePath, basename)
import Node.Path as Path
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Tidy (class FormatError, FormatOptions)
import PureScript.CST.Tidy as Tidy
import PureScript.CST.Types (Module)
import Test.FormatDirective (FormatDirective, defaultFormat, directiveRegex, formatDirective, parseDirectivesFromModule)
import Text.Parsing.StringParser (Parser, runParser)

data SnapshotResult
  = Passed
  | Saved
  | Accepted
  | Failed String
  | ErrorRunningTest Error

type SnapshotTest =
  { name :: String
  , results :: Array { output :: String, result :: SnapshotResult }
  }

isBad :: SnapshotResult -> Boolean
isBad = case _ of
  Failed _ -> true
  _ -> false

snapshotFormat :: String -> Boolean -> Maybe Pattern -> Aff (Array SnapshotTest)
snapshotFormat directory accept mbPattern = do
  paths <- mapMaybe (filterPath <=< stripSuffix (Pattern ".purs") <<< basename) <$> readdir directory
  for paths runSnapshot
  where
  filterPath = case mbPattern of
    Just pat ->
      \path ->
        guard (String.contains pat path) $> path
    Nothing ->
      pure

  makeErrorResult :: String -> Error -> Aff SnapshotTest
  makeErrorResult name err = pure { name, results: [ { output: "", result: ErrorRunningTest err } ] }

  runSnapshot :: String -> Aff SnapshotTest
  runSnapshot name = flip catchError (makeErrorResult name) do
    let filePath = Path.concat [ directory, name <> ".purs" ]
    let outputPath = Path.concat [ directory, name <> ".output" ]
    contents <- liftEffect <<< bufferToUTF8 =<< readFile filePath
    case parseModule contents of
      ParseSucceeded mod ->
        runSnapshotForModule name outputPath mod
      ParseSucceededWithErrors mod _ ->
        runSnapshotForModule name outputPath mod
      ParseFailed err -> do
        let
          formattedError =
            printParseError err.error
              <> " at "
              <> show (err.position.line + 1)
              <> ":"
              <> show (err.position.column + 1)
        pure { name, results: [ { output: "", result: Failed formattedError } ] }

  runSnapshotForModule :: forall e. FormatError e => String -> FilePath -> Module e -> Aff SnapshotTest
  runSnapshotForModule name outputPath mod = do
    let
      inputModule = parseDirectivesFromModule mod

      formatModuleWith { printOptions, formatOptions } =
        formatModule printOptions formatOptions inputModule.module

      defaultFormattedModule =
        formatModuleWith defaultFormat

      snapshotOutputs = spy "snapshotOutputs"
        [ defaultFormattedModule
        , foldMap formatModuleWith inputModule.directives
        ]

      snapshotOutputFileContents = spy "snapshotOutputFileContents" $ Array.intercalate "\n"
        [ defaultFormattedModule
        , inputModule.directives # foldMapWithIndex \directiveSource directive ->
            directiveSource
              <> "\n"
              <> formatModuleWith directive
        ]

      acceptOutput =
        writeFile outputPath
          =<< liftEffect (Buffer.fromString snapshotOutputFileContents UTF8)

    savedOutputFile <- try $ readFile outputPath
    case savedOutputFile of
      Left _ -> do
        acceptOutput
        pure
          { name
          , results: map (\output -> { result: Saved, output }) snapshotOutputs
          }
      Right buffer -> do
        storedOutput <- liftEffect $ bufferToUTF8 buffer
        let
          _ = spy "storedOutput" storedOutput
          -- TODO: What if we haven't accepted an output with the same number of format permutations?
          -- TODO: Should report "No format for...."
          storedOutputDirectives :: Array String
          storedOutputDirectives =
            storedOutput
              # Regex.match directiveRegex
              # foldMap (NEA.toUnfoldable >>> Array.catMaybes)
              # map String.trim
              # Array.mapMaybe (\input -> map (const input) $ hush $ runParser formatDirective' input)
            where
            formatDirective' :: forall a. Parser (FormatDirective e a)
            formatDirective' = formatDirective

          matchedOutputs =
            storedOutput
              # Regex.split directiveRegex
              # Array.zip snapshotOutputs

          checkOutput :: Tuple String String -> Aff { output :: String, result :: SnapshotResult }
          checkOutput (Tuple output saved) =
            if output == saved then
              pure { output, result: Passed }
            else if accept then do
              acceptOutput
              pure { output, result: Accepted }
            else do
              { stdout: diffBuff } <- exec ("diff <(echo \"" <> output <> "\") <(echo \"" <> saved <> "\")")
              diffOutput <- liftEffect $ bufferToUTF8 diffBuff
              pure { output, result: Failed diffOutput }

          acceptedOutput :: String -> Aff { output :: String, result :: SnapshotResult }
          acceptedOutput output =
            pure { output, result: Accepted }

        if (spy "storedOutputDirectives" storedOutputDirectives) == (spy "inputModule directives" (Set.toUnfoldable (Map.keys inputModule.directives))) then do
          results <- for matchedOutputs checkOutput
          pure { name, results }
        else if accept then do
          acceptOutput
          results <- for snapshotOutputs acceptedOutput
          pure { name, results }
        else
          makeErrorResult name (error "Mismatched format options in output file.")

  formatModule :: forall e a. PrintOptions -> FormatOptions e a -> Module e -> String
  formatModule opts conf = Dodo.print Dodo.plainText opts <<< Tidy.toDoc <<< Tidy.formatModule conf

exec :: String -> Aff ExecResult
exec command = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Effect String
bufferToUTF8 = map (ImmutableBuffer.toString UTF8) <<< freeze
