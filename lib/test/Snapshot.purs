module Test.Snapshot where

import Prelude

import Control.Alternative (guard)
import Data.Array (dropEnd, mapMaybe)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Posix.Signal (Signal(..))
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), split, stripSuffix)
import Data.String as String
import Data.String.Regex as Regex
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Dodo (PrintOptions)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError, effectCanceler, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Buffer (Buffer, freeze)
import Node.Buffer as Buffer
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess.Types as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (Module)
import Test.FormatDirective (defaultFormat, directiveRegex, parseDirectivesFromModule)
import Tidy (class FormatError, FormatOptions)
import Tidy as Tidy
import Tidy.Operators (parseOperatorTable)
import Tidy.Operators.Defaults (defaultOperators)
import Tidy.Precedence (PrecedenceMap)

data SnapshotResult
  = Passed
  | Saved
  | Accepted
  | Failed String
  | ErrorRunningTest Error

type SnapshotTest =
  { name :: String
  , results :: Array { output :: String, result :: SnapshotResult, directive :: String }
  }

isBad :: SnapshotResult -> Boolean
isBad = case _ of
  Failed _ -> true
  ErrorRunningTest _ -> true
  _ -> false

newtype SnapshotResultGroup = SnapshotResultGroup
  { results :: Array SnapshotTest
  , nested :: Map String SnapshotResultGroup
  , hasBad :: Boolean
  }

snapshotDirectory :: { path :: String, trimPath :: String -> String }
snapshotDirectory =
  { path: "./test/snapshots"
  , trimPath: String.replace (Pattern "test/snapshots/") (Replacement "")
  }

snapshotFormat :: Boolean -> Maybe Pattern -> Aff SnapshotResultGroup
snapshotFormat accept mbPattern = do
  paths <- mapMaybe goPath <<< Array.fromFoldable <$> expandGlobs snapshotDirectory.path [ "**/*.purs" ]
  tested <- for paths runSnapshot
  pure $ groupSnapshots tested
  where
  groupSnapshots :: Array (Tuple (Array String) SnapshotTest) -> SnapshotResultGroup
  groupSnapshots =
    Array.sortWith fst
      >>> foldl addToGroup (emptyGroup false)

  addToGroup :: SnapshotResultGroup -> Tuple (Array String) SnapshotTest -> SnapshotResultGroup
  addToGroup (SnapshotResultGroup { results, nested, hasBad }) (Tuple path result) = do
    let
      badResult = Array.any (_.result >>> isBad) result.results
    case Array.uncons path of
      Nothing ->
        SnapshotResultGroup
          { results: Array.snoc results result
          , nested
          , hasBad: hasBad || badResult
          }
      Just { head, tail } ->
        SnapshotResultGroup
          { results
          , nested: Map.alter (fromMaybe (emptyGroup badResult) >>> flip addToGroup (Tuple tail result) >>> Just) head nested
          , hasBad: hasBad || badResult
          }

  emptyGroup :: Boolean -> SnapshotResultGroup
  emptyGroup hasBad = SnapshotResultGroup
    { results: []
    , nested: Map.empty
    , hasBad
    }

  goPath :: String -> Maybe { path :: FilePath, name :: String }
  goPath path = do
    let
      splitPath = split (Pattern "/") path
    name <- filterPath =<< stripSuffix (Pattern ".purs") =<< Array.last splitPath
    pure { path, name }

  filterPath = case mbPattern of
    Just pat ->
      \path ->
        guard (String.contains pat path) $> path
    Nothing ->
      pure

  makeErrorResult :: String -> Error -> Aff SnapshotTest
  makeErrorResult name err = pure { name, results: [ { output: "", result: ErrorRunningTest err, directive: "" } ] }

  runSnapshot :: { path :: FilePath, name :: String } -> Aff (Tuple (Array String) SnapshotTest)
  runSnapshot { path, name } = flip catchError (map (Tuple testPath) <<< makeErrorResult name) do
    let outputPath = String.replace (Pattern ".purs") (Replacement ".output") path
    contents <- liftEffect <<< bufferToUTF8 =<< readFile path
    case parseModule contents of
      ParseSucceeded mod ->
        Tuple testPath <$> runSnapshotForModule name outputPath mod
      ParseSucceededWithErrors mod _ ->
        Tuple testPath <$> runSnapshotForModule name outputPath mod
      ParseFailed err -> do
        let
          formattedError =
            printParseError err.error
              <> " at "
              <> show (err.position.line + 1)
              <> ":"
              <> show (err.position.column + 1)
        pure (Tuple testPath { name, results: [ { output: "", result: Failed formattedError, directive: "" } ] })
    where
    testPath :: Array String
    testPath = dropEnd 1 $ String.split (Pattern "/") $ snapshotDirectory.trimPath path

  runSnapshotForModule :: forall e. FormatError e => String -> FilePath -> Module e -> Aff SnapshotTest
  runSnapshotForModule name outputPath mod = do
    let
      inputModule = parseDirectivesFromModule mod

      formatModuleWith { printOptions, formatOptions } =
        formatModule printOptions formatOptions inputModule.module

      defaultFormattedModule =
        formatModuleWith defaultFormat

      snapshotOutputs =
        Array.cons (Tuple "Default formatting" defaultFormattedModule)
          $ map (\(Tuple s d) -> Tuple s (formatModuleWith d))
          $ Map.toUnfoldable inputModule.directives

      snapshotOutputFileContents = Array.fold
        [ defaultFormattedModule
        , inputModule.directives # foldMapWithIndex \directiveSource directive ->
            "\n"
              <> directiveSource
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
          , results: map (\(Tuple directive output) -> { result: Saved, output, directive }) snapshotOutputs
          }
      Right buffer -> do
        storedOutput <- liftEffect $ bufferToUTF8 buffer
        let
          storedOutputDirectives :: Array String
          storedOutputDirectives =
            storedOutput
              # Regex.match directiveRegex
              # foldMap (NEA.toUnfoldable >>> Array.catMaybes)
              # map String.trim
              # Array.mapMaybe (\input -> if String.contains (String.Pattern "@format") input then Just input else Nothing)

          matchedOutputs =
            storedOutput
              # Regex.split directiveRegex
              # Array.zip snapshotOutputs

          checkOutput :: Tuple (Tuple String String) String -> Aff { output :: String, result :: SnapshotResult, directive :: String }
          checkOutput (Tuple (Tuple directive output) saved) =
            if output == saved then
              pure { output, result: Passed, directive }
            else if accept then do
              acceptOutput
              pure { output, result: Accepted, directive }
            else do
              let
                diffCmd = "diff <(echo \"" <> output <> "\") <(echo \"" <> saved <> "\")"
              { stdout: diffBuff } <- exec diffCmd
              diffOutput <- liftEffect $ bufferToUTF8 diffBuff
              pure { output, result: Failed diffOutput, directive }

          acceptedOutput :: Tuple String String -> Aff { output :: String, result :: SnapshotResult, directive :: String }
          acceptedOutput (Tuple directive output) =
            pure { output, result: Accepted, directive }

        if storedOutputDirectives == Set.toUnfoldable (Map.keys inputModule.directives) then do
          results <- for matchedOutputs checkOutput
          pure { name, results }
        else if accept then do
          acceptOutput
          results <- for snapshotOutputs acceptedOutput
          pure { name, results }
        else
          makeErrorResult name (error "Mismatched format options in output file.")

  formatModule :: forall e a. PrintOptions -> FormatOptions e a -> Module e -> String
  formatModule opts conf = Dodo.print Dodo.plainText opts <<< Tidy.toDoc <<< Tidy.formatModule (conf { operators = operators })

  operators :: PrecedenceMap
  operators = parseOperatorTable defaultOperators

exec :: String -> Aff ExecResult
exec command = makeAff \k -> do
  childProc <- ChildProcess.exec' command (\x -> x { shell = Just (ChildProcess.customShell "/bin/bash") }) (k <<< pure)
  pure $ effectCanceler $ map (const unit) $ ChildProcess.killSignal SIGABRT childProc

bufferToUTF8 :: Buffer -> Effect String
bufferToUTF8 = map (ImmutableBuffer.toString UTF8) <<< freeze
