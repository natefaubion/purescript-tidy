module Test.Snapshot where

import Prelude

import Control.MonadZero (guard)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..), stripSuffix)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.Tuple (uncurry)
import Dodo (PrintOptions, twoSpaces)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError, effectCanceler, makeAff, try)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, freeze)
import Node.Buffer as Buffer
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, defaultExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readdir, writeFile)
import Node.Path (FilePath, basename)
import Node.Path as Path
import Node.Stream as Stream
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Tidy (class FormatError, TypeArrowOption(..), FormatOptions, defaultFormatOptions)
import PureScript.CST.Tidy as Tidy
import PureScript.CST.Types (Comment(..), Module(..), ModuleHeader(..), SourceToken)

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
  makeErrorResult name err = pure { name, output: "", result: ErrorRunningTest err }

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
        pure { name, output: "", result: Failed formattedError }

  runSnapshotForModule :: forall e. FormatError e => String -> FilePath -> Module e -> Aff SnapshotTest
  runSnapshotForModule name outputPath mod' = do
    let printOptions = twoSpaces { pageWidth = top :: Int }
    let formatOptions = defaultFormatOptions
    let { mod, options } = parseOptionsFromModule mod'
    let
      outputs =
        options # Array.mapWithIndex \ix { original, option } -> fold do
          [ original <> "\n" # guard (ix /= 0)
          , formatModule printOptions (formatOptions { typeArrowPlacement = option }) mod
          ]

    let acceptOutput = writeFile outputPath =<< liftEffect (Buffer.fromString (Array.intercalate "\n" outputs) UTF8)
    savedOutputFile <- try $ readFile outputPath
    case savedOutputFile of
      Left _ -> do
        acceptOutput
        pure { name, output, result: Saved }
      Right buffer -> do
        savedOutput <- liftEffect $ bufferToUTF8 buffer
        let
          matchedOutputs =
            savedOutput
              # Regex.split optionRegex
              # Array.zip outputs

        -- TODO: Test outputs in format-matching pairs, producing multiple SnapshotResults

        if Array.all (uncurry eq) matchedOutput then
          pure { name, output, result: Passed }
        else if accept then do
          acceptOutput
          pure { name, output, result: Accepted }
        else do
          { stdout: diffBuff } <- execWithStdin ("diff " <> outputPath <> " -") output
          diffOutput <- liftEffect $ bufferToUTF8 diffBuff
          pure { name, output, result: Failed diffOutput }

  formatModule :: forall e a. PrintOptions -> FormatOptions e a -> Module e -> String
  formatModule opts conf = Dodo.print Dodo.plainText opts <<< Tidy.toDoc <<< Tidy.formatModule conf

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input mempty
  Stream.end (ChildProcess.stdin childProc) mempty
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Effect String
bufferToUTF8 = map (ImmutableBuffer.toString UTF8) <<< freeze

parseOptionsFromModule
  :: forall e
   . Module e
  -> { mod :: Module e, options :: Array { original :: String, option :: TypeArrowOption } }
parseOptionsFromModule (Module { header: ModuleHeader header, body }) =
  { mod
  , options
  }
  where
  headerComments = header.keyword.leadingComments

  defaultOptions :: forall a. FormatOptions Void a
  defaultOptions = defaultFormatOptions

  options =
    Array.cons defaultOptions.typeArrowPlacement
      $ Array.mapMaybe parseOptionFromComment headerComments

  keyword =
    { range: header.keyword.range
    , trailingComments: header.keyword.trailingComments
    , value: header.keyword.value
    , leadingComments: Array.filter (isNothing <<< parseOptionFromComment) headerComments
    }

  strippedHeader = ModuleHeader
    { keyword
    , name: header.name
    , exports: header.exports
    , where: header.where
    , imports: header.imports
    }

  mod = Module
    { header: strippedHeader
    , body
    }

  parseOptionFromComment :: forall l. Comment l -> Maybe TypeArrowOption
  parseOptionFromComment = case _ of
    Space _ -> Nothing
    Line _ _ -> Nothing
    Comment " @format arrow-first" -> Just TypeArrowFirst
    Comment " @format arrow-last" -> Just TypeArrowLast
    Comment _ -> Nothing

optionRegex :: Regex
optionRegex = unsafeRegex " @format (arrow-first|arrow-last)" noFlags
