module Bin.Worker where

import Prelude

import Bin.FormatOptions (FormatOptions)
import Bin.FormatOptions as FormatOptions
import Bin.Timing (hrtime, hrtimeDiff, toMilliseconds)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either, fromRight')
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.WorkerBees as Worker
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import Tidy (defaultFormatOptions, formatModule, toDoc)
import Tidy.Operators (parseOperatorTable)
import Tidy.Precedence (PrecedenceMap, remapOperators)

type WorkerConfig =
  { importSort :: String
  , importWrap :: String
  , indent :: Int
  , operatorsFile :: String
  , ribbon :: Number
  , thenPlacement :: String
  , typeArrowPlacement :: String
  , unicode :: String
  , width :: Int
  }

toWorkerConfig :: FormatOptions -> WorkerConfig
toWorkerConfig options =
  { importSort: FormatOptions.importSortToString options.importSort
  , importWrap: FormatOptions.importWrapToString options.importWrap
  , indent: options.indent
  , operatorsFile: fromMaybe ".tidyoperators.default" options.operatorsFile
  , ribbon: options.ribbon
  , thenPlacement: FormatOptions.thenPlacementToString options.thenPlacement
  , typeArrowPlacement: FormatOptions.typeArrowPlacementToString options.typeArrowPlacement
  , unicode: FormatOptions.unicodeToString options.unicode
  , width: fromMaybe top options.width
  }

type WorkerData =
  { shouldCheck :: Boolean
  , operatorsByPath :: Object (Array String)
  }

type WorkerInput =
  { filePath :: FilePath
  , config :: WorkerConfig
  }

type WorkerOutput =
  { filePath :: FilePath
  , error :: String
  , alreadyFormatted :: Boolean
  , timing :: Number
  }

formatCommand :: FormatOptions -> PrecedenceMap -> String -> Either String String
formatCommand args operators contents = do
  let
    print = Dodo.print Dodo.plainText
      { pageWidth: fromMaybe top args.width
      , ribbonRatio: args.ribbon
      , indentWidth: args.indent
      , indentUnit: power " " args.indent
      }

  case parseModule contents of
    ParseSucceeded ok -> do
      let
        opts = defaultFormatOptions
          { importSort = args.importSort
          , importWrap = args.importWrap
          , operators = remapOperators operators ok
          , thenPlacement = args.thenPlacement
          , typeArrowPlacement = args.typeArrowPlacement
          , unicode = args.unicode
          }
      Right $ print $ toDoc $ formatModule opts ok
    ParseSucceededWithErrors _ errs -> do
      Left $ printPositionedError $ NonEmptyArray.head errs
    ParseFailed err ->
      Left $ printPositionedError err

printPositionedError :: PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error

formatInPlaceCommand :: Boolean -> PrecedenceMap -> WorkerInput -> Aff WorkerOutput
formatInPlaceCommand shouldCheck operators { filePath, config } = do
  let
    formatOptions :: FormatOptions
    formatOptions =
      { importSort:
          fromRight' (\_ -> unsafeCrashWith "Unknown importSort value") do
            FormatOptions.importSortFromString config.importSort
      , importWrap:
          fromRight' (\_ -> unsafeCrashWith "Unknown importWrap value") do
            FormatOptions.importWrapFromString config.importWrap
      , indent: config.indent
      , operatorsFile: Nothing
      , ribbon: config.ribbon
      , thenPlacement:
          fromRight' (\_ -> unsafeCrashWith "Unknown thenPlacement value") do
            FormatOptions.thenPlacementFromString config.thenPlacement
      , typeArrowPlacement:
          fromRight' (\_ -> unsafeCrashWith "Unknown typeArrowPlacement value") do
            FormatOptions.typeArrowPlacementFromString config.typeArrowPlacement
      , unicode:
          fromRight' (\_ -> unsafeCrashWith "Unknown unicode value") do
            FormatOptions.unicodeFromString config.unicode
      , width: Just config.width
      }
  contents <- FS.readTextFile UTF8 filePath
  start <- liftEffect hrtime
  case formatCommand formatOptions operators contents of
    Right formatted -> do
      timing <- map (unwrap <<< toMilliseconds) $ liftEffect $ hrtimeDiff start
      if shouldCheck then do
        let alreadyFormatted = formatted == contents
        pure { filePath, error: "", alreadyFormatted, timing }
      else do
        FS.writeTextFile UTF8 filePath formatted
        pure { filePath, error: "", alreadyFormatted: false, timing }
    Left error ->
      pure { filePath, error, alreadyFormatted: false, timing: zero }

main :: Effect Unit
main = Worker.makeAsMain \{ receive, reply, workerData: { shouldCheck, operatorsByPath } } -> do
  let
    parsedOperatorsByPath :: Object (Lazy PrecedenceMap)
    parsedOperatorsByPath =
      (\operators -> Lazy.defer \_ -> parseOperatorTable operators) <$> operatorsByPath

  receive \input@{ config } -> do
    let
      operators :: PrecedenceMap
      operators =
        maybe Map.empty Lazy.force $ Object.lookup config.operatorsFile parsedOperatorsByPath
    runAff_
      (either throwError reply)
      (formatInPlaceCommand shouldCheck operators input)
