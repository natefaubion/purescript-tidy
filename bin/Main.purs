module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.FormatOptions (FormatOptions, formatOptions)
import Bin.FormatOptions as FormatOptions
import Bin.Operators (getModuleName, parseOperatorTable, resolveOperatorExports)
import Bin.Version (version)
import Control.Monad.State (evalStateT, lift)
import Control.Monad.State as State
import Control.Parallel (parTraverse)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), fromRight')
import Data.Foldable (fold, foldMap, foldl, for_)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard, power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DefaultOperators (defaultOperators)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, makeAff, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Sync as Sync
import Node.Glob.Basic (expandGlobsCwd, expandGlobsWithStatsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (cwd)
import Node.Process as Process
import Node.Stream as Stream
import Node.WorkerBees (Worker)
import Node.WorkerBees as Worker
import Node.WorkerBees.Aff.Pool (poolTraverse)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule, toRecovered)
import PureScript.CST.Errors (ParseError, printParseError)
import PureScript.CST.ModuleGraph (ModuleSort(..), sortModules)
import PureScript.CST.Tidy (defaultFormatOptions, formatModule, toDoc)
import PureScript.CST.Tidy.Precedence (OperatorNamespace(..), PrecedenceMap, remapOperators)

data FormatMode = Check | Write
derive instance Eq FormatMode

data ConfigOption
  = Require
  | Ignore
  | Prefer

data Command
  = GenerateOperators (Array String)
  | GenerateRc FormatOptions
  | FormatInPlace FormatMode FormatOptions ConfigOption Int (Array String)
  | Format FormatOptions ConfigOption

rcFileName :: String
rcFileName = ".tidyrc.json"

parser :: ArgParser Command
parser =
  Arg.choose "command"
    [ Arg.command [ "generate-operators" ]
        "Generate an operator precedence table for better operator formatting.\nBest used with `spago sources`. Prints to stdout."
        do
          GenerateOperators <$> pursGlobs
            <* Arg.flagHelp
    , Arg.command [ "generate-config" ]
        "Writes a .tidyrc file to the current working directory based\non the command line options given."
        do
          GenerateRc <$> formatOptions
            <* Arg.flagHelp
    , Arg.command [ "format-in-place" ]
        "Format source files in place."
        do
          FormatInPlace Write
            <$> formatOptions
            <*> configOption
            <*> workerOptions
            <*> pursGlobs
            <* Arg.flagHelp
    , Arg.command [ "format" ]
        "Format input over stdin."
        do
          Format
            <$> formatOptions
            <*> configOption
            <* Arg.flagHelp
    , Arg.command [ "check" ]
        "Check source files are formatted."
        do
          FormatInPlace Check
            <$> formatOptions
            <*> configOption
            <*> workerOptions
            <*> pursGlobs
            <* Arg.flagHelp
    ]
    <* Arg.flagInfo [ "--version", "-v" ] "Shows the current version." version
    <* Arg.flagHelp
  where
  pursGlobs =
    Arg.anyNotFlag "PURS_GLOB" "Globs for PureScript sources."
      # Arg.unfolded1

  workerOptions =
    Arg.argument [ "--threads", "-t" ]
      "Number of worker threads to use.\nDefaults to 4."
      # Arg.int
      # Arg.default 4

  configOption =
    Arg.choose "config behavior"
      [ Arg.flag [ "--config-prefer", "-cp" ]
          "Always use config files when present, otherwise use CLI options.\nDefault."
          $> Prefer
      , Arg.flag [ "--config-require", "-cr" ]
          "Require the presence of a config file.\nUseful for editors."
          $> Require
      , Arg.flag [ "--config-ignore", "-ci" ]
          "Ignore all configuration files and only use CLI options.\nNot recommended."
          $> Ignore
      ]
      # Arg.default Prefer

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let
    parsedCmd =
      Arg.parseArgs "purs-tidy" "A tidy-upper for PureScript source code." parser args

  case parsedCmd of
    Left err -> do
      Console.log $ Arg.printArgError err
      case err of
        Arg.ArgError _ Arg.ShowHelp ->
          liftEffect $ Process.exit 0
        Arg.ArgError _ (Arg.ShowInfo _) ->
          liftEffect $ Process.exit 0
        _ ->
          liftEffect $ Process.exit 1
    Right cmd ->
      case cmd of
        GenerateOperators globs ->
          generateOperatorsCommand globs

        GenerateRc cliOptions -> do
          rcExists <- FS.exists rcFileName
          if rcExists then do
            Console.error $ rcFileName <> " already exists."
            liftEffect $ Process.exit 1
          else do
            let contents = Json.stringifyWithIndent 2 $ FormatOptions.toJson cliOptions
            FS.writeTextFile UTF8 rcFileName $ contents <> "\n"

        FormatInPlace mode cliOptions configOption numThreads globs -> do
          files <- expandGlobs globs
          filesWithOptions <- flip evalStateT Map.empty do
            for files \filePath -> do
              rcMap <- State.get
              rcOptions <- State.state <<< const =<< lift (resolveRcForDir rcMap (Path.dirname filePath))
              options <- lift $ getOptions cliOptions rcOptions filePath configOption
              pure
                { filePath
                , config: toWorkerConfig options
                }

          operatorsByPath <-
            filesWithOptions
              # map _.config.operatorsFile
              # Array.nub
              # parTraverse (\path -> Tuple path <$> readOperatorTable path)
              # map Object.fromFoldable

          let
            workerData =
              { shouldCheck: mode == Check
              , operatorsByPath
              }

          results <- poolTraverse formatWorker workerData numThreads filesWithOptions

          let
            { errors, notFormatted } =
              results # foldMap \{ filePath, error, alreadyFormatted } ->
                { errors: guard (not String.null error) [ filePath /\ error ]
                , notFormatted: guard (not alreadyFormatted) [ filePath ]
                }

          case mode of
            Write ->
              for_ errors \(Tuple filePath error) ->
                Console.error $ filePath <> ":\n  " <> error <> "\n"

            Check -> liftEffect do
              if Array.null errors && Array.null notFormatted then do
                Console.log "All files are formatted."
                Process.exit 0
              else do
                unless (Array.null errors) do
                  Console.log "Some files have errors:\n"
                  for_ errors \(Tuple filePath error) ->
                    Console.error $ filePath <> ":\n  " <> error <> "\n"
                unless (Array.null notFormatted) do
                  Console.log "Some files are not formatted:\n"
                  for_ notFormatted Console.error
                Process.exit 1

        Format cliOptions configOption -> do
          Tuple rcOptions _ <- resolveRcForDir Map.empty =<< liftEffect cwd
          options <- getOptions cliOptions rcOptions "the current directory." configOption
          operators <- parseOperatorTable <<< fromMaybe defaultOperators <$> traverse readOperatorTable options.operatorsFile
          contents <- readStdin
          case formatCommand options operators contents of
            Left err -> do
              Console.error $ printParseError err
              liftEffect $ Process.exit 1
            Right str ->
              makeAff \k -> do
                _ <- Stream.writeString Process.stdout UTF8 str (k (Right unit))
                pure mempty

expandGlobs :: Array String -> Aff (Array String)
expandGlobs = map dirToGlob >>> expandGlobsWithStatsCwd >>> map onlyFiles
  where
  dirToGlob path =
    if Path.extname path == "" then
      if isJust (String.stripSuffix (Pattern "**") path) then
        Path.concat [ path, "*.purs" ]
      else
        Path.concat [ path, "**", "*.purs" ]
    else
      path

  onlyFiles =
    Map.filter Stats.isFile
      >>> Map.keys
      >>> Set.toUnfoldable

getOptions :: FormatOptions -> Maybe FormatOptions -> FilePath -> ConfigOption -> Aff FormatOptions
getOptions cliOptions rcOptions filePath = case _ of
  Prefer -> pure $ fromMaybe cliOptions rcOptions
  Ignore -> pure cliOptions
  Require ->
    case rcOptions of
      Nothing -> do
        Console.error $ rcFileName <> " not found for " <> filePath
        liftEffect $ Process.exit 1
      Just options ->
        pure options

readOperatorTable :: FilePath -> Aff (Array String)
readOperatorTable path
  | path == ".tidyoperators.default" = pure defaultOperators
  | otherwise = String.split (Pattern "\n") <$> FS.readTextFile UTF8 path

formatCommand :: FormatOptions -> PrecedenceMap -> String -> Either ParseError String
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
          { operators = remapOperators operators ok
          , typeArrowPlacement = args.typeArrowPlacement
          , unicode = args.unicode
          }
      Right $ print $ toDoc $ formatModule opts ok
    ParseSucceededWithErrors ok _ -> do
      let
        opts =
          defaultFormatOptions
            { operators = remapOperators operators ok
            , typeArrowPlacement = args.typeArrowPlacement
            , unicode = args.unicode
            }
      Right $ print $ toDoc $ formatModule opts ok
    ParseFailed err ->
      Left err.error

type WorkerConfig =
  { importWrap :: String
  , indent :: Int
  , operatorsFile :: String
  , ribbon :: Number
  , typeArrowPlacement :: String
  , unicode :: String
  , width :: Int
  }

toWorkerConfig :: FormatOptions -> WorkerConfig
toWorkerConfig options =
  { importWrap: FormatOptions.importWrapToString options.importWrap
  , indent: options.indent
  , operatorsFile: fromMaybe ".tidyoperators.default" options.operatorsFile
  , ribbon: options.ribbon
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
  }

formatWorker :: Worker WorkerData WorkerInput WorkerOutput
formatWorker = Worker.make \{ receive, reply, workerData: { shouldCheck, operatorsByPath } } -> do
  let
    parsedOperatorsByPath :: Object (Lazy PrecedenceMap)
    parsedOperatorsByPath =
      (\operators -> Lazy.defer \_ -> parseOperatorTable operators) <$> operatorsByPath

  receive \{ filePath, config } -> do
    let
      operators :: PrecedenceMap
      operators =
        maybe Map.empty Lazy.force $ Object.lookup config.operatorsFile parsedOperatorsByPath

      formatOptions :: FormatOptions
      formatOptions =
        { importWrap:
            fromRight' (\_ -> unsafeCrashWith "Unknown importWrap value") do
              FormatOptions.importWrapFromString config.importWrap
        , indent: config.indent
        , operatorsFile: Nothing
        , ribbon: config.ribbon
        , typeArrowPlacement:
            fromRight' (\_ -> unsafeCrashWith "Unknown typeArrowPlacement value") do
              FormatOptions.typeArrowPlacementFromString config.typeArrowPlacement
        , unicode:
            fromRight' (\_ -> unsafeCrashWith "Unknown unicode value") do
              FormatOptions.unicodeFromString config.unicode
        , width: Just config.width
        }

    contents <- Sync.readTextFile UTF8 filePath
    case formatCommand formatOptions operators contents of
      Right formatted ->
        if shouldCheck then do
          let alreadyFormatted = formatted == contents
          reply { filePath, error: "", alreadyFormatted }
        else do
          Sync.writeTextFile UTF8 filePath formatted
          reply { filePath, error: "", alreadyFormatted: false }
      Left err ->
        reply { filePath, error: printParseError err, alreadyFormatted: false }

type RcMap = Map FilePath (Maybe FormatOptions)

resolveRcForDir :: RcMap -> FilePath -> Aff (Tuple (Maybe FormatOptions) RcMap)
resolveRcForDir = go List.Nil
  where
  go :: List FilePath -> RcMap -> FilePath -> Aff (Tuple (Maybe FormatOptions) RcMap)
  go paths cache dir = case Map.lookup dir cache of
    Just res ->
      pure $ unwind cache res paths
    Nothing -> do
      let filePath = Path.concat [ dir, rcFileName ]
      contents <- try $ FS.readTextFile UTF8 filePath
      case contents of
        Left _
          | dir == Path.sep ->
              pure $ unwind cache Nothing (List.Cons dir paths)
          | otherwise ->
              go (List.Cons dir paths) cache (Path.dirname dir)
        Right contents' ->
          case FormatOptions.fromJson =<< parseJson contents' of
            Left jsonError ->
              throwError $ error $ "Could not decode " <> filePath <> ": " <> printJsonDecodeError jsonError
            Right options -> do
              let
                resolvedOptions =
                  options { operatorsFile = Path.relative dir <$> options.operatorsFile }
              pure $ unwind cache (Just resolvedOptions) (List.Cons dir paths)

  unwind :: RcMap -> Maybe FormatOptions -> List FilePath -> Tuple (Maybe FormatOptions) RcMap
  unwind cache res = case _ of
    List.Cons p ps ->
      unwind (Map.insert p res cache) res ps
    List.Nil ->
      Tuple res cache

readStdin :: Aff String
readStdin = makeAff \k -> do
  contents <- Ref.new ""
  Stream.onData Process.stdin \buff -> do
    chunk <- Buffer.toString UTF8 buff
    void $ Ref.modify (_ <> chunk) contents
  Stream.onEnd Process.stdin do
    k <<< Right =<< Ref.read contents
  pure mempty

generateOperatorsCommand :: Array String -> Aff Unit
generateOperatorsCommand globs = do
  sourcePaths <- expandGlobsCwd globs
  modules <-
    sourcePaths # Array.fromFoldable # parTraverse \path -> do
      contents <- liftEffect <<< Buffer.toString UTF8 =<< FS.readFile path
      pure $ parseModule contents

  let
    parsedModules = modules # Array.mapMaybe case _ of
      ParseSucceeded m -> Just $ toRecovered m
      ParseSucceededWithErrors m _ -> Just m
      _ -> Nothing

  case sortModules (_.header <<< unwrap) parsedModules of
    CycleDetected ms -> do
      let modNames = map (unwrap <<< getModuleName) ms
      Console.error $ String.joinWith "\n"
        [ "Cycle detected in modules:"
        , String.joinWith "\n" $ map (append "  ") modNames
        ]
    Sorted sorted -> do
      let precMap = foldl resolveOperatorExports Map.empty sorted
      for_ (Map.toUnfoldable precMap :: Array _) \(Tuple mbModName ops) -> do
        let modName = foldMap unwrap mbModName
        for_ (Map.toUnfoldable ops :: Array _) \(Tuple (Tuple ns op) prec) -> do
          Console.log $ fold
            [ modName
            , case ns of
                OperatorType -> ".(" <> unwrap op <> ")" <> " type"
                OperatorValue -> ".(" <> unwrap op <> ")"
            , " " <> show prec
            ]

