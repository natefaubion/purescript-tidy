module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.FormatOptions (FormatOptions, formatOptions)
import Bin.FormatOptions as FormatOptions
import Bin.Version (version)
import Bin.Worker (WorkerData, WorkerInput, WorkerOutput, formatCommand, formatInPlaceCommand, toWorkerConfig)
import Control.Monad.State (evalStateT, lift)
import Control.Monad.State as State
import Control.Parallel (parTraverse)
import Control.Plus ((<|>))
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Foldable (fold, foldMap, foldl, for_, oneOf)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Number.Format as NF
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, makeAff, throwError, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.Glob.Basic (expandGlobsCwd, expandGlobsWithStatsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import Node.WorkerBees as Worker
import Node.WorkerBees.Aff.Pool (poolTraverse)
import PureScript.CST (RecoveredParserResult(..), parseModule, toRecovered)
import PureScript.CST.ModuleGraph (ModuleSort(..), sortModules)
import PureScript.CST.Types (Module(..), ModuleHeader(..), Name(..))
import Tidy.Operators (parseOperatorTable, resolveOperatorExports)
import Tidy.Operators.Defaults (defaultOperators)
import Tidy.Precedence (OperatorNamespace(..), PrecedenceMap)

data FormatMode = Check | Write

derive instance Eq FormatMode

data ConfigOption
  = Require
  | Ignore
  | Prefer

data Command
  = GenerateOperators (Array String)
  | GenerateRc FormatOptions
  | FormatInPlace FormatMode FormatOptions ConfigOption Int Boolean (Array String)
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
            <*> timingOption
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
            <*> timingOption
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

  timingOption =
    Arg.flag [ "--timing" ]
      "Print the time spent formatting each file."
      # Arg.boolean
      # Arg.default false

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
          rcStats <- Aff.try $ FS.stat rcFileName
          if isLeft rcStats then do
            let contents = Json.stringifyWithIndent 2 $ FormatOptions.toJson cliOptions
            FS.writeTextFile UTF8 rcFileName $ contents <> "\n"
          else do
            Console.error $ rcFileName <> " already exists."
            liftEffect $ Process.exit 1

        FormatInPlace mode cliOptions configOption numThreads printTiming globs -> do
          currentDir <- liftEffect Process.cwd
          let root = (Path.parse currentDir).root
          srcLocation <- fold <$> liftEffect (Process.lookupEnv "TIDY_INSTALL_LOC")
          files <- expandGlobs globs
          filesWithOptions <- flip evalStateT Map.empty do
            for files \filePath -> do
              rcMap <- State.get
              rcOptions <- State.state <<< const =<< lift (resolveRcForDir root rcMap (Path.dirname filePath))
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

          results <-
            if Array.length filesWithOptions > numThreads * 2 then do
              -- Worker location for production bin
              let bundleLocation = Path.concat [ srcLocation, "bundle", "Bin.Worker", "index.js" ]
              -- Worker location for local dev
              let outputLocation = Path.concat [ srcLocation, "output", "Bin.Worker", "index.js" ]
              worker <-
                oneOf
                  [ FS.stat bundleLocation $> Worker.unsafeWorkerFromPath bundleLocation
                  , FS.stat outputLocation $> Worker.unsafeWorkerFromPath outputLocation
                  ]
                  <|> throwError (error "Worker not found")
              poolTraverse worker workerData numThreads filesWithOptions
            else
              parTraverse (formatInPlaceOne workerData) filesWithOptions

          let
            { errors, notFormatted } =
              results # foldMap \{ filePath, error, alreadyFormatted } ->
                { errors: guard (not String.null error) [ filePath /\ error ]
                , notFormatted: guard (not alreadyFormatted) [ filePath ]
                }

          when printTiming do
            for_ (Array.sortBy (comparing _.timing) results) \{ filePath, timing } ->
              when (timing > 0.0) do
                Console.log $ fold
                  [ Path.relative currentDir filePath
                  , " "
                  , NF.toStringWith (NF.fixed 2) timing
                  , "ms"
                  ]

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
          currentDir <- liftEffect Process.cwd
          let root = (Path.parse currentDir).root
          Tuple rcOptions _ <- resolveRcForDir root Map.empty currentDir
          options <- getOptions cliOptions rcOptions "the current directory." configOption
          operators <- parseOperatorTable <<< fromMaybe defaultOperators <$> traverse readOperatorTable options.operatorsFile
          contents <- readStdin
          case formatCommand options operators contents of
            Left err -> do
              Console.error err
              liftEffect $ Process.exit 1
            Right str ->
              makeAff \k -> do
                _ <- Stream.writeString Process.stdout UTF8 str (const (k (Right unit)))
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

formatInPlaceOne :: WorkerData -> WorkerInput -> Aff WorkerOutput
formatInPlaceOne { shouldCheck, operatorsByPath } input@{ config } = do
  let
    parsedOperatorsByPath :: Object (Lazy PrecedenceMap)
    parsedOperatorsByPath =
      (\operators -> Lazy.defer \_ -> parseOperatorTable operators) <$> operatorsByPath

  let
    operators :: PrecedenceMap
    operators =
      maybe Map.empty Lazy.force $ Object.lookup config.operatorsFile parsedOperatorsByPath

  formatInPlaceCommand shouldCheck operators input

type RcMap = Map FilePath (Maybe FormatOptions)

resolveRcForDir :: FilePath -> RcMap -> FilePath -> Aff (Tuple (Maybe FormatOptions) RcMap)
resolveRcForDir root = go List.Nil
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
          | dir == root ->
              pure $ unwind cache Nothing (List.Cons dir paths)
          | otherwise ->
              go (List.Cons dir paths) cache (Path.dirname dir)
        Right contents' ->
          case FormatOptions.fromJson =<< parseJson contents' of
            Left jsonError ->
              throwError $ error $ "Could not decode " <> filePath <> ": " <> printJsonDecodeError jsonError
            Right options -> do
              operatorsFile <- liftEffect $ traverse (Path.resolve [ dir ]) options.operatorsFile
              pure $ unwind cache (Just options { operatorsFile = operatorsFile }) (List.Cons dir paths)

  unwind :: RcMap -> Maybe FormatOptions -> List FilePath -> Tuple (Maybe FormatOptions) RcMap
  unwind cache res = case _ of
    List.Cons p ps ->
      unwind (Map.insert p res cache) res ps
    List.Nil ->
      Tuple res cache

readStdin :: Aff String
readStdin = makeAff \k -> do
  contents <- Ref.new []
  Stream.onData Process.stdin \buff -> do
    void $ Ref.modify (_ `Array.snoc` buff) contents
  Stream.onEnd Process.stdin do
    k <<< Right =<< Buffer.toString UTF8 =<< Buffer.concat =<< Ref.read contents
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
      let getModuleName (Module { header: ModuleHeader { name: Name { name } } }) = name
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
