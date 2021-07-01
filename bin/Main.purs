module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.FormatOptions (FormatOptions, formatOptions)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, foldl, foldr, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), snd, uncurry)
import DefaultOperators (defaultOperators)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Sync as Sync
import Node.Glob.Basic (expandGlobsCwd, expandGlobsWithStatsCwd)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import Node.WorkerBees (Worker)
import Node.WorkerBees as Worker
import Node.WorkerBees.Aff.Pool (poolTraverse)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule, toRecovered)
import PureScript.CST.Errors (ParseError, printParseError)
import PureScript.CST.Lexer as Lexer
import PureScript.CST.ModuleGraph (ModuleSort(..), sortModules)
import PureScript.CST.Tidy (TypeArrowOption(..), UnicodeOption(..), defaultFormatOptions, formatModule, toDoc)
import PureScript.CST.Tidy.Precedence (OperatorNamespace(..), Precedence, QualifiedOperator(..), PrecedenceMap, insertOperator, lookupOperator, remapOperators)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Declaration(..), Export(..), FixityOp(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator(..), Separated(..), Token(..), Wrapped(..))

data Command
  = GenerateOperators (Array String)
  | FormatInPlace FormatOptions Int (Array String)
  | Format FormatOptions

parser :: ArgParser Command
parser =
  Arg.choose "command"
    [ Arg.command [ "generate-operators" ]
        "Generate an operator precedence table for better operator formatting.\nBest used with `spago sources`. Prints to stdout."
        do
          GenerateOperators <$> pursGlobs
            <* Arg.flagHelp
    , Arg.command [ "format-in-place" ]
        "Format source files in place."
        do
          FormatInPlace
            <$> formatOptions
            <*>
              do
                Arg.argument [ "--threads", "-t" ]
                  "Number of worker threads to use.\nDefaults to 4."
                  # Arg.int
                  # Arg.default 4
            <*> pursGlobs
            <* Arg.flagHelp
    , Arg.command [ "format" ]
        "Format input over stdin."
        do
          Format <$> formatOptions
            <* Arg.flagHelp
    ]
    <* Arg.flagInfo [ "--version", "-v" ] "Shows the current version." "v1.0.0"
    <* Arg.flagHelp
  where
  pursGlobs =
    Arg.anyNotFlag "PURS_GLOB" "Globs for PureScript sources."
      # Arg.unfolded1

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
        FormatInPlace options numThreads globs -> do
          operators <- readOperatorTable options.operators
          paths <- expandGlobsWithStatsCwd globs
          let
            files :: Array String
            files =
              paths
                # Map.filter Stats.isFile
                # Map.keys
                # Set.toUnfoldable

            workerConfig :: WorkerConfig
            workerConfig =
              { indent: options.indent
              , operators
              , ribbon: options.ribbon
              , typeArrowPlacement:
                  case options.typeArrowPlacement of
                    TypeArrowFirst -> "arrow-first"
                    TypeArrowLast -> "arrow-last"
              , unicode:
                  case options.unicode of
                    UnicodeSource -> "source"
                    UnicodeAlways -> "always"
                    UnicodeNever -> "never"
              , width: options.width
              }

          results <- poolTraverse formatWorker workerConfig numThreads files
          for_ results \{ filePath, error } ->
            unless (String.null error) do
              Console.error $ filePath <> ":\n  " <> error <> "\n"

        Format options -> do
          operators <- parseOperatorTable <$> readOperatorTable options.operators
          contents <- readStdin
          case formatCommand options operators contents of
            Left err -> do
              Console.error $ printParseError err
              liftEffect $ Process.exit 1
            Right str ->
              Console.log str

readOperatorTable :: Maybe FilePath -> Aff (Array String)
readOperatorTable = case _ of
  Nothing ->
    pure defaultOperators
  Just path -> do
    table <- liftEffect <<< Buffer.toString UTF8 =<< FS.readFile path
    pure $ String.split (Pattern "\n") table

formatCommand :: FormatOptions -> PrecedenceMap -> String -> Either ParseError String
formatCommand args operators contents = do
  let
    print = Dodo.print Dodo.plainText
      { pageWidth: args.width
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
  { indent :: Int
  , operators :: Array String
  , ribbon :: Number
  , typeArrowPlacement :: String
  , unicode :: String
  , width :: Int
  }

formatWorker :: Worker WorkerConfig FilePath { filePath :: FilePath, error :: String }
formatWorker = Worker.make \{ receive, reply, workerData } -> do
  let
    operators =
      parseOperatorTable workerData.operators

    formatOptions =
      { indent: workerData.indent
      , operators: Nothing
      , ribbon: workerData.ribbon
      , typeArrowPlacement:
          case workerData.typeArrowPlacement of
            "arrow-first" -> TypeArrowFirst
            "arrow-last" -> TypeArrowLast
            _ -> unsafeCrashWith "Unknown typeArrowPlacement"
      , unicode:
          case workerData.unicode of
            "source" -> UnicodeSource
            "always" -> UnicodeAlways
            "never" -> UnicodeNever
            _ -> unsafeCrashWith "Unknown unicode"
      , width: workerData.width
      }

  receive \filePath -> do
    contents <- Sync.readTextFile UTF8 filePath
    case formatCommand formatOptions operators contents of
      Right formatted -> do
        Sync.writeTextFile UTF8 filePath formatted
        reply { filePath, error: "" }
      Left err ->
        reply { filePath, error: printParseError err }

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

parseOperatorTable :: Array String -> PrecedenceMap
parseOperatorTable = foldr (uncurry insertOperator) Map.empty <<< Array.mapMaybe parseOperatorPrec

parseOperatorPrec :: String -> Maybe (Tuple QualifiedOperator Precedence)
parseOperatorPrec = Lexer.lex >>> tokenStreamToArray >>> case _ of
  Right [ TokSymbolName modName op, TokInt _ prec ] ->
    Just $ Tuple (QualifiedOperator modName OperatorValue (Operator op)) prec
  Right [ TokSymbolName modName op, TokLowerName Nothing "type", TokInt _ prec ] ->
    Just $ Tuple (QualifiedOperator modName OperatorType (Operator op)) prec
  _ ->
    Nothing

resolveOperatorExports :: forall e. PrecedenceMap -> Module e -> PrecedenceMap
resolveOperatorExports precMap mod@(Module { header: ModuleHeader { exports }, body: ModuleBody { decls } }) =
  case exports of
    Nothing ->
      foldl goDecl precMap decls
    Just (Wrapped { value: Separated { head, tail } }) ->
      foldl goExport precMap $ Array.cons head $ map snd tail
  where
  modName =
    getModuleName mod

  remappedPrecMap =
    remapOperators precMap mod

  goExport pm = fromMaybe pm <<< case _ of
    ExportOp (Name { name: op }) -> do
      prec <- lookupOperator (QualifiedOperator Nothing OperatorValue op) remappedPrecMap
      pure $ insertOperator (QualifiedOperator (Just modName) OperatorValue op) prec pm
    ExportTypeOp _ (Name { name: op }) -> do
      prec <- lookupOperator (QualifiedOperator Nothing OperatorType op) remappedPrecMap
      pure $ insertOperator (QualifiedOperator (Just modName) OperatorType op) prec pm
    ExportModule _ (Name { name: exportModName }) -> do
      prec <- Map.lookup (Just exportModName) remappedPrecMap
      pure $ Map.insertWith Map.union (Just modName) prec pm
    _ ->
      Nothing

  goDecl pm = case _ of
    DeclFixity { prec: Tuple _ prec, operator } ->
      case operator of
        FixityValue _ _ (Name { name: op }) ->
          insertOperator (QualifiedOperator (Just modName) OperatorValue op) prec pm
        FixityType _ _ _ (Name { name: op }) ->
          insertOperator (QualifiedOperator (Just modName) OperatorType op) prec pm
    _ ->
      pm

getModuleName :: forall e. Module e -> ModuleName
getModuleName (Module { header: ModuleHeader { name: Name { name } } }) = name

tokenStreamToArray :: TokenStream -> Either ParseError (Array Token)
tokenStreamToArray = go []
  where
  go acc = TokenStream.step >>> case _ of
    TokenEOF _ _ ->
      Right acc
    TokenError _ err _ _ ->
      Left err
    TokenCons tok _ next _ ->
      go (Array.snoc acc tok.value) next
