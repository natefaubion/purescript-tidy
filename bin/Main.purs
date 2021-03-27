module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, findMap, fold, foldMap, foldl, foldr, for_)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..), snd, uncurry)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobsCwd)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CST (RecoveredParserResult(..), parseModule, toRecovered)
import PureScript.CST.Errors (ParseError, printParseError)
import PureScript.CST.Lexer as Lexer
import PureScript.CST.ModuleGraph (ModuleSort(..), sortModules)
import PureScript.CST.Tidy (UnicodeOption(..), defaultFormatOptions, formatModule, toDoc)
import PureScript.CST.Tidy.Precedence (OperatorNamespace(..), Precedence, PrecedenceMap, QualifiedOperator(..), insertOperator, lookupOperator, remapOperators)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Declaration(..), Export(..), FixityOp(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator(..), Separated(..), Token(..), Wrapped(..))

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 1 <$> liftEffect Process.argv
  case Array.uncons args of
    Just { head, tail } | head == "gen-operator-table" ->
      operatorTableCommand tail
    _ ->
      formatCommand args

formatCommand :: Array String -> Aff Unit
formatCommand args = do
  contents <- readStdin
  let
    unicode
      | elem "--unicode-never" args = UnicodeNever
      | elem "--unicode-always" args = UnicodeAlways
      | otherwise = UnicodeSource

    pageWidth = fromMaybe top $ findMap (String.stripPrefix (Pattern "--width=") >=> Int.fromString) args
    ribbonRatio = fromMaybe 1.0 $ findMap (String.stripPrefix (Pattern "--ribbon=") >=> Number.fromString) args
    indentWidth = fromMaybe 2 $ findMap (String.stripPrefix (Pattern "--indent=") >=> Int.fromString) args
    indentUnit = power " " indentWidth
    print = Dodo.print Dodo.plainText { pageWidth, ribbonRatio, indentWidth, indentUnit }

  operators <-
    case findMap (String.stripPrefix (Pattern "--operators=")) args of
      Nothing -> pure Map.empty
      Just path -> do
        table <- liftEffect <<< Buffer.toString UTF8 =<< FS.readFile path
        pure $ parseOperatorTable $ String.split (Pattern "\n") table

  case parseModule contents of
    ParseSucceeded ok -> do
      let opts = defaultFormatOptions { operators = remapOperators operators ok, unicode = unicode }
      Console.log $ print $ toDoc $ formatModule opts ok
    ParseSucceededWithErrors ok _ -> do
      let opts = defaultFormatOptions { operators = remapOperators operators ok, unicode = unicode  }
      Console.log $ print $ toDoc $ formatModule opts ok
    ParseFailed err ->
      Console.log $ printParseError err.error

readStdin :: Aff String
readStdin = makeAff \k -> do
  contents <- Ref.new ""
  Stream.onData Process.stdin \buff -> do
    chunk <- Buffer.toString UTF8 buff
    void $ Ref.modify (_ <> chunk) contents
  Stream.onEnd Process.stdin do
    k <<< Right =<< Ref.read contents
  pure mempty

operatorTableCommand :: Array String -> Aff Unit
operatorTableCommand globs = do
  sourcePaths <- expandGlobsCwd globs
  modules <- sourcePaths # Array.fromFoldable # parTraverse \path -> do
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
