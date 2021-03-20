module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (findMap)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Tidy (defaultFormatConf, formatModule)
import PureScript.CST.Tidy.Doc (toDoc)
import PureScript.CST.Tidy.Precedence (OperatorNamespace(..), PrecedenceMap, remapOperators)
import PureScript.CST.Types (ModuleName(..), Operator(..))

defaultOperators :: PrecedenceMap
defaultOperators = Map.fromFoldable
  [ Tuple (Just (ModuleName "Prelude")) $ Map.fromFoldable
      [ Tuple (Tuple OperatorValue (Operator "<|>")) 4
      , Tuple (Tuple OperatorValue (Operator "<$>")) 5
      , Tuple (Tuple OperatorValue (Operator "<*>")) 5
      ]
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 1 <$> liftEffect Process.argv
  contents <- readStdin
  let
    pageWidth = fromMaybe top $ findMap (String.stripPrefix (Pattern "--width=") >=> Int.fromString) args
    ribbonRatio = fromMaybe 1.0 $ findMap (String.stripPrefix (Pattern "--ribbon=") >=> Number.fromString) args
    indentWidth = fromMaybe 2 $ findMap (String.stripPrefix (Pattern "--indent=") >=> Int.fromString) args
    indentUnit = power " " indentWidth
    print = Dodo.print Dodo.plainText { pageWidth, ribbonRatio, indentWidth, indentUnit }
  case parseModule contents of
    ParseSucceeded ok -> do
      let conf = defaultFormatConf { operators = remapOperators defaultOperators ok }
      Console.log $ print $ toDoc $ formatModule conf ok
    ParseSucceededWithErrors ok _ -> do
      let conf = defaultFormatConf { operators = remapOperators defaultOperators ok }
      Console.log $ print $ toDoc $ formatModule conf ok
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
