module Tidy.Token
  ( UnicodeOption(..)
  , printToken
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Types (ModuleName(..), SourceStyle(..), Token(..))

data UnicodeOption
  = UnicodeSource
  | UnicodeAlways
  | UnicodeNever

derive instance eqUnicodeOption :: Eq UnicodeOption

printUnicode :: String -> String -> SourceStyle -> UnicodeOption -> String
printUnicode ascii uni style = case _ of
  UnicodeNever -> ascii
  UnicodeAlways -> uni
  UnicodeSource ->
    case style of
      ASCII -> ascii
      Unicode -> uni

printToken :: UnicodeOption -> Token -> String
printToken option = case _ of
  TokLeftParen ->
    "("
  TokRightParen ->
    ")"
  TokLeftBrace ->
    "{"
  TokRightBrace ->
    "}"
  TokLeftSquare ->
    "["
  TokRightSquare ->
    "]"
  TokLeftArrow style ->
    printUnicode "<-" "←" style option
  TokRightArrow style ->
    printUnicode "->" "→" style option
  TokRightFatArrow style ->
    printUnicode "=>" "⇒" style option
  TokDoubleColon style ->
    printUnicode "::" "∷" style option
  TokForall style ->
    printUnicode "forall" "∀" style option
  TokEquals ->
    "="
  TokPipe ->
    "|"
  TokTick ->
    "`"
  TokDot ->
    "."
  TokComma ->
    ","
  TokUnderscore ->
    "_"
  TokBackslash ->
    "\\"
  TokAt ->
    "@"
  TokLowerName moduleName name ->
    printQualified moduleName name
  TokUpperName moduleName name ->
    printQualified moduleName name
  TokOperator moduleName name ->
    printQualified moduleName name
  TokSymbolName moduleName name ->
    printQualified moduleName ("(" <> name <> ")")
  TokSymbolArrow style ->
    printUnicode "(->)" "(→)" style option
  TokHole name ->
    "?" <> name
  TokChar raw _ ->
    "'" <> raw <> "'"
  TokString raw _ ->
    "\"" <> raw <> "\""
  TokRawString raw ->
    "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _ ->
    raw
  TokNumber raw _ ->
    raw
  TokLayoutStart _ ->
    ""
  TokLayoutSep _ ->
    ""
  TokLayoutEnd _ ->
    ""

printQualified :: Maybe ModuleName -> String -> String
printQualified moduleName name = case moduleName of
  Nothing -> name
  Just (ModuleName mn) -> mn <> "." <> name
