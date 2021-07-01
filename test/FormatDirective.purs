module Test.FormatDirective
  ( directiveRegex
  , SnapshotModule
  , parseDirectivesFromModule
  , FormatDirective
  , defaultFormat
  , formatDirective
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as Number
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Dodo (PrintOptions, twoSpaces)
import PureScript.CST.Tidy (class FormatError, FormatOptions, TypeArrowOption(..), UnicodeOption(..), defaultFormatOptions)
import PureScript.CST.Types (Comment(..), LineFeed, Module(..), ModuleHeader(..))
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (regex)
import Text.Parsing.StringParser.CodeUnits (anyChar, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (manyTill, option)

directiveRegex :: Regex
directiveRegex = unsafeRegex "\\n-- @format .+\\n" global

type SnapshotModule e a =
  { module :: Module e
  , directives :: Map String (FormatDirective e a)
  }

-- | Parse format directives from a module. The resulting module will have all
-- | format directives stripped out.
parseDirectivesFromModule :: forall e a. FormatError e => Module e -> SnapshotModule e a
parseDirectivesFromModule (Module { header: ModuleHeader header, body }) =
  { module: moduleNoDirectives
  , directives
  }
  where
  directives :: Map String (FormatDirective e a)
  directives =
    Map.fromFoldable
      $ Array.mapMaybe parseOptionFromComment header.keyword.leadingComments

  -- Format directives are not considered part of the source code, so we
  -- strip them out of the input module.
  moduleNoDirectives :: Module e
  moduleNoDirectives = do
    let
      headerNoDirectives = ModuleHeader $ header
        { keyword
            { leadingComments = do
                let filterFn = isNothing <<< parseOptionFromComment
                Array.filter filterFn header.keyword.leadingComments
            }
        }

    Module { header: headerNoDirectives, body }

  parseOptionFromComment :: Comment LineFeed -> Maybe (Tuple String (FormatDirective e a))
  parseOptionFromComment = case _ of
    Comment original -> case runParser formatDirective original of
      Left _ -> Nothing
      Right directive -> pure $ Tuple original directive
    _ -> Nothing

type FormatDirective e a =
  { printOptions :: PrintOptions
  , formatOptions :: FormatOptions e a
  }

defaultFormat :: forall e a. FormatError e => FormatDirective e a
defaultFormat =
  { printOptions: defaultPrintOptions
  , formatOptions: defaultFormatOptions
  }

defaultPrintOptions :: PrintOptions
defaultPrintOptions = twoSpaces { pageWidth = top :: Int }

formatDirective :: forall e a. FormatError e => Parser (FormatDirective e a)
formatDirective = do
  _ <- manyTill anyChar (string "@format")
  print <- try printOptions
  format <- try formatOptions
  pure
    { printOptions: print
    , formatOptions: format
    }

printOptions :: Parser PrintOptions
printOptions = do
  pageWidth <- try $ option defaultPrintOptions.pageWidth do
    keyValue "width" (regex """[0-9]+""") Int.fromString

  ribbonRatio <- try $ option defaultPrintOptions.ribbonRatio do
    let
      fromString = Number.fromString >=> \n ->
        if n >= 0.0 && n <= 1.0 then Just n else Nothing

    keyValue "ribbon" (regex """[0-9]+\.?[0-9]+""") fromString

  indentWidth <- try $ option defaultPrintOptions.indentWidth do
    keyValue "indent" (regex """[0-9]+""") Int.fromString

  pure
    { pageWidth
    , ribbonRatio
    , indentWidth
    , indentUnit: defaultPrintOptions.indentUnit
    }

formatOptions :: forall e a. FormatError e => Parser (FormatOptions e a)
formatOptions = do
  let default = defaultFormat.formatOptions

  typeArrowPlacement <- try $ option default.typeArrowPlacement do
    TypeArrowFirst <$ string "arrow-first"
      <|> TypeArrowLast <$ string "arrow-last"

  unicode <- try $ option default.unicode do
    UnicodeSource <$ string "unicode-source"
      <|> UnicodeAlways <$ string "unicode-always"
      <|> UnicodeNever <$ string "unicode-never"

  pure
    { typeArrowPlacement
    , unicode
    , formatError: default.formatError
    , operators: default.operators
    }

keyValue :: forall a. String -> Parser String -> (String -> Maybe a) -> Parser a
keyValue key parser k = do
  _ <- string key *> skipSpaces
  b <- parser
  case k b of
    Nothing -> fail $ "Could not parse key '" <> key <> "'"
    Just a -> pure a
