module Test.FormatDirective
  ( directiveRegex
  , FormatDirective
  , defaultFormat
  , SnapshotModule
  , parseDirectivesFromModule
  ) where

import Prelude

import ArgParse.Basic (ArgError, parseArgs, printArgError)
import Bin.FormatOptions as Bin
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Dodo (PrintOptions, twoSpaces)
import PureScript.CST.Types (Comment(..), LineFeed, Module(..), ModuleHeader(..))
import Tidy (class FormatError, FormatOptions, defaultFormatOptions)

directiveRegex :: Regex
directiveRegex = unsafeRegex "\\n-- @format .+\\n" global

type FormatDirective e a =
  { printOptions :: PrintOptions
  , formatOptions :: FormatOptions e a
  }

defaultFormat :: forall e a. FormatError e => FormatDirective e a
defaultFormat =
  { printOptions: twoSpaces { pageWidth = top :: Int }
  , formatOptions: defaultFormatOptions
  }

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
      strippedComments = stripFormatDirectives header.keyword.leadingComments
      headerNoDirectives = ModuleHeader $ header { keyword { leadingComments = strippedComments } }

    Module { header: headerNoDirectives, body }

  stripFormatDirectives :: Array (Comment LineFeed) -> Array (Comment LineFeed)
  stripFormatDirectives input = case Array.uncons input of
    Nothing ->
      []
    Just { head, tail } -> case parseOptionFromComment head of
      Nothing ->
        Array.cons head (stripFormatDirectives tail)
      Just _ -> case Array.uncons tail of
        Nothing ->
          []
        -- If we strip the comment, we should strip the trailing line as well.
        Just { head: Line _ _, tail: tail' } ->
          stripFormatDirectives tail'
        Just _ ->
          stripFormatDirectives tail

  parseOptionFromComment :: Comment LineFeed -> Maybe (Tuple String (FormatDirective e a))
  parseOptionFromComment = case _ of
    Comment original | String.contains (String.Pattern "@format") original -> do
      let input = String.split (String.Pattern " ") $ String.drop 11 $ String.trim original
      case parseFormatOptions input of
        Left _ -> Nothing
        Right directive -> pure $ Tuple original directive
    _ -> Nothing

  parseFormatOptions :: Array String -> Either String (FormatDirective e a)
  parseFormatOptions = bimap printArgError fromBinFormatOptions <<< parse
    where
    parse :: Array String -> Either ArgError Bin.FormatOptions
    parse = parseArgs "format-directives" "Parse format directives." Bin.formatOptions

  fromBinFormatOptions :: Bin.FormatOptions -> FormatDirective e a
  fromBinFormatOptions opts =
    { printOptions:
        { indentUnit: power " " opts.indent
        , indentWidth: opts.indent
        , pageWidth: fromMaybe top opts.width
        , ribbonRatio: opts.ribbon
        }
    , formatOptions:
        { formatError: default.formatError
        , operators: default.operators
        , unicode: opts.unicode
        , thenPlacement: opts.thenPlacement
        , typeArrowPlacement: opts.typeArrowPlacement
        , importSort: opts.importSort
        , importWrap: opts.importWrap
        }
    }
    where
    default :: FormatOptions e a
    default = defaultFormatOptions
