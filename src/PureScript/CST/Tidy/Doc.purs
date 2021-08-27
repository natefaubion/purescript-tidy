module PureScript.CST.Tidy.Doc
  ( FormatDoc(..)
  , ForceBreak(..)
  , text
  , leadingLineComment
  , trailingLineComment
  , blockComment
  , anchor
  , flatten
  , forceBreak
  , indent
  , align
  , alignCurrentColumn
  , locally
  , break
  , softBreak
  , spaceBreak
  , sourceBreak
  , forceMinSourceBreaks
  , space
  , softSpace
  , flexSpaceBreak
  , flexSoftSpace
  , flexSoftBreak
  , flexGroup
  , fromDoc
  , toDoc
  , joinWithMap
  , joinWith
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Internal (LocalOptions)
import PureScript.CST.Tidy.Util (splitLines)

data ForceBreak
  = ForceNone
  | ForceSpace
  | ForceBreak

derive instance eqForceBreak :: Eq ForceBreak
derive instance ordForceBreak :: Ord ForceBreak

data FormatDoc a = FormatEmpty | FormatDoc ForceBreak Int Boolean (Doc a) ForceBreak

type FormatDocOperator a = FormatDoc a -> FormatDoc a -> FormatDoc a

instance semigroupFormatDoc :: Semigroup (FormatDoc a) where
  append = joinDoc (force identity)

instance monoidFormatDoc :: Monoid (FormatDoc a) where
  mempty = FormatEmpty

fromDoc :: forall a. Doc a -> FormatDoc a
fromDoc doc = FormatDoc ForceNone 0 false doc ForceNone

text :: forall a. String -> FormatDoc a
text str = FormatDoc ForceNone 0 false (Dodo.text str) ForceNone

leadingLineComment :: forall a. String -> FormatDoc a
leadingLineComment str = FormatDoc ForceBreak 0 false (Dodo.text str) ForceBreak

trailingLineComment :: forall a. String -> FormatDoc a
trailingLineComment str = FormatDoc ForceSpace 0 false (Dodo.text str) ForceBreak

blockComment :: forall a. String -> FormatDoc a
blockComment = splitLines >>> Array.uncons >>> foldMap \{ head, tail } -> do
  let
    prefixSpaces =
      tail
        # Array.mapMaybe
          ( \str -> do
              let spaces = SCU.length $ String.takeWhile (eq (String.codePointFromChar ' ')) str
              guard (spaces < SCU.length str) $> spaces
          )
        # Array.sort
        # Array.head
  case prefixSpaces of
    Nothing ->
      FormatDoc ForceSpace 0 false (Dodo.text head) ForceSpace
    Just ind ->
      FormatDoc ForceSpace 0 true commentDoc ForceSpace
      where
      commentDoc = Dodo.withPosition \pos -> do
        let newIndent = if ind < pos.indent then 0 else ind
        let spaces = power " " newIndent
        let tailDocs = map (\str -> Dodo.text $ fromMaybe str $ String.stripPrefix (String.Pattern spaces) str) tail
        Dodo.lines
          [ Dodo.text head
          , Dodo.locally
              ( \prev ->
                  if newIndent < prev.indent then
                    prev
                      { indentSpaces = spaces
                      , indent = newIndent
                      }
                  else prev
              )
              (intercalate Dodo.break tailDocs)
          ]

anchor :: forall a. FormatDoc a -> FormatDoc a
anchor = case _ of
  FormatEmpty ->
    FormatEmpty
  FormatDoc fl n m doc fr ->
    FormatDoc fl 0 (if n > 0 then true else m) doc fr

flatten :: forall a. FormatDoc a -> FormatDoc a
flatten = case _ of
  FormatEmpty ->
    FormatEmpty
  FormatDoc fl _ m doc fr ->
    FormatDoc fl 0 m doc fr

forceBreak :: forall a. FormatDoc a -> FormatDoc a
forceBreak = case _ of
  FormatEmpty ->
    FormatEmpty
  FormatDoc _ n m doc fr ->
    FormatDoc ForceBreak n m doc fr

flexGroup :: forall a. FormatDoc a -> FormatDoc a
flexGroup = case _ of
  FormatEmpty ->
    FormatEmpty
  a@(FormatDoc fl n m doc fr)
    | not m -> FormatDoc fl n false (Dodo.flexGroup doc) fr
    | otherwise -> a

indent :: forall a. FormatDoc a -> FormatDoc a
indent = mapDoc Dodo.indent

align :: forall a. Int -> FormatDoc a -> FormatDoc a
align = mapDoc <<< Dodo.align

alignCurrentColumn :: forall a. FormatDoc a -> FormatDoc a
alignCurrentColumn = mapDoc Dodo.alignCurrentColumn

locally :: forall a. (LocalOptions -> LocalOptions) -> FormatDoc a -> FormatDoc a
locally = mapDoc <<< Dodo.locally

sourceBreak :: forall a. Int -> FormatDoc a -> FormatDoc a
sourceBreak m = case _ of
  FormatEmpty
    | m <= 0 -> FormatEmpty
    | otherwise -> FormatDoc ForceNone m false mempty ForceNone
  FormatDoc fl n multi doc fr ->
    FormatDoc fl (m + n) multi doc fr

forceMinSourceBreaks :: forall a. Int -> FormatDoc a -> FormatDoc a
forceMinSourceBreaks m = case _ of
  FormatEmpty -> FormatEmpty
  FormatDoc fl n multi doc fr ->
    FormatDoc fl (max m n) multi doc fr

space :: forall a. FormatDocOperator a
space = joinDoc (force (append Dodo.space))

break :: forall a. FormatDocOperator a
break = joinDoc (force (append Dodo.break))

spaceBreak :: forall a. FormatDocOperator a
spaceBreak = joinDoc (force (append Dodo.spaceBreak))

flexSpaceBreak :: forall a. FormatDocOperator a
flexSpaceBreak = joinDoc \f m doc -> case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  _ ->
    if m then
      Tuple true (Dodo.spaceBreak <> doc)
    else
      Tuple false (Dodo.flexGroup (Dodo.spaceBreak <> doc))

flexSoftSpace :: forall a. FormatDocOperator a
flexSoftSpace = joinDoc \f m doc -> case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  ForceSpace ->
    if m then
      Tuple true (Dodo.space <> doc)
    else
      Tuple false (Dodo.flexGroup (Dodo.space <> doc))
  ForceNone ->
    if m then
      Tuple true (softSpaceDoc <> doc)
    else
      Tuple false (Dodo.flexGroup (softSpaceDoc <> doc))

flexSoftBreak :: forall a. FormatDocOperator a
flexSoftBreak = joinDoc \f m doc -> case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  ForceSpace ->
    if m then
      Tuple true (Dodo.space <> doc)
    else
      Tuple false (Dodo.flexGroup (Dodo.spaceBreak <> doc))
  ForceNone ->
    if m then
      Tuple true (Dodo.break <> doc)
    else
      Tuple false (Dodo.flexGroup (Dodo.softBreak <> doc))

softBreak :: forall a. FormatDocOperator a
softBreak = joinDoc \f m doc -> case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  ForceSpace ->
    Tuple m (Dodo.spaceBreak <> doc)
  ForceNone ->
    Tuple m (Dodo.softBreak <> doc)

softSpace :: forall a. FormatDocOperator a
softSpace = joinDoc \f m doc -> case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  ForceSpace ->
    Tuple m (Dodo.space <> doc)
  ForceNone ->
    Tuple m (softSpaceDoc <> doc)

isEmpty :: forall a. FormatDoc a -> Boolean
isEmpty = case _ of
  FormatEmpty -> true
  _ -> false

joinDoc :: forall a. (ForceBreak -> Boolean -> Doc a -> Tuple Boolean (Doc a)) -> FormatDocOperator a
joinDoc spc = case _, _ of
  FormatEmpty, b -> b
  a, FormatEmpty -> a
  a@(FormatDoc fl1 n1 m1 doc1 fr1), b@(FormatDoc fl2 n2 m2 doc2 fr2)
    | isEmpty a -> b
    | isEmpty b -> a
    | n2 == 1 ->
        FormatDoc fl1 n1 true (doc1 <> Dodo.break <> doc2) fr2
    | n2 >= 2 ->
        FormatDoc fl1 n1 true (doc1 <> Dodo.break <> Dodo.break <> doc2) fr2
    | otherwise -> do
        let (Tuple m3 doc3) = spc (max fr1 fl2) m2 doc2
        FormatDoc fl1 n1 (m1 || m3) (doc1 <> doc3) fr2

force :: forall a. (Doc a -> Doc a) -> (ForceBreak -> Boolean -> Doc a -> Tuple Boolean (Doc a))
force k f m doc = case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  _ ->
    Tuple m (k doc)

mapDoc :: forall a b. (Doc a -> Doc b) -> FormatDoc a -> FormatDoc b
mapDoc k = case _ of
  FormatEmpty ->
    FormatEmpty
  FormatDoc fl n m doc fr ->
    FormatDoc fl n m (k doc) fr

toDoc :: forall a. FormatDoc a -> Doc a
toDoc = case _ of
  FormatEmpty -> mempty
  FormatDoc _ _ _ doc _ -> doc

joinWithMap
  :: forall f a b
   . Foldable f
  => (FormatDoc a -> FormatDoc a -> FormatDoc a)
  -> (b -> FormatDoc a)
  -> f b
  -> FormatDoc a
joinWithMap op k = foldl go mempty
  where
  go a b
    | isEmpty a = k b
    | otherwise = op a (k b)

joinWith
  :: forall f a
   . Foldable f
  => (FormatDoc a -> FormatDoc a -> FormatDoc a)
  -> f (FormatDoc a)
  -> FormatDoc a
joinWith = flip joinWithMap identity

softSpaceDoc :: forall a. Doc a
softSpaceDoc = Dodo.flexAlt mempty Dodo.space
