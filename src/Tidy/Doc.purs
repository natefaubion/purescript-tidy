module Tidy.Doc
  ( FormatDoc(..)
  , LeadingComment(..)
  , TrailingComment(..)
  , ForceBreak(..)
  , text
  , leadingLineComment
  , trailingLineComment
  , leadingBlockComment
  , trailingBlockComment
  , anchor
  , flatten
  , flattenMax
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
  , flexDoubleBreak
  , flexGroup
  , fromDoc
  , toDoc
  , mapDoc
  , breakDoc
  , breaks
  , joinWithMap
  , joinWith
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Internal (LocalOptions)
import Tidy.Util (splitLines)

data ForceBreak = ForceNone | ForceSpace | ForceBreak

derive instance eqForceBreak :: Eq ForceBreak
derive instance ordForceBreak :: Ord ForceBreak

newtype LeadingComment a = LeadingComment
  { doc :: Doc a
  , left :: ForceBreak
  , lines :: Int
  , multiline :: Boolean
  , right :: ForceBreak
  }

newtype TrailingComment a = TrailingComment
  { doc :: Doc a
  , left :: ForceBreak
  , multiline :: Boolean
  , right :: ForceBreak
  }

instance Semigroup (LeadingComment a) where
  append (LeadingComment c1) (LeadingComment c2) = do
    let br = max c1.right c2.left
    if c2.lines > 0 || br == ForceBreak then
      LeadingComment c1
        { doc = c1.doc <> breaks ForceBreak c2.lines <> c2.doc
        , multiline = true
        , right = c2.right
        }
    else
      LeadingComment c1
        { doc = c1.doc <> breakDoc br <> c2.doc
        , multiline = c1.multiline || c2.multiline
        , right = c2.right
        }

instance Monoid (LeadingComment a) where
  mempty = LeadingComment
    { doc: mempty
    , left: ForceNone
    , lines: 0
    , multiline: false
    , right: ForceNone
    }

instance Semigroup (TrailingComment a) where
  append (TrailingComment c1) (TrailingComment c2) =
    TrailingComment c1
      { doc = c1.doc <> breakDoc (max c1.right c2.left) <> c2.doc
      , multiline = c1.multiline || c2.multiline
      , right = c2.right
      }

instance Monoid (TrailingComment a) where
  mempty = TrailingComment
    { doc: mempty
    , left: ForceNone
    , multiline: false
    , right: ForceNone
    }

newtype FormatDoc a = FormatDoc
  { doc :: Doc a
  , isEmpty :: Boolean
  , leading :: LeadingComment a
  , multiline :: Boolean
  , trailing :: TrailingComment a
  }

type FormatDocOperator a = FormatDoc a -> FormatDoc a -> FormatDoc a

instance semigroupFormatDoc :: Semigroup (FormatDoc a) where
  append = joinDoc (force identity)

instance monoidFormatDoc :: Monoid (FormatDoc a) where
  mempty = FormatDoc
    { doc: mempty
    , leading: mempty
    , isEmpty: true
    , multiline: false
    , trailing: mempty
    }

fromDoc :: forall a. Doc a -> FormatDoc a
fromDoc doc
  | Dodo.isEmpty doc = mempty
  | otherwise = FormatDoc
      { doc
      , leading: mempty
      , isEmpty: false
      , multiline: false
      , trailing: mempty
      }

text :: forall a. String -> FormatDoc a
text = fromDoc <<< Dodo.text

leadingLineComment :: forall a. String -> FormatDoc a -> FormatDoc a
leadingLineComment str (FormatDoc doc) = FormatDoc doc
  { leading = comm <> doc.leading
  , isEmpty = false
  }
  where
  comm = LeadingComment
    { doc: Dodo.text str
    , left: ForceBreak
    , lines: 0
    , multiline: false
    , right: ForceBreak
    }

trailingLineComment :: forall a. String -> FormatDoc a -> FormatDoc a
trailingLineComment str (FormatDoc doc) = FormatDoc doc
  { trailing = doc.trailing <> comm
  , isEmpty = false
  }
  where
  comm = TrailingComment
    { doc: Dodo.text str
    , left: ForceSpace
    , multiline: false
    , right: ForceBreak
    }

formatBlockComment :: forall a. String -> Tuple Boolean (Doc a)
formatBlockComment = splitLines >>> Array.uncons >>> case _ of
  Nothing ->
    Tuple false mempty
  Just { head, tail } ->
    case prefixSpaces of
      Nothing ->
        Tuple false (Dodo.text head)
      Just ind ->
        Tuple true $ Dodo.withPosition \pos -> do
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
    where
    prefixSpaces =
      tail
        # Array.mapMaybe
            ( \str -> do
                let spaces = SCU.length $ String.takeWhile (eq (String.codePointFromChar ' ')) str
                guard (spaces < SCU.length str) $> spaces
            )
        # Array.sort
        # Array.head

leadingBlockComment :: forall a. String -> FormatDoc a -> FormatDoc a
leadingBlockComment str (FormatDoc doc) = FormatDoc doc
  { leading = comm <> doc.leading
  , isEmpty = false
  }
  where
  Tuple multi commDoc =
    formatBlockComment str

  comm = LeadingComment
    { doc: commDoc
    , left: ForceSpace
    , lines: 0
    , multiline: multi
    , right: ForceSpace
    }

trailingBlockComment :: forall a. String -> FormatDoc a -> FormatDoc a
trailingBlockComment str (FormatDoc doc) = FormatDoc doc
  { trailing = doc.trailing <> comm
  , isEmpty = false
  }
  where
  Tuple multi commDoc =
    formatBlockComment str

  comm = TrailingComment
    { doc: commDoc
    , left: ForceSpace
    , multiline: multi
    , right: ForceSpace
    }

anchor :: forall a. FormatDoc a -> FormatDoc a
anchor (FormatDoc doc) = case doc.leading of
  LeadingComment comm | comm.lines > 0 ->
    FormatDoc doc
      { leading = LeadingComment comm { lines = 0 }
      , multiline = true
      }
  _ ->
    FormatDoc doc

flatten :: forall a. FormatDoc a -> FormatDoc a
flatten = flattenMax 0

flattenMax :: forall a. Int -> FormatDoc a -> FormatDoc a
flattenMax n (FormatDoc doc) = case doc.leading of
  LeadingComment comm ->
    FormatDoc doc
      { leading = LeadingComment comm { lines = min comm.lines n }
      }

flexGroup :: forall a. FormatDoc a -> FormatDoc a
flexGroup (FormatDoc doc)
  | doc.multiline = FormatDoc doc
  | otherwise = FormatDoc doc { doc = Dodo.flexGroup doc.doc }

indent :: forall a. FormatDoc a -> FormatDoc a
indent = mapDocs Dodo.indent

align :: forall a. Int -> FormatDoc a -> FormatDoc a
align = mapDocs <<< Dodo.align

alignCurrentColumn :: forall a. FormatDoc a -> FormatDoc a
alignCurrentColumn = mapDocs Dodo.alignCurrentColumn

locally :: forall a. (LocalOptions -> LocalOptions) -> FormatDoc a -> FormatDoc a
locally k (FormatDoc doc) = FormatDoc doc { doc = Dodo.locally k doc.doc }

sourceBreak :: forall a. Int -> FormatDoc a -> FormatDoc a
sourceBreak n (FormatDoc doc) = do
  let LeadingComment comm = doc.leading
  FormatDoc doc
    { isEmpty = false
    , leading = LeadingComment comm { lines = comm.lines + n }
    }

forceMinSourceBreaks :: forall a. Int -> FormatDoc a -> FormatDoc a
forceMinSourceBreaks n (FormatDoc doc)
  | doc.isEmpty = FormatDoc doc
  | otherwise = do
      let LeadingComment comm = doc.leading
      FormatDoc doc
        { leading = LeadingComment comm { lines = max comm.lines n }
        }

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

-- | Warning: This is not an associative join operation, and *requires*
-- | right associativity. You will always get double breaks when used
-- | with left associativity.
flexDoubleBreak :: forall a. FormatDocOperator a
flexDoubleBreak (FormatDoc doc1) (FormatDoc doc2)
  | doc1.isEmpty = FormatDoc doc2
  | doc2.isEmpty = FormatDoc doc1
  | otherwise = do
      let TrailingComment comm1 = doc1.trailing
      let LeadingComment comm2 = doc2.leading
      let docLeft = doc1.doc <> breakDoc comm1.left <> comm1.doc
      let docRight = comm2.doc <> breakDoc comm2.right <> doc2.doc
      if comm2.lines >= 2 || doc1.multiline then
        FormatDoc doc1
          { doc = docLeft <> Dodo.break <> Dodo.break <> docRight
          , multiline = true
          , trailing = doc2.trailing
          }
      else
        FormatDoc doc1
          { doc = Dodo.flexSelect docLeft mempty Dodo.break <> Dodo.break <> docRight
          , multiline = true
          , trailing = doc2.trailing
          }

isEmpty :: forall a. FormatDoc a -> Boolean
isEmpty (FormatDoc doc) = doc.isEmpty

breakDoc :: forall a. ForceBreak -> Doc a
breakDoc = case _ of
  ForceBreak -> Dodo.break
  ForceSpace -> Dodo.space
  ForceNone -> mempty

breaks :: forall a. ForceBreak -> Int -> Doc a
breaks fl n
  | n >= 2 = Dodo.break <> Dodo.break
  | n == 1 = Dodo.break
  | otherwise = breakDoc fl

joinDoc :: forall a. (ForceBreak -> Boolean -> Doc a -> Tuple Boolean (Doc a)) -> FormatDocOperator a
joinDoc spaceFn (FormatDoc doc1) (FormatDoc doc2)
  | doc1.isEmpty = FormatDoc doc2
  | doc2.isEmpty = FormatDoc doc1
  | otherwise = do
      let TrailingComment comm1 = doc1.trailing
      let LeadingComment comm2 = doc2.leading
      let docLeft = doc1.doc <> breakDoc comm1.left <> comm1.doc
      let docRight = comm2.doc <> breakDoc comm2.right <> doc2.doc
      if comm2.lines > 0 then
        FormatDoc doc1
          { doc = docLeft <> breaks ForceBreak comm2.lines <> docRight
          , multiline = true
          , trailing= doc2.trailing
          }
      else do
        let Tuple m3 doc3 = spaceFn (max comm1.right comm2.left) (comm2.multiline || doc2.multiline) docRight
        FormatDoc doc1
          { doc = docLeft <> doc3
          , multiline = comm1.multiline || doc1.multiline || m3
          , trailing= doc2.trailing
          }

force :: forall a. (Doc a -> Doc a) -> (ForceBreak -> Boolean -> Doc a -> Tuple Boolean (Doc a))
force k f m doc = case f of
  ForceBreak ->
    Tuple true (Dodo.break <> doc)
  _ ->
    Tuple m (k doc)

mapDoc :: forall a. (Doc a -> Doc a) -> FormatDoc a -> FormatDoc a
mapDoc k (FormatDoc doc)
  | doc.isEmpty = FormatDoc doc
  | otherwise = FormatDoc doc { doc = k doc.doc }

mapDocs :: forall a. (Doc a -> Doc a) -> FormatDoc a -> FormatDoc a
mapDocs k (FormatDoc doc)
  | doc.isEmpty = FormatDoc doc
  | otherwise = do
      let LeadingComment comm1 = doc.leading
      let TrailingComment comm2 = doc.trailing
      FormatDoc doc
        { doc = k doc.doc
        , leading = LeadingComment comm1 { doc = k comm1.doc }
        , trailing = TrailingComment comm2 { doc = k comm2.doc }
        }

toDoc :: forall a. FormatDoc a -> Doc a
toDoc (FormatDoc doc)
  | doc.isEmpty = mempty
  | otherwise = do
      let LeadingComment comm1 = doc.leading
      let TrailingComment comm2 = doc.trailing
      comm1.doc
        <> breakDoc comm1.right
        <> doc.doc
        <> breakDoc comm2.left
        <> comm2.doc

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
