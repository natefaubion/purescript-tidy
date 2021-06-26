module PureScript.CST.Tidy.Hang
  ( HangingDoc
  , hang
  , hangWithIndent
  , hangBreak
  , hangApp
  , hangOp
  , hangConcatApp
  , toFormatDoc
  ) where

import Prelude

import Data.Array (foldr)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (maybe)
import Dodo (Doc)
import Dodo as Dodo
import PureScript.CST.Tidy.Doc (ForceBreak(..), FormatDoc(..), flexGroup)

data HangingDoc a
  = HangBreak (FormatDoc a)
  | HangOp (Doc a -> Doc a) (FormatDoc a) (HangingDoc a)
  | HangApp (Doc a -> Doc a) (HangingDoc a) (NonEmptyArray (HangingDoc a))

hang :: forall a. FormatDoc a -> HangingDoc a -> HangingDoc a
hang a = HangApp Dodo.indent (hangBreak a) <<< pure

hangWithIndent :: forall a. (Doc a -> Doc a) -> HangingDoc a -> Array (HangingDoc a) -> HangingDoc a
hangWithIndent ind a = maybe a (HangApp ind a) <<< NonEmptyArray.fromArray

hangBreak :: forall a. FormatDoc a -> HangingDoc a
hangBreak = HangBreak <<< flexGroup

hangApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangApp = HangApp Dodo.indent

hangOp :: forall a. FormatDoc a -> HangingDoc a -> HangingDoc a
hangOp = HangOp Dodo.indent <<< flexGroup

hangConcatApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangConcatApp a b = case a of
  HangApp ind head tail -> HangApp ind head (tail <> b)
  _ -> HangApp Dodo.indent a b

data HangStk a
  = HangStkRoot
  | HangStkApp (HangStk a) (Doc a -> Doc a) (HangingDoc a) (Array (HangingDoc a))
  | HangStkOp (HangStk a) (Doc a -> Doc a) (FormatDoc a)

data FormatDoc' a = FormatDoc' ForceBreak Int Boolean (Doc a) ForceBreak

data HangDoc a
  = HangGroup (Doc a) (Doc a) (FormatDoc' a)
  | HangEmpty

toFormatDoc :: forall a. HangingDoc a -> FormatDoc a
toFormatDoc = unHangDoc <<< followLast HangStkRoot
  where
  unHangDoc :: HangDoc a -> FormatDoc a
  unHangDoc = case _ of
    HangEmpty ->
      FormatEmpty
    HangGroup _ _ (FormatDoc' a b c d e) ->
      FormatDoc a b c d e

  followLast :: HangStk a -> HangingDoc a -> HangDoc a
  followLast stk = case _ of
    HangBreak doc ->
      unwindStack (formatBreak doc) stk
    HangOp ind doc docs ->
      followLast (HangStkOp stk ind doc) docs
    HangApp ind doc docs ->
      followLast (HangStkApp stk ind doc init) last
      where
      { init, last } =
        NonEmptyArray.unsnoc docs

  unwindStack :: HangDoc a -> HangStk a -> HangDoc a
  unwindStack accum = case _ of
    HangStkApp stk ind head tail -> do
      let
        args = foldr (formatArg <<< toFormatDoc) accum tail
        next = case head of
          HangBreak _ ->
            formatApp ind (toFormatDoc head) args
          _ ->
            formatMultilineApp (toFormatDoc head) args
      unwindStack next stk
    HangStkOp stk ind op ->
      unwindStack (formatOp ind op accum) stk
    HangStkRoot ->
      accum

  formatMultilineApp :: FormatDoc a -> HangDoc a -> HangDoc a
  formatMultilineApp = case _, _ of
    FormatEmpty, b ->
      b
    a, HangEmpty ->
      formatBreak a
    FormatDoc fl1 n1 m1 doc1 _, HangGroup _ docBreak (FormatDoc' _ _ _ _ fr2) ->
      HangGroup
        (forceBreaks n1 <> doc1 <> docBreak)
        (forceBreaks n1 <> doc1 <> docBreak)
        (FormatDoc' fl1 n1 m1 (Dodo.flexGroup doc1 <> docBreak) fr2)

  formatApp :: (Doc a -> Doc a) -> FormatDoc a -> HangDoc a -> HangDoc a
  formatApp ind = case _, _ of
    FormatEmpty, b ->
      b
    a, HangEmpty ->
      formatBreak a
    FormatDoc fl1 n1 m1 doc1 _, HangGroup docGroup _ (FormatDoc' _ _ _ _ fr2) -> do
      let docIndent = ind docGroup
      HangGroup
        (Dodo.flexSelect
          (withBreaks fl1 n1 doc1 (Dodo.spaceBreak <> doc1))
          docGroup
          docIndent)
        (forceBreaks n1 <> doc1 <> docIndent)
        (FormatDoc' fl1 n1 m1 (Dodo.flexGroup doc1 <> docIndent) fr2)

  formatArg :: FormatDoc a -> HangDoc a -> HangDoc a
  formatArg = case _, _ of
    FormatEmpty, b ->
      b
    a, HangEmpty ->
      formatBreak a
    FormatDoc fl1 n1 m1 doc1 _, HangGroup docGroup docBreak (FormatDoc' _ _ _ _ fr2) ->
      HangGroup
        (Dodo.flexSelect
          (withBreaks fl1 n1 doc1 (Dodo.spaceBreak <> doc1))
          docGroup
          docBreak)
        (forceBreaks n1 <> doc1 <> docBreak)
        (FormatDoc' fl1 n1 m1
          (Dodo.flexSelect
            (withBreaks fl1 n1 doc1 doc1)
            docGroup
            docBreak)
          fr2)

  formatOp :: (Doc a -> Doc a) -> FormatDoc a -> HangDoc a -> HangDoc a
  formatOp = formatApp -- Currently the same as an app, but that could change.

  formatBreak :: FormatDoc a -> HangDoc a
  formatBreak = case _ of
    FormatEmpty ->
      HangEmpty
    FormatDoc fl n m doc fr ->
      HangGroup
        (withBreaks fl n doc (Dodo.flexGroup (Dodo.spaceBreak <> doc)))
        (forceBreaks n <> doc)
        (FormatDoc' fl n m doc fr)

forceBreaks :: forall a. Int -> Doc a
forceBreaks n
  | n >= 2 = Dodo.break <> Dodo.break
  | otherwise = Dodo.break

withBreaks :: forall a. ForceBreak -> Int -> Doc a -> Doc a -> Doc a
withBreaks fl n doc1 doc2
  | n > 0 || fl == ForceBreak = forceBreaks n <> doc1
  | otherwise = doc2
