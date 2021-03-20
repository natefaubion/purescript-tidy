module PureScript.CST.Tidy.Hang
  ( HangingDoc
  , hang
  , hangBreak
  , hangApp
  , hangConcatApp
  , toFormatDoc
  ) where

import Prelude

import Data.Array (foldr)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Dodo (Doc)
import Dodo as Dodo
import PureScript.CST.Tidy.Doc (ForceBreak(..), FormatDoc(..), flexGroup)

data HangingDoc a
  = HangBreak (FormatDoc a)
  | HangApp (HangingDoc a) (NonEmptyArray (HangingDoc a))

hang :: forall a. FormatDoc a -> HangingDoc a -> HangingDoc a
hang a = HangApp (hangBreak a) <<< pure

hangBreak :: forall a. FormatDoc a -> HangingDoc a
hangBreak = HangBreak <<< flexGroup

hangApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangApp = HangApp

hangConcatApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangConcatApp a b = case a of
  HangApp head tail -> HangApp head (tail <> b)
  _ -> HangApp a b

data HangStk a
  = HangRoot
  | HangApps (HangStk a) (HangingDoc a) (Array (HangingDoc a))

data FormatDoc' a = FormatDoc' ForceBreak Int Boolean (Doc a) ForceBreak

data HangDoc a
  = HangGroup (Doc a) (Doc a) (FormatDoc' a)
  | HangEmpty

toFormatDoc :: forall a. HangingDoc a -> FormatDoc a
toFormatDoc = unHangDoc <<< followLast HangRoot
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
    HangApp doc docs ->
      followLast (HangApps stk doc init) last
      where
      { init, last } =
        NonEmptyArray.unsnoc docs

  unwindStack :: HangDoc a -> HangStk a -> HangDoc a
  unwindStack accum = case _ of
    HangApps stk head tail -> do
      let
        args = foldr (formatArg <<< toFormatDoc) accum tail
        next = case head of
          HangBreak _ ->
            formatApp (toFormatDoc head) args
          HangApp _ _ ->
            formatMultilineApp (toFormatDoc head) args
      unwindStack next stk
    HangRoot ->
      accum

  formatMultilineApp :: FormatDoc a -> HangDoc a -> HangDoc a
  formatMultilineApp = case _, _ of
    FormatEmpty, b ->
      b
    a, HangEmpty ->
      formatBreak a
    FormatDoc fl1 n1 m1 doc1 fr1, HangGroup _ docBreak (FormatDoc' _ _ _ _ fr2) ->
      HangGroup
        (forceBreaks n1 <> doc1 <> docBreak)
        (forceBreaks n1 <> doc1 <> docBreak)
        (FormatDoc' fl1 n1 m1 (Dodo.flexGroup doc1 <> docBreak) fr2)

  formatApp :: FormatDoc a -> HangDoc a -> HangDoc a
  formatApp = case _, _ of
    FormatEmpty, b ->
      b
    a, HangEmpty ->
      formatBreak a
    FormatDoc fl1 n1 m1 doc1 fr1, HangGroup docGroup _ (FormatDoc' _ _ _ _ fr2) -> do
      let docIndent = Dodo.indent docGroup
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
    FormatDoc fl1 n1 m1 doc1 fr1, HangGroup docGroup docBreak (FormatDoc' _ _ _ _ fr2) ->
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
