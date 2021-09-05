module Tidy.Hang
  ( HangingDoc
  , HangingOp(..)
  , hang
  , hangWithIndent
  , hangBreak
  , hangApp
  , hangOps
  , hangHead
  , overHangHead
  , hangConcatApp
  , toFormatDoc
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Dodo (Doc)
import Dodo as Dodo
import Partial.Unsafe (unsafeCrashWith)
import Tidy.Doc (ForceBreak(..), FormatDoc(..), align, break, flatten, flexGroup, forceBreak, indent)

data HangingDoc a
  = HangBreak (FormatDoc a)
  | HangOps (FormatDoc a -> FormatDoc a) (HangingDoc a) (NonEmptyArray (HangingOp a))
  | HangApp (FormatDoc a -> FormatDoc a) (HangingDoc a) (NonEmptyArray (HangingDoc a))

data HangingOp a = HangingOp Int (FormatDoc a) (HangingDoc a)

hang :: forall a. FormatDoc a -> HangingDoc a -> HangingDoc a
hang a = HangApp indent (hangBreak a) <<< pure

hangWithIndent :: forall a. (FormatDoc a -> FormatDoc a) -> HangingDoc a -> Array (HangingDoc a) -> HangingDoc a
hangWithIndent ind a = maybe a (HangApp ind a) <<< NonEmptyArray.fromArray

hangBreak :: forall a. FormatDoc a -> HangingDoc a
hangBreak = HangBreak <<< flexGroup

hangApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangApp = HangApp indent

hangOps :: forall a. HangingDoc a -> NonEmptyArray (HangingOp a) -> HangingDoc a
hangOps = HangOps indent

hangConcatApp :: forall a. HangingDoc a -> NonEmptyArray (HangingDoc a) -> HangingDoc a
hangConcatApp a b = case a of
  HangApp ind head tail -> HangApp ind head (tail <> b)
  _ -> HangApp indent a b

hangHead :: forall a. HangingDoc a -> FormatDoc a
hangHead = case _ of
  HangBreak doc -> doc
  HangOps _ doc _ -> hangHead doc
  HangApp _ doc _ -> hangHead doc

overHangHead :: forall a. (FormatDoc a -> FormatDoc a) -> HangingDoc a -> HangingDoc a
overHangHead f = go
  where
  go = case _ of
    HangBreak doc -> HangBreak (f doc)
    HangOps ind doc docs -> HangOps ind (go doc) docs
    HangApp ind doc docs -> HangApp ind (go doc) docs

forceBreaks :: forall a. Int -> Doc a
forceBreaks n
  | n >= 2 = Dodo.break <> Dodo.break
  | otherwise = Dodo.break

breaks :: forall a. ForceBreak -> Int -> Doc a
breaks fl n
  | fl == ForceBreak || n > 0 = forceBreaks n
  | fl == ForceSpace = Dodo.space
  | otherwise = mempty

toFormatDoc :: forall a. HangingDoc a -> FormatDoc a
toFormatDoc = fst <<< goInit
  where
  goInit = case _ of
    HangBreak doc ->
      Tuple doc doc
    HangApp ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr goInitApp (goLastApp last) init
        this = fst (goInit head)
        docGroup = flexSelect this (ind (fst next)) (indMulti head ind (fst next))
        docBreak = this `break` ind (snd next)
      Tuple docGroup docBreak
    HangOps ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr (goInitOp ind) (goLastOp ind last) init
        this = fst (goInit head)
        docGroup = flexGroup this <> ind (fst next)
        docBreak = this `break` ind (snd next)
      Tuple docGroup docBreak

  goLast = case _ of
    HangBreak doc -> do
      let doc' = docJoin doc
      Tuple doc' doc'
    HangApp ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr goInitApp (goLastApp last) init
        this = fst (goInit head)
        docGroup = flexSelectJoin this (fst next) (indMulti head ind (fst next))
        docBreak = docJoin this <> indMulti head ind (fst next)
      Tuple docGroup docBreak
    HangOps ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr (goInitOp ind) (goLastOp ind last) init
        this = fst (goInit head)
        docGroup = flexSelectJoin this (fst next) (ind (fst next))
        docBreak = docJoin this `break` ind (snd next)
      Tuple docGroup docBreak

  goInitApp doc next = do
    let
      this = fst (goInit doc)
      docGroup = flexSelectJoin this (fst next) (snd next)
      docBreak = docJoin this <> snd next
    Tuple docGroup docBreak

  goLastApp doc = do
    let
      this = goLast doc
      docGroup = flexGroup (fst this)
      docBreak = snd this
    Tuple docGroup docBreak

  goInitOp ind (HangingOp width op doc) next = do
    let
      Tuple op' doc' = realignOp op doc
      algn = if width <= 1 then align 2 else identity
      docOprd = fst (goInitOperand algn ind doc')
      docGroup = flexSelectJoin (op' <> docOprd) (fst next) (snd next)
      docBreak = docJoin op' <> docOprd <> snd next
    Tuple docGroup docBreak

  goLastOp ind (HangingOp width op doc) = do
    let
      algn = if width <= 1 then align 2 else identity
      next = goLastOperand algn ind doc
      docIndent = snd next
      docGroup = flexSelectJoin op (fst next) docIndent
      docBreak = docJoin op <> docIndent
    Tuple docGroup docBreak

  goInitOperand prevAlgn prevInd = case _ of
    HangBreak doc -> do
      let doc' = prevInd (flexGroup (docJoin doc))
      Tuple doc' doc'
    HangApp ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr goInitApp (goLastApp last) init
        this = fst (goInit head)
        docGroup =
          flexSelectJoin (prevInd this)
            (indMulti head (prevAlgn <<< ind) (fst next))
            (prevInd (indMulti head ind (fst next)))
        docBreak = prevInd (docJoin this <> ind (snd next))
      Tuple docGroup docBreak
    HangOps ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr (goInitOp ind) (goLastOp ind last) init
        this = fst (goInit head)
        docGroup =
          flexSelectJoin (prevInd this)
            (prevAlgn (ind (fst next)))
            (prevInd (ind (fst next)))
        docBreak = prevInd (docJoin this <> ind (snd next))
      Tuple docGroup docBreak

  goLastOperand prevAlgn prevInd = case _ of
    HangBreak doc -> do
      let doc' = flexGroup (docJoin doc)
      Tuple doc' (prevInd doc')
    HangApp ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr goInitApp (goLastApp last) init
        this = fst $ goInit $ case head of
          HangApp _ _ _ -> overHangHead forceBreak head
          _ -> head
        docIndent = indMulti head ind (fst next)
        docGroup = flexSelectJoin this (fst next) docIndent
        docBreak = flexSelectJoin (prevInd this) (prevAlgn docIndent) (prevInd docIndent)
      Tuple docGroup docBreak
    HangOps ind head tail -> do
      let
        { init, last } = NonEmptyArray.unsnoc tail
        next = Array.foldr (goInitOp ind) (goLastOp ind last) init
        this = fst (goInit head)
        docIndent = ind (fst next)
        docGroup = flexSelectJoin this (fst next) docIndent
        docBreak = flexSelectJoin (prevInd this) (prevAlgn docIndent) (prevInd docIndent)
      Tuple docGroup docBreak

  flexSelect = case _, _, _ of
    FormatDoc fl1 n1 m1 doc1 fr1, FormatDoc fl2 n2 m2 doc2 fr2, FormatDoc fl3 n3 m3 doc3 fr3 -> do
      let
        doc2' = breaks (max fr1 fl2) n2 <> doc2
        doc3' = breaks (max fr1 fl3) n3 <> doc3
      FormatDoc fl1 n1 (m1 || (m2 && m3))
        (Dodo.flexSelect doc1 doc2' doc3')
        (max fr2 fr3)
    _, _, _ ->
      unsafeCrashWith "flexSelect/FormatEmpty"

  flexSelectJoin = case _, _, _ of
    FormatDoc fl1 n1 m1 doc1 fr1, FormatDoc fl2 n2 m2 doc2 fr2, FormatDoc fl3 n3 m3 doc3 fr3 -> do
      let
        doc2' = breaks (max fr1 fl2) n2 <> doc2
        doc3' = breaks (max fr1 fl3) n3 <> doc3
        br = if fl1 == ForceBreak || n1 > 0 then Dodo.break else Dodo.spaceBreak
      FormatDoc ForceNone 0 (m1 || (m2 && m3))
        (Dodo.flexSelect (br <> doc1) doc2' doc3')
        (max fr2 fr3)
    _, _, _ ->
      unsafeCrashWith "flexSelect/FormatEmpty"

  docJoin = case _ of
    FormatEmpty -> FormatEmpty
    fdoc@(FormatDoc fl n m doc fr) ->
      if fl == ForceBreak || n > 0 then fdoc
      else if m then FormatDoc ForceBreak n m doc fr
      else FormatDoc ForceNone 0 false (Dodo.spaceBreak <> doc) fr

  realignOp op doc = case op, hangHead doc of
    FormatDoc fl1 n1 _ _ fr1, FormatDoc fl2 n2 m2 _ _
      | fl1 /= ForceBreak && n1 == 0 && fr1 /= ForceBreak && fl2 /= ForceBreak && n2 > 0 ->
          realignOp (forceBreak op) (overHangHead flatten doc)
      | HangBreak _ <- doc
      , fr1 /= ForceBreak && fl2 /= ForceBreak && n2 == 0 && m2 ->
          Tuple op (overHangHead forceBreak doc)
    _, _ ->
      Tuple op doc

  indMulti head ind doc = case head of
    HangApp _ _ _ -> doc
    _ -> ind doc
