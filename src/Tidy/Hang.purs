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
import Tidy.Doc (ForceBreak(..), FormatDoc(..), LeadingComment(..), TrailingComment(..), align, break, breakDoc, flatten, flexGroup, forceMinSourceBreaks, indent)

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
          HangApp _ _ _ -> overHangHead (forceMinSourceBreaks 1) head
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

  flexSelect (FormatDoc doc1) (FormatDoc doc2) (FormatDoc doc3) = do
    let
      TrailingComment comm1r = doc1.trailing

      doc1' =
        doc1.doc
          <> breakDoc comm1r.left comm1r.doc

      LeadingComment comm2l = doc2.leading
      TrailingComment comm2r = doc2.trailing

      doc2' =
        breaks (max comm1r.right comm2l.left) comm2l.lines
          <> comm2l.doc
          <> breakDoc comm2l.right doc2.doc
          <> breakDoc comm2r.left comm2r.doc

      LeadingComment comm3l = doc3.leading
      TrailingComment comm3r = doc3.trailing

      doc3' =
        breaks (max comm1r.right comm3l.left) comm3l.lines
          <> comm3l.doc
          <> breakDoc comm2l.right doc3.doc
          <> breakDoc comm3r.left comm3r.doc

      m1 = doc1.multiline || comm1r.multiline
      m2 = comm2l.multiline || doc2.multiline || comm2r.multiline
      m3 = comm3l.multiline || doc3.multiline || comm3r.multiline

    FormatDoc
      { doc: Dodo.flexSelect doc1' doc2' doc3'
      , leading: doc1.leading
      , isEmpty: false
      , multiline: m1 || (m2 && m3)
      , trailing:
          TrailingComment
            { doc: mempty
            , left: ForceNone
            , multiline: false
            , right: max comm2r.right comm3r.right
            }
      }

  flexSelectJoin (FormatDoc doc1) (FormatDoc doc2) (FormatDoc doc3) = do
    let
      LeadingComment comm1l = doc1.leading
      TrailingComment comm1r = doc1.trailing

      break
        | comm1l.left == ForceBreak || comm1l.lines > 0 = Dodo.break
        | otherwise = Dodo.spaceBreak

      doc1' =
        break
          <> comm1l.doc
          <> breakDoc comm1l.right doc1.doc
          <> breakDoc comm1r.left comm1r.doc

      LeadingComment comm2l = doc2.leading
      TrailingComment comm2r = doc2.trailing

      doc2' =
        breaks (max comm1r.right comm2l.left) comm2l.lines
          <> comm2l.doc
          <> breakDoc comm2l.right doc2.doc
          <> breakDoc comm2r.left comm2r.doc

      LeadingComment comm3l = doc3.leading
      TrailingComment comm3r = doc3.trailing

      doc3' =
        breaks (max comm1r.right comm3l.left) comm3l.lines
          <> comm3l.doc
          <> breakDoc comm2l.right doc3.doc
          <> breakDoc comm3r.left comm3r.doc

      m1 = comm1l.multiline || doc1.multiline || comm1r.multiline
      m2 = comm2l.multiline || doc2.multiline || comm2r.multiline
      m3 = comm3l.multiline || doc3.multiline || comm3r.multiline

    FormatDoc
      { doc: Dodo.flexSelect doc1' doc2' doc3'
      , leading: mempty
      , isEmpty: false
      , multiline: m1 || (m2 && m3)
      , trailing:
          TrailingComment
            { doc: mempty
            , left: ForceNone
            , multiline: false
            , right: max comm2r.right comm3r.right
            }
      }

  docJoin fdoc@(FormatDoc doc)
    | doc.isEmpty = fdoc
    | otherwise = do
        let LeadingComment comm = doc.leading
        if comm.left == ForceBreak || comm.lines > 0 then
          fdoc
        else if comm.multiline || doc.multiline then
          forceMinSourceBreaks 1 fdoc
        else
          FormatDoc doc
            { doc = Dodo.spaceBreak <> comm.doc <> breakDoc comm.right doc.doc
            , leading = mempty
            }

  realignOp op doc = case op, hangHead doc of
    FormatDoc doc1@{ leading: LeadingComment comm1, trailing: TrailingComment comm2 },
    FormatDoc { leading: LeadingComment comm3 }
      | comm1.left /= ForceBreak && comm1.lines == 0 && comm2.right /= ForceBreak && comm3.left /= ForceBreak && comm3.lines > 0 ->
          realignOp (forceMinSourceBreaks 1 op) (overHangHead flatten doc)
      | HangBreak _ <- doc
      , comm2.right /= ForceBreak && comm3.left /= ForceBreak && comm3.lines == 0 && (comm3.multiline || doc1.multiline) ->
          Tuple op (overHangHead (forceMinSourceBreaks 1) doc)
    _, _ ->
      Tuple op doc

  indMulti head ind doc = case head of
    HangApp _ _ _ -> doc
    _ -> ind doc
