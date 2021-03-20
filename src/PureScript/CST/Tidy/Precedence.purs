module PureScript.CST.Tidy.Precedence
  ( QualifiedOperator(..)
  , OperatorNamespace(..)
  , OperatorTree(..)
  , OperatorChain
  , Precedence
  , PrecedenceMap
  , defaultPrecedence
  , toOperatorTree
  , remapOperators
  , insertOperator
  , lookupOperator
  , remapOperatorTo
  , remapModuleTo
  , remapModuleToHiding
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (class Foldable, elem, foldMap, foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), snd, uncurry)
import PureScript.CST.Types (Declaration(..), FixityOp(..), Import(..), ImportDecl(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator, Separated(..), Wrapped(..))

data OperatorNamespace = OperatorType | OperatorValue

derive instance eqOperatorNamespace :: Eq OperatorNamespace
derive instance ordOperatorNamespace :: Ord OperatorNamespace

data QualifiedOperator = QualifiedOperator (Maybe ModuleName) OperatorNamespace Operator

derive instance eqQualifiedOperator :: Eq QualifiedOperator
derive instance ordQualifiedOperator :: Ord QualifiedOperator

type Precedence = Int

defaultPrecedence :: Precedence
defaultPrecedence = 10

type PrecedenceMap = Map (Maybe ModuleName) (Map (Tuple OperatorNamespace Operator) Precedence)

data OperatorTree op a
  = OpList (OperatorTree op a) Precedence (OperatorChain op a)
  | OpPure a

data OperatorStk op a
  = OpHead (OperatorTree op a)
  | OpPrec (OperatorStk op a) Precedence (OperatorChain op a)

type OperatorChain op a = NonEmptyArray (Tuple op (OperatorTree op a))

toOperatorTree
  :: forall op a
   . PrecedenceMap
  -> (op -> QualifiedOperator)
  -> a
  -> NonEmptyArray (Tuple op a)
  -> OperatorTree op a
toOperatorTree precMap getOperator init =
  unwind <<< foldl go (OpHead (OpPure init))
  where
  go :: OperatorStk op a -> Tuple op a -> OperatorStk op a
  go stk (Tuple op value) = do
    let QualifiedOperator modName opNs opName = getOperator op
    let prec = fromMaybe defaultPrecedence $ Map.lookup (Tuple opNs opName) =<< Map.lookup modName precMap
    let opCh = pure $ Tuple op $ OpPure value
    push stk $ pure $ Tuple prec opCh

push :: forall op a. OperatorStk op a -> List (Tuple Precedence (OperatorChain op a)) -> OperatorStk op a
push stk chs = case chs of
  Nil -> stk
  Tuple prec ops : Nil ->
    case stk of
      OpHead _ ->
        OpPrec stk prec ops
      OpPrec prevStk prevPrec prevOps ->
        case compare prec prevPrec of
          EQ -> OpPrec prevStk prevPrec (prevOps <> ops)
          GT -> OpPrec stk prec ops
          LT -> push prevStk (Tuple prevPrec prevOps : chs)
  Tuple nextPrec nextOps : next ->
    case stk of
      OpHead value ->
        push (OpHead (OpList value nextPrec nextOps)) next
      OpPrec prevStk prevPrec prevOps ->
        case compare nextPrec prevPrec of
          EQ -> push (OpPrec prevStk prevPrec (prevOps <> nextOps)) next
          GT -> push (OpPrec prevStk prevPrec (snoc prevOps nextPrec nextOps)) next
          LT -> push prevStk (Tuple prevPrec (snoc prevOps nextPrec nextOps) : next)

unwind :: forall op a. OperatorStk op a -> OperatorTree op a
unwind = case _ of
  OpHead value -> value
  OpPrec stk prec ops -> go prec ops stk
  where
  go prec ops = case _ of
    OpHead value -> OpList value prec ops
    OpPrec stk prevPrec prevOps ->
      go prevPrec (snoc prevOps prec ops) stk

snoc :: forall op a. OperatorChain op a -> Precedence -> OperatorChain op a -> OperatorChain op a
snoc prevOps nextPrec nextOps = do
  let { init, last: Tuple op value } = NonEmptyArray.unsnoc prevOps
  NonEmptyArray.snoc' init (Tuple op (OpList value nextPrec nextOps))

remapOperators :: forall e. PrecedenceMap -> Module e -> PrecedenceMap
remapOperators = goModule
  where
  goModule precMap (Module { header: ModuleHeader { name: Name { name: modName }, imports }, body: ModuleBody { decls } }) =
    foldl (goDecl modName) (foldl goImportDecl precMap imports) decls

  goImportDecl precMap (ImportDecl { "module": Name { name: modName }, names, qualified }) = do
    let
      newModName =
        map (\(Tuple _ (Name { name })) -> name) qualified
    case names of
      Nothing ->
        remapModuleTo newModName modName precMap
      Just (Tuple hiding (Wrapped { value: Separated { head, tail } })) -> do
        let
          impOps =
            goImport modName head
              <> foldMap (goImport modName <<< snd) tail
        if isJust hiding then
          remapModuleToHiding impOps newModName modName precMap
        else
          foldl (flip (remapOperatorTo newModName)) precMap impOps

  goImport modName = case _ of
    ImportOp (Name { name: op }) ->
      [QualifiedOperator (Just modName) OperatorValue op]
    ImportTypeOp _ (Name { name: op }) ->
      [QualifiedOperator (Just modName) OperatorType op]
    _ ->
      []

  goDecl modName precMap = case _ of
    DeclFixity { prec: Tuple _ prec, operator } ->
      case operator of
        FixityValue _ _ (Name { name: op }) ->
          precMap
            # insertOperator (QualifiedOperator Nothing OperatorValue op) prec
            # insertOperator (QualifiedOperator (Just modName) OperatorValue op) prec
        FixityType _ _ _ (Name { name: op }) ->
          precMap
            # insertOperator (QualifiedOperator Nothing OperatorType op) prec
            # insertOperator (QualifiedOperator (Just modName) OperatorType op) prec
    _ ->
      precMap

insertOperator :: QualifiedOperator -> Precedence -> PrecedenceMap -> PrecedenceMap
insertOperator (QualifiedOperator modName opNs op) prec =
  Map.alter
    case _ of
      Nothing ->
        Just $ Map.singleton opKey prec
      Just ops ->
        Just $ Map.insert opKey prec ops
    modName
  where
  opKey = Tuple opNs op

lookupOperator :: QualifiedOperator -> PrecedenceMap -> Maybe Precedence
lookupOperator (QualifiedOperator modName opNs op) precMap =
  Map.lookup modName precMap
    >>= Map.lookup (Tuple opNs op)

remapOperatorTo :: Maybe ModuleName -> QualifiedOperator -> PrecedenceMap -> PrecedenceMap
remapOperatorTo newModName qualOp@(QualifiedOperator _ opNs op) precMap =
  fromMaybe precMap do
    prec <- lookupOperator qualOp precMap
    pure $ insertOperator (QualifiedOperator newModName opNs op) prec precMap

remapModuleTo :: Maybe ModuleName -> ModuleName -> PrecedenceMap -> PrecedenceMap
remapModuleTo newModName modName precMap =
  fromMaybe precMap do
    ops <- Map.lookup (Just modName) precMap
    pure $ Map.alter
      case _ of
        Nothing ->
          Just ops
        Just oldOps ->
          Just (Map.union ops oldOps)
      newModName
      precMap

remapModuleToHiding
  :: forall f
   . Foldable f
  => f QualifiedOperator
  -> Maybe ModuleName
  -> ModuleName
  -> PrecedenceMap
  -> PrecedenceMap
remapModuleToHiding hiding newModName modName precMap =
  fromMaybe precMap do
    ops <- Map.lookup (Just modName) precMap
    let filteredOps = Map.filterKeys (not <<< flip elem hiding <<< uncurry (QualifiedOperator (Just modName))) ops
    pure $ Map.alter
      case _ of
        Nothing ->
          Just filteredOps
        Just oldOps ->
          Just (Map.union filteredOps oldOps)
      newModName
      precMap
