module Tidy
  ( FormatOptions
  , defaultFormatOptions
  , TypeArrowOption(..)
  , ThenPlacementOption(..)
  , ImportSortOption(..)
  , ImportWrapOption(..)
  , Format
  , formatModule
  , formatDecl
  , formatType
  , formatExpr
  , formatBinder
  , class FormatError
  , formatError
  , module Exports
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap, foldl, foldr)
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (power)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst, snd)
import Dodo as Dodo
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (RecoveredError(..))
import PureScript.CST.Types (AppSpine(..), Binder(..), ClassFundep(..), ClassHead, Comment(..), DataCtor(..), DataHead, DataMembers(..), Declaration(..), Delimited, DelimitedNonEmpty, DoStatement(..), Export(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr(..), Ident, IfThenElse, Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), InstanceHead, Label, Labeled(..), LetBinding(..), LineFeed, Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), OneOrDelimited(..), Operator, PatternGuard(..), Prefixed(..), Proper, QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), SourceStyle(..), SourceToken, Token(..), Type(..), TypeVarBinding(..), ValueBindingFields, Where(..), Wrapped(..))
import Tidy.Doc (FormatDoc, align, alignCurrentColumn, anchor, break, flexDoubleBreak, flexGroup, flexSoftBreak, flexSpaceBreak, forceMinSourceBreaks, fromDoc, indent, joinWith, joinWithMap, leadingBlockComment, leadingLineComment, locally, softBreak, softSpace, sourceBreak, space, spaceBreak, text, trailingBlockComment, trailingLineComment)
import Tidy.Doc (FormatDoc, toDoc) as Exports
import Tidy.Doc as Doc
import Tidy.Hang (HangingDoc, HangingOp(..), hang, hangApp, hangBreak, hangOps, hangWithIndent)
import Tidy.Hang as Hang
import Tidy.Precedence (OperatorNamespace(..), OperatorTree(..), PrecedenceMap, QualifiedOperator(..), toOperatorTree)
import Tidy.Token (UnicodeOption(..)) as Exports
import Tidy.Token (UnicodeOption(..), printToken)
import Tidy.Util (nameOf, overLabel, splitLines, splitStringEscapeLines)

data TypeArrowOption
  = TypeArrowFirst
  | TypeArrowLast

derive instance eqTypeArrowOption :: Eq TypeArrowOption

data ThenPlacementOption
  = ThenSameLine
  | ThenNewLine

derive instance eqThenPlacementOption :: Eq ThenPlacementOption

data ImportWrapOption
  = ImportWrapSource
  | ImportWrapAuto

derive instance eqImportWrapOption :: Eq ImportWrapOption

data ImportSortOption
  = ImportSortSource
  | ImportSortIde

derive instance eqImportSortOpion :: Eq ImportSortOption

type FormatOptions e a =
  { formatError :: e -> FormatDoc a
  , unicode :: UnicodeOption
  , typeArrowPlacement :: TypeArrowOption
  , thenPlacement :: ThenPlacementOption
  , operators :: PrecedenceMap
  , importSort :: ImportSortOption
  , importWrap :: ImportWrapOption
  }

defaultFormatOptions :: forall e a. FormatError e => FormatOptions e a
defaultFormatOptions =
  { formatError
  , unicode: UnicodeSource
  , typeArrowPlacement: TypeArrowFirst
  , thenPlacement: ThenSameLine
  , operators: Map.empty
  , importSort: ImportSortSource
  , importWrap: ImportWrapSource
  }

class FormatError e where
  formatError :: forall a. e -> FormatDoc a

instance formatErrorVoid :: FormatError Void where
  formatError = absurd

instance formatErrorRecoveredError :: FormatError RecoveredError where
  formatError (RecoveredError { tokens }) =
    case Array.uncons tokens of
      Just { head, tail } ->
        case Array.unsnoc tail of
          Just { init, last } ->
            formatWithComments head.leadingComments last.trailingComments
              $ fromDoc
              $ Dodo.withPosition \{ nextIndent } -> do
                  let
                    head' =
                      Dodo.text (printToken UnicodeSource head.value)
                        <> formatRecoveredComments nextIndent head.trailingComments
                    init' = init # foldMap \tok ->
                      formatRecoveredComments nextIndent tok.leadingComments
                        <> Dodo.text (printToken UnicodeSource tok.value)
                        <> formatRecoveredComments nextIndent tok.trailingComments
                    last' =
                      formatRecoveredComments nextIndent last.leadingComments
                        <> Dodo.text (printToken UnicodeSource last.value)
                  head' <> init' <> last'

          Nothing ->
            formatToken { unicode: UnicodeSource } head
      Nothing ->
        mempty
    where
    formatRecoveredComments :: forall a b. Int -> Array (Comment a) -> Dodo.Doc b
    formatRecoveredComments ind = _.doc <<< foldl (goComments ind) { line: false, doc: mempty }

    goComments :: forall a b. Int -> { line :: Boolean, doc :: Dodo.Doc b } -> Comment a -> { line :: Boolean, doc :: Dodo.Doc b }
    goComments ind acc = case _ of
      Comment str
        | leading <- SCU.take 2 str
        , leading == "--" || leading == "#!" ->
            { line: false, doc: acc.doc <> Dodo.text str }
        | otherwise ->
            { line: false, doc: acc.doc <> Dodo.lines (Dodo.text <$> splitLines str) }
      Line _ n ->
        { line: true, doc: acc.doc <> power Dodo.break n }
      Space n
        | acc.line ->
            { line: false, doc: acc.doc <> Dodo.text (power " " $ max 0 (n - ind)) }
        | otherwise ->
            { line: false, doc: acc.doc <> Dodo.text (power " " n) }

type Format f e a = FormatOptions e a -> f -> FormatDoc a
type FormatHanging f e a = FormatOptions e a -> f -> HangingDoc a
type FormatSpace a = FormatDoc a -> FormatDoc a -> FormatDoc a

formatComment
  :: forall l a
   . (String -> FormatDoc a -> FormatDoc a)
  -> (String -> FormatDoc a -> FormatDoc a)
  -> Comment l
  -> FormatDoc a
  -> FormatDoc a
formatComment lineComment blockComment com next = case com of
  Comment str
    | leading <- SCU.take 2 str
    , leading == "--" || leading == "#!" ->
        lineComment str next
    | otherwise ->
        blockComment str next
  Line _ n ->
    sourceBreak n next
  Space _ ->
    next

formatWithComments :: forall a. Array (Comment LineFeed) -> Array (Comment Void) -> FormatDoc a -> FormatDoc a
formatWithComments leading trailing doc =
  foldr
    (formatComment leadingLineComment leadingBlockComment)
    (doc <> foldr (formatComment trailingLineComment trailingBlockComment) mempty trailing)
    leading

formatToken :: forall a r. { unicode :: UnicodeOption | r } -> SourceToken -> FormatDoc a
formatToken conf tok = formatWithComments tok.leadingComments tok.trailingComments tokDoc
  where
  tokStr = printToken conf.unicode tok.value
  tokDoc = case tok.value of
    TokRawString _ -> formatRawString tokStr
    TokString _ _ -> formatString tokStr
    _ -> text tokStr

formatRawString :: forall a. String -> FormatDoc a
formatRawString = splitLines >>> Array.uncons >>> foldMap \{ head, tail } ->
  if Array.null tail then
    text head
  else
    fromDoc $ Dodo.lines
      [ Dodo.text head
      , Dodo.locally (_ { indent = 0, indentSpaces = "" }) do
          Array.intercalate Dodo.break $ Dodo.text <$> tail
      ]

formatString :: forall a. String -> FormatDoc a
formatString = splitStringEscapeLines >>> Array.uncons >>> foldMap \{ head, tail } ->
  case Array.unsnoc tail of
    Nothing -> text head
    Just rest ->
      text (head <> "\\")
        `break` joinWithMap break (\str -> text ("\\" <> str <> "\\")) rest.init
        `break` text ("\\" <> rest.last)

formatName :: forall e a n. Format (Name n) e a
formatName conf (Name { token }) = formatToken conf token

formatPrefixedName :: forall e a n. Format (Prefixed (Name n)) e a
formatPrefixedName conf (Prefixed { prefix, value: Name { token } }) =
  foldMap (formatToken conf) prefix <> formatToken conf token

formatQualifiedName :: forall e a n. Format (QualifiedName n) e a
formatQualifiedName conf (QualifiedName { token }) = formatToken conf token

formatModule :: forall e a. Format (Module e) e a
formatModule conf (Module { header: ModuleHeader header, body: ModuleBody body }) =
  joinWith break
    [ anchor (formatToken conf header.keyword) `space` indent do
        anchor (formatName conf header.name)
          `flexSpaceBreak`
            anchor (foldMap (formatParenListNonEmpty NotGrouped formatExport conf) header.exports)
          `space`
            anchor (formatToken conf header."where")
    , forceMinSourceBreaks 2 case conf.importWrap of
        ImportWrapAuto ->
          imports
        ImportWrapSource ->
          locally (_ { pageWidth = top, ribbonRatio = 1.0 }) imports
    , forceMinSourceBreaks 2 $ formatTopLevelGroups conf body.decls
    , foldr (formatComment leadingLineComment leadingBlockComment) mempty body.trailingComments
    ]
  where
  formatImports k =
    joinWithMap break (k <<< formatImportDecl conf)

  imports =
    case conf.importSort of
      ImportSortSource ->
        formatImports identity header.imports
      ImportSortIde -> do
        let { yes, no } = Array.partition isOpenImport sorted
        formatImports Doc.flatten yes
          <> forceMinSourceBreaks 2 (formatImports Doc.flatten no)
        where
        sorted =
          header.imports
            # map toComparison
            # Array.sortWith fst
            # map snd

        toComparison (ImportDecl decl) = do
          let modName = nameOf decl.module
          let qualName = nameOf <<< snd <$> decl.qualified
          case decl.names of
            Just (Tuple hiding names) -> do
              let Tuple cmps names' = sortImportsIde names
              let order = if isJust hiding then 3 else 1
              Tuple (ImportModuleCmp modName order cmps qualName) (ImportDecl decl { names = Just (Tuple hiding names') })
            Nothing ->
              Tuple (ImportModuleCmp modName 2 [] qualName) (ImportDecl decl)

        isOpenImport (ImportDecl a) = case a.qualified, a.names of
          Nothing, Nothing ->
            true
          Nothing, Just (Tuple (Just _) _) ->
            true
          _, _ ->
            false

data ImportModuleComparison =
  ImportModuleCmp ModuleName Int (Array ImportComparison) (Maybe ModuleName)

derive instance eqImportModuleComparison :: Eq ImportModuleComparison
derive instance ordImportModuleComparison :: Ord ImportModuleComparison

formatExport :: forall e a. Format (Export e) e a
formatExport conf = case _ of
  ExportValue n ->
    formatName conf n
  ExportOp n ->
    formatName conf n
  ExportType n members ->
    flexGroup $ formatName conf n `softBreak` indent (foldMap (formatDataMembers conf) members)
  ExportTypeOp t n ->
    formatToken conf t `space` anchor (formatName conf n)
  ExportClass t n ->
    formatToken conf t `space` anchor (formatName conf n)
  ExportModule t n ->
    formatToken conf t `space` anchor (formatName conf n)
  ExportError e ->
    conf.formatError e

formatDataMembers :: forall e a. Format DataMembers e a
formatDataMembers conf = case _ of
  DataAll t ->
    formatToken conf t
  DataEnumerated ms ->
    formatParenList NotGrouped formatName conf ms

formatImportDecl :: forall e a. Format (ImportDecl e) e a
formatImportDecl conf (ImportDecl imp) =
  formatToken conf imp.keyword `space` indent (anchor importDeclBody)
  where
  importDeclBody = case imp.names of
    Just (Tuple (Just hiding) nameList) ->
      formatName conf imp."module"
        `space` anchor (formatToken conf hiding)
        `flexSpaceBreak` anchor (formatParenListNonEmpty NotGrouped formatImport conf nameList)
        `space` anchor (foldMap formatImportQualified imp.qualified)
    Just (Tuple Nothing nameList) ->
      formatName conf imp."module"
        `flexSpaceBreak` anchor (formatParenListNonEmpty NotGrouped formatImport conf nameList)
        `space` anchor (foldMap formatImportQualified imp.qualified)
    Nothing ->
      formatName conf imp."module"
        `space` anchor (foldMap formatImportQualified imp.qualified)

  formatImportQualified (Tuple as qualName) =
    formatToken conf as `space` anchor (formatName conf qualName)

sortImportsIde :: forall e. DelimitedNonEmpty (Import e) -> Tuple (Array ImportComparison) (DelimitedNonEmpty (Import e))
sortImportsIde (Wrapped { open, value: Separated { head, tail }, close }) = do
  let
    Tuple commas tail' = Array.unzip tail
    Tuple cmps imports =
      NonEmptyArray.cons' head tail'
        # map (Tuple =<< toComparison)
        # NonEmptyArray.sortWith fst
        # NonEmptyArray.unzip

  Tuple (NonEmptyArray.toArray cmps) $ Wrapped
    { open
    , value: Separated
        { head: NonEmptyArray.head imports
        , tail: Array.zip commas (NonEmptyArray.tail imports)
        }
    , close
    }
  where
  toComparison = case _ of
    ImportValue (Name { name }) ->
      ImportValueCmp name
    ImportOp (Name { name }) ->
      ImportOpCmp name
    ImportType (Name { name }) Nothing ->
      ImportTypeCmp name true []
    ImportType (Name { name }) (Just (DataEnumerated (Wrapped { value }))) ->
      case value of
        Nothing ->
          ImportTypeCmp name true []
        Just (Separated ctors) ->
          ImportTypeCmp name true $ (_.name <<< un Name) <$> Array.cons ctors.head (map snd ctors.tail)
    ImportType (Name { name }) (Just (DataAll _)) ->
      ImportTypeCmp name false []
    ImportTypeOp _ (Name { name }) ->
      ImportTypeOpCmp name
    ImportClass _ (Name { name }) ->
      ImportClassCmp name
    ImportError _ ->
      ImportErrorCmp

data ImportComparison
  = ImportClassCmp Proper
  | ImportTypeOpCmp Operator
  | ImportTypeCmp Proper Boolean (Array Proper)
  | ImportValueCmp Ident
  | ImportOpCmp Operator
  | ImportErrorCmp

derive instance eqImportComparison :: Eq ImportComparison
derive instance ordImportComparison :: Ord ImportComparison

formatImport :: forall e a. Format (Import e) e a
formatImport conf = case _ of
  ImportValue n ->
    formatName conf n
  ImportOp n ->
    formatName conf n
  ImportType n members ->
    flexGroup $ formatName conf n `softBreak` indent (foldMap (formatDataMembers conf) members)
  ImportTypeOp t n ->
    formatToken conf t `space` anchor (formatName conf n)
  ImportClass t n ->
    formatToken conf t `space` anchor (formatName conf n)
  ImportError e ->
    conf.formatError e

formatDecl :: forall e a. Format (Declaration e) e a
formatDecl conf = case _ of
  DeclData head (Just (Tuple equals (Separated ctors))) ->
    if Array.null ctors.tail then
      declareHanging
        (formatDataHead conf head)
        space
        (anchor (formatToken conf equals))
        (formatHangingDataCtor conf ctors.head)
    else
      formatDataHead conf head `flexSpaceBreak` indent do
        formatDataElem (Tuple equals ctors.head)
          `spaceBreak` joinWithMap spaceBreak formatDataElem ctors.tail
    where
    formatDataElem (Tuple a b) =
      formatToken conf a
        `space` formatListElem 2 formatDataCtor conf b

  DeclData head _ ->
    formatDataHead conf head

  DeclType head equals ty ->
    declareHanging
      (formatDataHead conf head)
      space
      (anchor (formatToken conf equals))
      (formatHangingType conf ty)

  DeclNewtype head equals name ty ->
    declareHanging
      (formatDataHead conf head)
      space
      (anchor (formatToken conf equals))
      (formatHangingDataCtor conf (DataCtor { name, fields: [ ty ] }))

  DeclRole kw1 kw2 name rls ->
    flatten $ words <> NonEmptyArray.toArray roles
    where
    words =
      [ formatToken conf kw1
      , formatToken conf kw2
      , formatName conf name
      ]

    roles =
      map (formatToken conf <<< fst) rls

  DeclFixity { keyword: Tuple keyword _, prec: Tuple prec _, operator } ->
    case operator of
      FixityValue name as op ->
        flatten
          [ formatToken conf keyword
          , formatToken conf prec
          , formatQualifiedName conf name
          , formatToken conf as
          , formatName conf op
          ]
      FixityType ty name as op ->
        flatten
          [ formatToken conf keyword
          , formatToken conf prec
          , formatToken conf ty
          , formatQualifiedName conf name
          , formatToken conf as
          , formatName conf op
          ]

  DeclKindSignature tok (Labeled { label, separator, value }) ->
    formatSignature conf $ Labeled
      { label:
          flatten
            [ formatToken conf tok
            , formatName conf label
            ]
      , separator
      , value
      }

  DeclForeign kw1 kw2 frn ->
    case frn of
      ForeignValue lbl ->
        formatSignature conf $ overLabel
          ( \label ->
              flatten
                [ formatToken conf kw1
                , formatToken conf kw2
                , formatName conf label
                ]
          )
          lbl
      ForeignData kw3 lbl ->
        formatSignature conf $ overLabel
          ( \label ->
              flatten
                [ formatToken conf kw1
                , formatToken conf kw2
                , formatToken conf kw3
                , formatName conf label
                ]
          )
          lbl
      ForeignKind kw3 name ->
        flatten
          [ formatToken conf kw1
          , formatToken conf kw2
          , formatToken conf kw3
          , formatName conf name
          ]

  DeclClass clsHead mbBody ->
    case mbBody of
      Nothing ->
        formatClassHead conf (Tuple clsHead Nothing)
      Just (Tuple wh sigs) ->
        formatClassHead conf (Tuple clsHead (Just wh))
          `break` indent do
            joinWithMap break
              (formatSignature conf <<< overLabel (formatName conf))
              sigs

  DeclInstanceChain (Separated { head, tail }) ->
    formatInstance conf head
      `break`
        joinWithMap break
          (\(Tuple tok inst) -> formatToken conf tok `space` anchor (formatInstance conf inst))
          tail

  DeclDerive kw nt hd ->
    formatToken conf kw
      `space` foldMap (indent <<< anchor <<< formatToken conf) nt
      `space` anchor (formatInstanceHead conf (Tuple hd Nothing))

  DeclSignature sig ->
    formatSignature conf $ overLabel (flatten <<< pure <<< formatName conf) sig

  DeclValue binding ->
    formatValueBinding conf binding

  DeclError e ->
    conf.formatError e

formatDataHead :: forall e a. Format (DataHead e) e a
formatDataHead conf { keyword, name, vars } =
  formatToken conf keyword `space` indent do
    anchor (formatName conf name)
      `flexSpaceBreak` joinWithMap spaceBreak (formatTypeVarBindingPlain conf) vars

formatDataCtor :: forall e a. Format (DataCtor e) e a
formatDataCtor conf = Hang.toFormatDoc <<< formatHangingDataCtor conf

formatHangingDataCtor :: forall e a. FormatHanging (DataCtor e) e a
formatHangingDataCtor conf (DataCtor { name, fields }) =
  case NonEmptyArray.fromArray fields of
    Nothing -> hangingName
    Just fs -> hangingName `hangApp` map (formatHangingType conf) fs
  where
  hangingName =
    hangBreak $ formatName conf name

formatClassHead :: forall e a. Format (Tuple (ClassHead e) (Maybe SourceToken)) e a
formatClassHead conf (Tuple cls wh) =
  formatToken conf cls.keyword `flexSpaceBreak` indent do
    anchor (foldMap (formatConstraints conf) cls.super)
      `spaceBreak`
        flexGroup do
          formatName conf cls.name
            `spaceBreak`
              joinWithMap spaceBreak (indent <<< formatTypeVarBindingPlain conf) cls.vars
      `spaceBreak`
        flexGroup do
          anchor (foldMap formatFundeps cls.fundeps)
      `space`
        foldMap (formatToken conf) wh
  where
  formatFundeps (Tuple tok (Separated { head, tail })) =
    formatToken conf tok
      `space`
        formatListElem 2 formatFundep conf head
      `softBreak`
        joinWithMap softBreak
          ( \(Tuple sep elem) ->
              formatToken conf sep
                `space` formatListElem 2 formatFundep conf elem
          )
          tail

formatConstraints :: forall e a. Format (Tuple (OneOrDelimited (Type e)) SourceToken) e a
formatConstraints conf (Tuple cs arr) =
  formatOneOrDelimited formatType conf cs
    `space` anchor (formatToken conf unicodeArr)
  where
  unicodeArr = case arr.value of
    TokOperator Nothing "<=" | conf.unicode == UnicodeAlways ->
      arr { value = TokOperator Nothing "⇐" }
    TokOperator Nothing "⇐" | conf.unicode == UnicodeNever ->
      arr { value = TokOperator Nothing "<=" }
    _ ->
      arr

formatFundep :: forall e a. Format ClassFundep e a
formatFundep conf = case _ of
  FundepDetermined tok names ->
    formatToken conf tok
      `space` joinWithMap space (formatName conf) names
  FundepDetermines names1 tok names2 ->
    joinWithMap space (formatName conf) names1
      `space` formatToken conf tok
      `space` joinWithMap space (formatName conf) names2

formatOneOrDelimited :: forall b e a. Format b e a -> Format (OneOrDelimited b) e a
formatOneOrDelimited format conf = case _ of
  One a -> format conf a
  Many as -> formatParenListNonEmpty NotGrouped format conf as

formatInstance :: forall e a. Format (Instance e) e a
formatInstance conf (Instance { head, body }) = case body of
  Nothing ->
    formatInstanceHead conf (Tuple head Nothing)
  Just (Tuple wh bindings) ->
    formatInstanceHead conf (Tuple head (Just wh)) `break` indent do
      joinWithMap break (formatInstanceBinding conf) bindings

formatInstanceHead :: forall e a. Format (Tuple (InstanceHead e) (Maybe SourceToken)) e a
formatInstanceHead conf (Tuple hd mbWh) =
  case hd.name of
    Just (Tuple name sep) ->
      formatToken conf hd.keyword
        `space` anchor (formatName conf name)
        `space` anchor (formatToken conf sep)
        `flexSpaceBreak` indent hdTypes
        `space` indent (foldMap (formatToken conf) mbWh)
    Nothing ->
      formatToken conf hd.keyword
        `flexSpaceBreak` indent hdTypes
        `space` indent (foldMap (formatToken conf) mbWh)
  where
  hdTypes =
    foldMap (formatConstraints conf) hd.constraints
      `spaceBreak` flexGroup do
        formatQualifiedName conf hd.className
          `space` indent (joinWithMap spaceBreak (formatType conf) hd.types)

formatInstanceBinding :: forall e a. Format (InstanceBinding e) e a
formatInstanceBinding conf = case _ of
  InstanceBindingSignature sig ->
    formatSignature conf $ overLabel (formatName conf) sig
  InstanceBindingName vbf ->
    formatValueBinding conf vbf

formatTypeVarBinding :: forall e a. Format (TypeVarBinding (Prefixed (Name Ident)) e) e a
formatTypeVarBinding conf = case _ of
  TypeVarKinded w ->
    formatParensBlock formatKindedTypeVarBinding conf w
  TypeVarName n ->
    formatPrefixedName conf n

formatKindedTypeVarBinding :: forall e a. Format (Labeled (Prefixed (Name Ident)) (Type e)) e a
formatKindedTypeVarBinding conf (Labeled { label, separator, value }) =
  formatPrefixedName conf label `space` indent do
    anchor (formatToken conf separator)
      `flexSpaceBreak` formatType conf value

formatTypeVarBindingPlain :: forall e a. Format (TypeVarBinding (Name Ident) e) e a
formatTypeVarBindingPlain conf = case _ of
  TypeVarKinded w ->
    formatParensBlock formatKindedTypeVarBindingPlain conf w
  TypeVarName n ->
    formatName conf n

formatKindedTypeVarBindingPlain :: forall e a. Format (Labeled (Name Ident) (Type e)) e a
formatKindedTypeVarBindingPlain conf (Labeled { label, separator, value }) =
  formatName conf label `space` indent do
    anchor (formatToken conf separator)
      `flexSpaceBreak` formatType conf value

formatSignature :: forall e a. Format (Labeled (FormatDoc a) (Type e)) e a
formatSignature conf (Labeled { label, separator, value }) =
  case conf.typeArrowPlacement of
    TypeArrowFirst ->
      if Array.null polytype.init then
        label `flexSpaceBreak` indent do
          anchor (formatToken conf separator)
            `space` anchor (align width (Hang.toFormatDoc formattedPolytype))
      else
        label `flexSpaceBreak` indent do
          anchor (formatToken conf separator)
            `space` anchor (Hang.toFormatDoc formattedPolytype)
      where
      formattedPolytype =
        formatHangingPolytype (align width) conf polytype

      polytype =
        toPolytype value

      width
        | isUnicode = 2
        | otherwise = 3

      isUnicode = case conf.unicode of
        UnicodeAlways -> true
        UnicodeNever -> false
        UnicodeSource ->
          case separator of
            { value: TokDoubleColon Unicode } -> true
            _ -> false

    TypeArrowLast ->
      label `space` indent do
        flexGroup $ anchor (formatToken conf separator)
          `spaceBreak` anchor (flexGroup (formatType conf value))

formatMonotype :: forall e a. Format (Type e) e a
formatMonotype conf = Hang.toFormatDoc <<< formatHangingMonotype conf

formatHangingMonotype :: forall e a. FormatHanging (Type e) e a
formatHangingMonotype conf = case _ of
  TypeVar n ->
    hangBreak $ formatName conf n
  TypeConstructor n ->
    hangBreak $ formatQualifiedName conf n
  TypeWildcard t ->
    hangBreak $ formatToken conf t
  TypeHole n ->
    hangBreak $ formatName conf n
  TypeString t _ ->
    hangBreak $ formatToken conf t
  TypeInt neg t _ ->
    hangBreak $ foldMap (formatToken conf) neg <> formatToken conf t
  TypeArrowName t ->
    hangBreak $ formatToken conf t
  TypeOpName n ->
    hangBreak $ formatQualifiedName conf n
  TypeRow row ->
    hangBreak $ formatRow softSpace softBreak conf row
  TypeRecord row ->
    hangBreak $ formatRow space spaceBreak conf row
  TypeApp head tail ->
    formatHangingType conf head
      `hangApp` map (formatHangingType conf) tail
  TypeParens ty ->
    hangBreak $ formatParensBlock formatType conf ty
  TypeKinded ty1 t ty2 ->
    hangBreak $ formatType conf ty1 `space` indent do
      anchor (formatToken conf t)
        `flexSpaceBreak` anchor (formatType conf ty2)
  TypeOp ty tys ->
    formatHangingOperatorTree formatQualifiedName formatHangingType conf
      $ toQualifiedOperatorTree conf.operators OperatorType ty tys
  TypeError e ->
    hangBreak $ conf.formatError e
  TypeArrow _ _ _ ->
    unsafeCrashWith "formatMonotype: TypeArrow handled by formatPolytype"
  TypeConstrained _ _ _ ->
    unsafeCrashWith "formatMonotype: TypeConstrained handled by formatPolytype"
  TypeForall _ _ _ _ ->
    unsafeCrashWith "formatMonotype: TypeForall handled by formatPolytype"

formatType :: forall e a. Format (Type e) e a
formatType conf = Hang.toFormatDoc <<< formatHangingType conf

formatHangingType :: forall e a. FormatHanging (Type e) e a
formatHangingType conf = formatHangingPolytype identity conf <<< toPolytype

data Poly e
  = PolyForall SourceToken (NonEmptyArray (TypeVarBinding (Prefixed (Name Ident)) e)) SourceToken
  | PolyArrow (Type e) SourceToken

type Polytype e =
  { init :: Array (Poly e)
  , last :: Type e
  }

toPolytype :: forall e. Type e -> Polytype e
toPolytype = go []
  where
  go init = case _ of
    TypeForall tok vars dot ty ->
      go (Array.snoc init (PolyForall tok vars dot)) ty
    TypeArrow ty1 arr ty2 ->
      go (Array.snoc init (PolyArrow ty1 arr)) ty2
    TypeConstrained ty1 arr ty2 ->
      go (Array.snoc init (PolyArrow ty1 arr)) ty2
    last ->
      { init, last }

formatHangingPolytype :: forall e a. (FormatDoc a -> FormatDoc a) -> FormatHanging (Polytype e) e a
formatHangingPolytype ind conf { init, last } = case conf.typeArrowPlacement of
  _ | Array.null init ->
    formatHangingMonotype conf last
  TypeArrowFirst ->
    hangBreak $ foldl formatPolyArrowFirst ind init $ anchor $ formatMonotype conf last
    where
    isUnicode = Array.all isUnicodeArrow init
    isUnicodeArrow = case conf.unicode of
      UnicodeAlways ->
        const true
      UnicodeNever ->
        const false
      UnicodeSource ->
        case _ of
          PolyArrow _ { value: TokRightArrow Unicode } -> true
          PolyArrow _ { value: TokRightFatArrow Unicode } -> true
          PolyForall { value: TokForall Unicode } _ _ -> true
          _ -> false

    formatPolyArrowFirst k = case _ of
      PolyForall kw vars dot ->
        \doc ->
          k (foldl go (formatToken conf kw) vars)
            `softBreak`
              ( Monoid.guard (not isUnicode) (fromDoc (Dodo.flexAlt mempty Dodo.space))
                  <> anchor (formatToken conf dot)
              )
            `space` anchor (alignCurrentColumn doc)
        where
        go doc tyVar =
          doc `flexSpaceBreak` indent (formatTypeVarBinding conf tyVar)
      PolyArrow ty arr ->
        \doc ->
          k (flexGroup (formatMonotype conf ty))
            `spaceBreak` anchor (formatToken conf arr)
            `space` anchor (alignCurrentColumn doc)

  TypeArrowLast ->
    hangBreak $ joinWithMap spaceBreak formatPolyArrowLast init
      `spaceBreak` flexGroup (formatMonotype conf last)
    where
    formatPolyArrowLast = case _ of
      PolyForall kw vars dot ->
        foldl go (formatToken conf kw) vars
          <> indent (anchor (formatToken conf dot))
        where
        go doc tyVar =
          doc `flexSpaceBreak` indent (formatTypeVarBinding conf tyVar)
      PolyArrow ty arr ->
        flexGroup (formatType conf ty)
          `space` indent (anchor (formatToken conf arr))

formatRow :: forall e a. FormatSpace a -> FormatSpace a -> Format (Wrapped (Row e)) e a
formatRow openSpace closeSpace conf (Wrapped { open, value: Row { labels, tail }, close }) = case labels, tail of
  Nothing, Nothing ->
    formatEmptyList conf { open, close }
  Just value, Nothing ->
    formatDelimitedNonEmpty openSpace closeSpace 2 Grouped formatRowLabeled conf (Wrapped { open, value, close })
  Nothing, Just (Tuple bar ty) ->
    formatToken conf open
      `openSpace`
        flatten
          [ formatToken conf bar
          , formatType conf ty
          ]
      `closeSpace`
        formatToken conf close
  Just (Separated rowLabels), Just (Tuple bar ty) ->
    formatToken conf open
      `openSpace`
        formatListElem 2 formatRowLabeled conf rowLabels.head
      `softBreak`
        formatListTail 2 formatRowLabeled conf rowLabels.tail
      `spaceBreak`
        (formatToken conf bar `space` formatListElem 2 formatType conf ty)
      `closeSpace`
        formatToken conf close

formatRowLabeled :: forall e a. Format (Labeled (Name Label) (Type e)) e a
formatRowLabeled conf (Labeled { label, separator, value }) =
  formatName conf label `space` indent do
    anchor (formatToken conf separator)
      `flexSpaceBreak` anchor (formatType conf value)

formatExpr :: forall e a. Format (Expr e) e a
formatExpr conf = Hang.toFormatDoc <<< formatHangingExpr conf

formatHangingExpr :: forall e a. FormatHanging (Expr e) e a
formatHangingExpr conf = case _ of
  ExprHole n ->
    hangBreak $ formatName conf n
  ExprSection t ->
    hangBreak $ formatToken conf t
  ExprIdent n ->
    hangBreak $ formatQualifiedName conf n
  ExprConstructor n ->
    hangBreak $ formatQualifiedName conf n
  ExprBoolean t _ ->
    hangBreak $ formatToken conf t
  ExprChar t _ ->
    hangBreak $ formatToken conf t
  ExprString t _ ->
    hangBreak $ formatToken conf t
  ExprInt t _ ->
    hangBreak $ formatToken conf t
  ExprNumber t _ ->
    hangBreak $ formatToken conf t
  ExprArray exprs ->
    hangBreak $ formatBasicList Grouped formatExpr conf exprs
  ExprRecord fields ->
    hangBreak $ formatBasicList Grouped (formatRecordLabeled formatHangingExpr) conf fields
  ExprParens expr ->
    hangBreak $ formatParensBlock formatExpr conf expr
  ExprTyped expr separator ty ->
    hangBreak $ formatSignature conf $ Labeled
      { label: formatExpr conf expr
      , separator
      , value: ty
      }
  ExprInfix expr exprs ->
    hangOps
      (formatHangingExpr conf expr)
      (map (\(Tuple op b) -> HangingOp 3 (formatParens formatExpr conf op) (formatHangingExpr conf b)) exprs)
  ExprOp expr exprs ->
    formatHangingOperatorTree formatQualifiedName formatHangingExpr conf
      $ toQualifiedOperatorTree conf.operators OperatorValue expr exprs
  ExprOpName n ->
    hangBreak $ formatQualifiedName conf n
  ExprNegate t expr ->
    hangBreak $ formatToken conf t <> formatExpr conf expr
  ExprRecordAccessor { expr, dot, path: Separated { head, tail } } ->
    hangBreak $ formatExpr conf expr <> indent do
      foldMap anchor
        [ formatToken conf dot
        , formatName conf head
        , foldMap (\(Tuple a b) -> anchor (formatToken conf a) <> anchor (formatName conf b)) tail
        ]
  ExprRecordUpdate expr upd ->
    hang
      (formatExpr conf expr)
      (hangBreak (formatBasicListNonEmpty Grouped formatRecordUpdate conf upd))

  ExprApp expr exprs ->
    hangApp
      (formatHangingExpr conf expr)
      (map (formatHangingExprAppSpine conf) exprs)

  ExprLambda lmb ->
    hang
      ( (formatToken conf lmb.symbol <> alignCurrentColumn binders)
          `space` indent (anchor (formatToken conf lmb.arrow))
      )
      (formatHangingExpr conf lmb.body)
    where
    binders = flexGroup do
      joinWithMap spaceBreak (anchor <<< formatBinder conf) lmb.binders

  ExprIf ifte ->
    hangBreak $ formatElseIfChain conf $ toElseIfChain ifte

  ExprCase caseOf@{ head: Separated { head, tail } } ->
    hang
      (formatToken conf caseOf.keyword `flexSpaceBreak` indent caseHead)
      (hangBreak (joinWithMap break (flexGroup <<< formatCaseBranch conf) caseOf.branches))
    where
    caseHead =
      caseHeadExprs `spaceBreak` anchor (formatToken conf caseOf.of)

    caseHeadExprs =
      foldl
        ( \doc (Tuple a b) ->
            (doc <> anchor (formatToken conf a))
              `spaceBreak` flexGroup (formatExpr conf b)
        )
        (flexGroup (formatExpr conf head))
        tail

  ExprLet letIn ->
    hangBreak $ formatToken conf letIn.keyword
      `spaceBreak`
        indent (formatLetGroups conf (NonEmptyArray.toArray letIn.bindings))
      `spaceBreak`
        (formatToken conf letIn.in `spaceBreak` indent (flexGroup (formatExpr conf letIn.body)))

  ExprDo doBlock ->
    hang
      (formatToken conf doBlock.keyword)
      (hangBreak (joinWithMap break (flexGroup <<< formatDoStatement conf) doBlock.statements))

  ExprAdo adoBlock ->
    hang
      (formatToken conf adoBlock.keyword)
      ( hangBreak
          ( joinWithMap break (formatDoStatement conf) adoBlock.statements
              `flexSpaceBreak`
                ( formatToken conf adoBlock.in
                    `flexSpaceBreak`
                      indent (formatExpr conf adoBlock.result)
                )
          )
      )

  ExprError e ->
    hangBreak $ conf.formatError e

formatHangingExprAppSpine :: forall e a. FormatHanging (AppSpine Expr e) e a
formatHangingExprAppSpine conf = case _ of
  AppType tok ty ->
    hangBreak $ formatToken conf tok <> formatType conf ty
  AppTerm expr ->
    formatHangingExpr conf expr

data ElseIfChain e
  = IfThen SourceToken (Expr e) SourceToken (Expr e)
  | ElseIfThen SourceToken SourceToken (Expr e) SourceToken (Expr e)
  | Else SourceToken (Expr e)

toElseIfChain :: forall e. IfThenElse e -> NonEmptyArray (ElseIfChain e)
toElseIfChain ifte = go (pure (IfThen ifte.keyword ifte.cond ifte.then ifte.true)) ifte
  where
  go acc curr = case curr.false of
    ExprIf next -> do
      let chain = ElseIfThen curr.else next.keyword next.cond next.then next.true
      go (NonEmptyArray.snoc acc chain) next
    expr ->
      NonEmptyArray.snoc acc (Else curr.else expr)

formatElseIfChain :: forall e a. Format (NonEmptyArray (ElseIfChain e)) e a
formatElseIfChain conf = flexGroup <<< joinWithMap spaceBreak case _ of
  IfThen kw1 cond kw2 expr ->
    case conf.thenPlacement of
      ThenSameLine ->
        formatToken conf kw1
          `flexSpaceBreak`
            indent (anchor (flexGroup (formatExpr conf cond)))
          `space`
            Hang.toFormatDoc (anchor (formatToken conf kw2) `hang` formatHangingExpr conf expr)
      ThenNewLine ->
        formatToken conf kw1
          `flexSpaceBreak`
            indent (anchor (flexGroup (formatExpr conf cond)))
          `break`
            Hang.toFormatDoc (anchor (formatToken conf kw2) `hang` formatHangingExpr conf expr)
  ElseIfThen kw1 kw2 cond kw3 expr ->
    case conf.thenPlacement of
      ThenSameLine ->
        formatToken conf kw1
          `space`
            indent (anchor (formatToken conf kw2))
          `flexSpaceBreak`
            indent (anchor (flexGroup (formatExpr conf cond)))
          `space`
            Hang.toFormatDoc (anchor (formatToken conf kw3) `hang` formatHangingExpr conf expr)
      ThenNewLine ->
        formatToken conf kw1
          `space`
            indent (anchor (formatToken conf kw2))
          `flexSpaceBreak`
            indent (anchor (flexGroup (formatExpr conf cond)))
          `break`
            Hang.toFormatDoc (anchor (formatToken conf kw3) `hang` formatHangingExpr conf expr)
  Else kw1 expr ->
    Hang.toFormatDoc (formatToken conf kw1 `hang` formatHangingExpr conf expr)

formatRecordUpdate :: forall e a. Format (RecordUpdate e) e a
formatRecordUpdate conf = case _ of
  RecordUpdateLeaf n t expr ->
    declareHanging (formatName conf n) space (formatToken conf t) (formatHangingExpr conf expr)
  RecordUpdateBranch n upd ->
    formatName conf n `flexSpaceBreak` indent do
      formatBasicListNonEmpty Grouped formatRecordUpdate conf upd

formatCaseBranch :: forall e a. Format (Tuple (Separated (Binder e)) (Guarded e)) e a
formatCaseBranch conf (Tuple (Separated { head, tail }) guarded) =
  case guarded of
    Unconditional tok (Where { expr, bindings }) ->
      caseBinders
        `space`
          Hang.toFormatDoc (formatToken conf tok `hang` formatHangingExpr conf expr)
        `break`
          indent (foldMap (formatWhere conf) bindings)

    Guarded guards ->
      if NonEmptyArray.length guards == 1 then
        Hang.toFormatDoc $ caseBinders `hang` formatGuardedExpr conf (NonEmptyArray.head guards)
      else
        caseBinders `flexSpaceBreak` indent do
          joinWithMap break (Hang.toFormatDoc <<< formatGuardedExpr conf) guards
  where
  caseBinders =
    flexGroup $ foldl
      ( \doc (Tuple a b) ->
          (doc <> indent (anchor (formatToken conf a)))
            `spaceBreak` flexGroup (formatBinder conf b)
      )
      (flexGroup (formatBinder conf head))
      tail

formatGuardedExpr :: forall e a. FormatHanging (GuardedExpr e) e a
formatGuardedExpr conf (GuardedExpr ge@{ patterns: Separated { head, tail }, where: Where { expr, bindings } }) =
  hangWithIndent (align 2 <<< indent)
    ( hangBreak
        ( formatToken conf ge.bar
            `space` flexGroup patternGuards
            `space` anchor (formatToken conf ge.separator)
        )
    )
    case bindings of
      Nothing ->
        [ formatHangingExpr conf expr ]
      Just wh ->
        [ formatHangingExpr conf expr
        , hangBreak $ formatWhere conf wh
        ]
  where
  patternGuards =
    formatListElem 2 formatPatternGuard conf head
      `softBreak` formatListTail 2 formatPatternGuard conf tail

formatPatternGuard :: forall e a. Format (PatternGuard e) e a
formatPatternGuard conf (PatternGuard { binder, expr }) = case binder of
  Nothing ->
    formatExpr conf expr
  Just (Tuple binder' t) ->
    formatBinder conf binder' `space` indent do
      anchor (formatToken conf t)
        `flexSpaceBreak` formatExpr conf expr

formatWhere :: forall e a. Format (Tuple SourceToken (NonEmptyArray (LetBinding e))) e a
formatWhere conf (Tuple kw bindings) =
  formatToken conf kw
    `break` formatLetGroups conf (NonEmptyArray.toArray bindings)

formatLetBinding :: forall e a. Format (LetBinding e) e a
formatLetBinding conf = case _ of
  LetBindingSignature (Labeled lbl) ->
    formatSignature conf $ Labeled lbl { label = formatName conf lbl.label }
  LetBindingName binding ->
    formatValueBinding conf binding
  LetBindingPattern binder tok (Where { expr, bindings }) ->
    flexGroup (formatBinder conf binder)
      `space`
        Hang.toFormatDoc (indent (anchor (formatToken conf tok)) `hang` formatHangingExpr conf expr)
      `break`
        indent (foldMap (formatWhere conf) bindings)

  LetBindingError e ->
    conf.formatError e

formatValueBinding :: forall e a. Format (ValueBindingFields e) e a
formatValueBinding conf { name, binders, guarded } =
  case guarded of
    Unconditional tok (Where { expr, bindings }) ->
      formatName conf name
        `flexSpaceBreak`
          indent do
            joinWithMap spaceBreak (anchor <<< formatBinder conf) binders
        `space`
          Hang.toFormatDoc (indent (anchor (formatToken conf tok)) `hang` formatHangingExpr conf expr)
        `break`
          indent (foldMap (formatWhere conf) bindings)

    Guarded guards ->
      if NonEmptyArray.length guards == 1 then
        Hang.toFormatDoc $ valBinders `hang` formatGuardedExpr conf (NonEmptyArray.head guards)
      else
        valBinders `flexSpaceBreak` indent do
          joinWithMap break (Hang.toFormatDoc <<< formatGuardedExpr conf) guards
      where
      valBinders =
        formatName conf name `flexSpaceBreak` indent do
          joinWithMap spaceBreak (anchor <<< flexGroup <<< formatBinder conf) binders

formatDoStatement :: forall e a. Format (DoStatement e) e a
formatDoStatement conf = case _ of
  DoLet kw bindings ->
    formatToken conf kw
      `flexSpaceBreak` indent (formatLetGroups conf (NonEmptyArray.toArray bindings))
  DoDiscard expr ->
    formatExpr conf expr
  DoBind binder tok expr ->
    flexGroup (formatBinder conf binder)
      `space` Hang.toFormatDoc do
        indent (anchor (formatToken conf tok)) `hang` formatHangingExpr conf expr
  DoError e ->
    conf.formatError e

formatBinder :: forall e a. Format (Binder e) e a
formatBinder conf = Hang.toFormatDoc <<< formatHangingBinder conf

formatHangingBinder :: forall e a. FormatHanging (Binder e) e a
formatHangingBinder conf = case _ of
  BinderWildcard t ->
    hangBreak $ formatToken conf t
  BinderVar n ->
    hangBreak $ formatName conf n
  BinderNamed n t b ->
    hangBreak $ formatName conf n <> (anchor (formatToken conf t) `flexSoftBreak` indent (formatBinder conf b))
  BinderConstructor n binders -> do
    let ctorName = hangBreak $ formatQualifiedName conf n
    case NonEmptyArray.fromArray binders of
      Nothing ->
        ctorName
      Just binders' ->
        hangApp ctorName (map (formatHangingBinder conf) binders')
  BinderBoolean t _ ->
    hangBreak $ formatToken conf t
  BinderChar t _ ->
    hangBreak $ formatToken conf t
  BinderString t _ ->
    hangBreak $ formatToken conf t
  BinderInt neg t _ ->
    hangBreak $ foldMap (formatToken conf) neg <> formatToken conf t
  BinderNumber neg t _ ->
    hangBreak $ foldMap (formatToken conf) neg <> formatToken conf t
  BinderArray binders ->
    hangBreak $ formatBasicList Grouped formatBinder conf binders
  BinderRecord binders ->
    hangBreak $ formatBasicList Grouped (formatRecordLabeled formatHangingBinder) conf binders
  BinderParens binder ->
    hangBreak $ formatParensBlock formatBinder conf binder
  BinderTyped binder separator ty ->
    hangBreak $ formatSignature conf $ Labeled
      { label: formatBinder conf binder
      , separator
      , value: ty
      }
  BinderOp binder binders ->
    formatHangingOperatorTree formatQualifiedName formatHangingBinder conf
      $ toQualifiedOperatorTree conf.operators OperatorValue binder binders
  BinderError e ->
    hangBreak $ conf.formatError e

formatRecordLabeled :: forall b e a. FormatHanging b e a -> Format (RecordLabeled b) e a
formatRecordLabeled format conf = case _ of
  RecordPun n ->
    formatName conf n
  RecordField label separator value ->
    declareHanging (formatName conf label) (<>) (anchor (formatToken conf separator)) (format conf value)

formatHangingOperatorTree :: forall e a b c. Format (QualifiedName b) e a -> FormatHanging c e a -> FormatHanging (OperatorTree (QualifiedName b) c) e a
formatHangingOperatorTree formatOperator format conf = go
  where
  go = case _ of
    OpPure a -> format conf a
    OpList head _ tail ->
      hangOps
        (go head)
        (map (\(Tuple op b) -> HangingOp (opWidth op) (formatOperator conf op) (go b)) tail)

  opWidth (QualifiedName { token }) =
    token.range.end.column - token.range.start.column

formatParens :: forall e a b. Format b e a -> Format (Wrapped b) e a
formatParens format conf (Wrapped { open, value, close }) =
  formatToken conf open
    <> anchor (format conf value)
    <> formatToken conf close

formatParensBlock :: forall e a b. Format b e a -> Format (Wrapped b) e a
formatParensBlock format conf (Wrapped { open, value, close }) =
  flexGroup $ formatToken conf open `softSpace` do
    align 2 (anchor (format conf value))
      `softBreak` formatToken conf close

formatBasicList :: forall e a b. FormatGrouped -> Format b e a -> Format (Delimited b) e a
formatBasicList = formatDelimited space spaceBreak 2

formatBasicListNonEmpty :: forall e a b. FormatGrouped -> Format b e a -> Format (DelimitedNonEmpty b) e a
formatBasicListNonEmpty = formatDelimitedNonEmpty space spaceBreak 2

formatParenList :: forall e a b. FormatGrouped -> Format b e a -> Format (Delimited b) e a
formatParenList = formatDelimited softSpace softBreak 2

formatParenListNonEmpty :: forall e a b. FormatGrouped -> Format b e a -> Format (DelimitedNonEmpty b) e a
formatParenListNonEmpty = formatDelimitedNonEmpty softSpace softBreak 2

formatDelimited :: forall e a b. FormatSpace a -> FormatSpace a -> Int -> FormatGrouped -> Format b e a -> Format (Delimited b) e a
formatDelimited openSpace closeSpace alignment grouped format conf (Wrapped { open, value, close }) = case value of
  Nothing ->
    formatEmptyList conf { open, close }
  Just (Separated { head, tail }) ->
    formatList openSpace closeSpace alignment grouped format conf { open, head, tail, close }

formatDelimitedNonEmpty :: forall e a b. FormatSpace a -> FormatSpace a -> Int -> FormatGrouped -> Format b e a -> Format (DelimitedNonEmpty b) e a
formatDelimitedNonEmpty openSpace closeSpace alignment grouped format conf (Wrapped { open, value: Separated { head, tail }, close }) =
  formatList openSpace closeSpace alignment grouped format conf { open, head, tail, close }

formatEmptyList :: forall e a. Format { open :: SourceToken, close :: SourceToken } e a
formatEmptyList conf { open, close } = formatToken conf open <> formatToken conf close

type FormatList b =
  { open :: SourceToken
  , head :: b
  , tail :: Array (Tuple SourceToken b)
  , close :: SourceToken
  }

data FormatGrouped = Grouped | NotGrouped

formatList :: forall e a b. FormatSpace a -> FormatSpace a -> Int -> FormatGrouped -> Format b e a -> Format (FormatList b) e a
formatList openSpace closeSpace alignment grouped format conf { open, head, tail, close } =
  case grouped of
    Grouped ->
      flexGroup $ formatToken conf open
        `openSpace` listElems
    NotGrouped ->
      formatToken conf open
        `openSpace` listElems
  where
  listElems =
    formatListElem alignment format conf head
      `softBreak`
        formatListTail alignment format conf tail
      `closeSpace`
        formatToken conf close

formatListElem :: forall e a b. Int -> Format b e a -> Format b e a
formatListElem alignment format conf b = flexGroup (align alignment (anchor (format conf b)))

formatListTail :: forall b e a. Int -> Format b e a -> Format (Array (Tuple SourceToken b)) e a
formatListTail alignment format conf =
  joinWithMap softBreak \(Tuple a b) ->
    formatToken conf a `space` formatListElem alignment format conf b

flatten :: forall a. Array (FormatDoc a) -> FormatDoc a
flatten = Array.uncons >>> foldMap format
  where
  format { head, tail } =
    head `space` indent do
      joinWithMap space anchor tail

declareHanging :: forall a. FormatDoc a -> FormatSpace a -> FormatDoc a -> HangingDoc a -> FormatDoc a
declareHanging label spc separator value =
  label `spc` Hang.toFormatDoc (indent separator `hang` value)

toQualifiedOperatorTree
  :: forall a
   . PrecedenceMap
  -> OperatorNamespace
  -> a
  -> NonEmptyArray (Tuple (QualifiedName Operator) a)
  -> OperatorTree (QualifiedName Operator) a
toQualifiedOperatorTree precMap opNs =
  toOperatorTree precMap \(QualifiedName qn) ->
    QualifiedOperator qn."module" opNs qn.name

data DeclGroup
  = DeclGroupValueSignature Ident
  | DeclGroupValue Ident
  | DeclGroupTypeSignature Proper
  | DeclGroupType Proper
  | DeclGroupClass Proper
  | DeclGroupInstance
  | DeclGroupFixity
  | DeclGroupForeign
  | DeclGroupRole
  | DeclGroupUnknown

data DeclGroupSeparator
  = DeclGroupSame
  | DeclGroupHard
  | DeclGroupSoft

formatTopLevelGroups :: forall e a. Format (Array (Declaration e)) e a
formatTopLevelGroups = formatDeclGroups topDeclGroupSeparator topDeclGroup formatDecl
  where
  topDeclGroupSeparator = case _, _ of
    DeclGroupValue a, DeclGroupValue b ->
      if a == b then DeclGroupSame
      else DeclGroupSoft
    DeclGroupValueSignature a, DeclGroupValue b ->
      if a == b then DeclGroupSame
      else DeclGroupHard
    _, DeclGroupValueSignature _ -> DeclGroupHard
    DeclGroupType _, DeclGroupType _ -> DeclGroupSoft
    DeclGroupTypeSignature a, DeclGroupType b ->
      if a == b then DeclGroupSame
      else DeclGroupHard
    DeclGroupTypeSignature a, DeclGroupClass b ->
      if a == b then DeclGroupSame
      else DeclGroupHard
    _, DeclGroupTypeSignature _ -> DeclGroupHard
    DeclGroupClass _, DeclGroupClass _ -> DeclGroupSoft
    _, DeclGroupClass _ -> DeclGroupHard
    DeclGroupInstance, DeclGroupInstance -> DeclGroupSoft
    _, DeclGroupInstance -> DeclGroupHard
    DeclGroupFixity, DeclGroupFixity -> DeclGroupSoft
    _, DeclGroupFixity -> DeclGroupHard
    DeclGroupForeign, DeclGroupForeign -> DeclGroupSoft
    _, DeclGroupForeign -> DeclGroupHard
    DeclGroupRole, DeclGroupRole -> DeclGroupSoft
    _, DeclGroupRole -> DeclGroupHard
    _, _ -> DeclGroupSoft

  topDeclGroup = case _ of
    DeclData { name: Name { name } } _ -> DeclGroupType name
    DeclType { name: Name { name } } _ _ -> DeclGroupType name
    DeclNewtype { name: Name { name } } _ _ _ -> DeclGroupType name
    DeclClass { name: Name { name } } _ -> DeclGroupClass name
    DeclKindSignature _ (Labeled { label: Name { name } }) -> DeclGroupTypeSignature name
    DeclSignature (Labeled { label: Name { name } }) -> DeclGroupValueSignature name
    DeclValue { name: Name { name } } -> DeclGroupValue name
    DeclInstanceChain _ -> DeclGroupInstance
    DeclDerive _ _ _ -> DeclGroupInstance
    DeclFixity _ -> DeclGroupFixity
    DeclForeign _ _ _ -> DeclGroupForeign
    DeclRole _ _ _ _ -> DeclGroupRole
    DeclError _ -> DeclGroupUnknown

formatLetGroups :: forall e a. Format (Array (LetBinding e)) e a
formatLetGroups = formatDeclGroups letDeclGroupSeparator letGroup formatLetBinding
  where
  letDeclGroupSeparator = case _, _ of
    _, DeclGroupValueSignature _ -> DeclGroupHard
    _, _ -> DeclGroupSame

  letGroup = case _ of
    LetBindingSignature (Labeled { label: Name { name } }) -> DeclGroupValueSignature name
    LetBindingName { name: Name { name } } -> DeclGroupValue name
    LetBindingPattern _ _ _ -> DeclGroupUnknown
    LetBindingError _ -> DeclGroupUnknown

formatDeclGroups
  :: forall e a b
   . (DeclGroup -> DeclGroup -> DeclGroupSeparator)
  -> (b -> DeclGroup)
  -> Format b e a
  -> Format (Array b) e a
formatDeclGroups declSeparator k format conf =
  maybe mempty joinDecls <<< foldr go Nothing
  where
  go decl = Just <<< case _ of
    Nothing ->
      { doc: mempty
      , sep: DeclGroupSame
      , group: k decl
      , decls: NonEmptyList.singleton decl
      }
    Just acc -> do
      let group = k decl
      case declSeparator group acc.group of
        DeclGroupSame ->
          { doc: acc.doc
          , sep: acc.sep
          , group
          , decls: NonEmptyList.cons decl acc.decls
          }
        sep ->
          { doc: joinDecls acc
          , sep
          , group
          , decls: NonEmptyList.singleton decl
          }

  joinDecls acc = case acc.sep of
    DeclGroupSame ->
      newDoc `break` acc.doc
    DeclGroupSoft ->
      newDoc `flexDoubleBreak` acc.doc
    DeclGroupHard ->
      newDoc `break` forceMinSourceBreaks 2 acc.doc
    where
    newDoc =
      joinWithMap break (format conf) acc.decls
