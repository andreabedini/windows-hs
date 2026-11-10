-- | Typed access to the metadata tables (ECMA-335 II.22).
module WinMD.Tables
  ( -- * Row types
    Row
  , rowDatabase
  , rowIndex
  , rowToken
  , rowsOf
  , Module
  , TypeRef
  , TypeDef
  , Field
  , MethodDef
  , Param
  , InterfaceImpl
  , MemberRef
  , Constant
  , CustomAttribute
  , ModuleRef
  , TypeSpec
  , ImplMap
  , AssemblyRef
  , GenericParam

    -- * Coded references
  , TypeDefOrRef (..)
  , ResolutionScope (..)
  , MemberRefParent (..)
  , CustomAttributeCtor (..)
  , HasAttributes

    -- * Module
  , moduleName

    -- * TypeRef
  , typeRefName
  , typeRefNamespace
  , typeRefScope

    -- * TypeDef
  , typeDefs
  , findTypeDef
  , findTypeDefs
  , resolveTypeRef
  , typeDefFlags
  , typeDefName
  , typeDefNamespace
  , typeDefExtends
  , typeDefFields
  , typeDefMethods
  , typeDefInterfaces
  , typeDefClassLayout
  , typeDefEnclosingType
  , typeDefNestedTypes
  , typeDefGenericParams

    -- * Field
  , fieldFlags
  , fieldName
  , fieldSignatureBlob
  , fieldConstant
  , fieldOffset
  , fieldParent

    -- * MethodDef
  , methodImplFlags
  , methodFlags
  , methodName
  , methodSignatureBlob
  , methodParams
  , methodImplMap
  , methodParent

    -- * Param
  , paramFlags
  , paramSequence
  , paramName
  , paramConstant

    -- * MemberRef
  , memberRefParent
  , memberRefName
  , memberRefSignatureBlob

    -- * TypeSpec
  , typeSpecSignatureBlob

    -- * ImplMap
  , implMapFlags
  , implMapImportName
  , implMapScope
  , moduleRefName

    -- * AssemblyRef
  , assemblyRefName

    -- * GenericParam
  , genericParamNumber
  , genericParamName

    -- * Constants
  , Value (..)
  , constantType
  , constantValue

    -- * Custom attributes
  , customAttributes
  , customAttributeCtor
  , customAttributeValueBlob
  , customAttributeTypeName

    -- * Names
  , typeDefOrRefName
  , typeDefOrRefFromEncoded
  , typeDefFullName

    -- * Flags
  , module WinMD.Tables.Flags
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf16LEWith)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import WinMD.Internal.Database
import WinMD.Internal.Parser (le16, le32, le64)
import WinMD.Tables.Flags

type Module = Row 'TModule

type TypeRef = Row 'TTypeRef

type TypeDef = Row 'TTypeDef

type Field = Row 'TField

type MethodDef = Row 'TMethodDef

type Param = Row 'TParam

type InterfaceImpl = Row 'TInterfaceImpl

type MemberRef = Row 'TMemberRef

type Constant = Row 'TConstant

type CustomAttribute = Row 'TCustomAttribute

type ModuleRef = Row 'TModuleRef

type TypeSpec = Row 'TTypeSpec

type ImplMap = Row 'TImplMap

type AssemblyRef = Row 'TAssemblyRef

type GenericParam = Row 'TGenericParam

------------------------------------------------------------------------------
-- Coded references

data TypeDefOrRef
  = IsTypeDef !TypeDef
  | IsTypeRef !TypeRef
  | IsTypeSpec !TypeSpec
  deriving stock (Eq, Ord, Show)

data ResolutionScope
  = ScopeModule !Module
  | ScopeModuleRef !ModuleRef
  | ScopeAssemblyRef !AssemblyRef
  | ScopeTypeRef !TypeRef
  deriving stock (Eq, Ord, Show)

data MemberRefParent
  = ParentTypeDef !TypeDef
  | ParentTypeRef !TypeRef
  | ParentModuleRef !ModuleRef
  | ParentMethodDef !MethodDef
  | ParentTypeSpec !TypeSpec
  deriving stock (Eq, Ord, Show)

-- | The constructor of a custom attribute.
data CustomAttributeCtor
  = CtorMethodDef !MethodDef
  | CtorMemberRef !MemberRef
  deriving stock (Eq, Ord, Show)

coded :: forall t x. (KnownTable t) => Coded -> Row t -> Int -> Maybe (Table, Row x)
coded c r col = do
  (t, i) <- decodeCoded c (column r col)
  pure (t, unsafeRow (rowDatabase r) i)

typeDefOrRef :: (KnownTable t) => Row t -> Int -> Maybe TypeDefOrRef
typeDefOrRef r col =
  coded TypeDefOrRef r col >>= \case
    (TTypeDef, x) -> Just (IsTypeDef (retag x))
    (TTypeRef, x) -> Just (IsTypeRef (retag x))
    (TTypeSpec, x) -> Just (IsTypeSpec (retag x))
    _ -> Nothing

retag :: Row a -> Row b
retag r = unsafeRow (rowDatabase r) (rowIndex r)

-- | Decode a TypeDefOrRefOrSpecEncoded value from a signature (II.23.2.8).
-- The tag and the 1-based index must be valid.
typeDefOrRefFromEncoded :: Database -> Word32 -> Either String TypeDefOrRef
typeDefOrRefFromEncoded db v = do
  let ix = fromIntegral (v `shiftR` 2) :: Int
  (t, mk) <- case v .&. 3 of
    0 -> Right (TTypeDef, IsTypeDef . unsafeRow db)
    1 -> Right (TTypeRef, IsTypeRef . unsafeRow db)
    2 -> Right (TTypeSpec, IsTypeSpec . unsafeRow db)
    tag -> Left ("invalid TypeDefOrRefOrSpecEncoded tag " ++ show tag)
  if ix >= 1 && ix <= rowCount db t
    then Right (mk (ix - 1))
    else Left ("TypeDefOrRefOrSpecEncoded index " ++ show ix ++ " is outside table " ++ tableName t)

-- | Tables whose rows can carry custom attributes.
class (KnownTable t) => HasAttributes t

instance HasAttributes 'TMethodDef

instance HasAttributes 'TField

instance HasAttributes 'TTypeRef

instance HasAttributes 'TTypeDef

instance HasAttributes 'TParam

instance HasAttributes 'TInterfaceImpl

instance HasAttributes 'TMemberRef

instance HasAttributes 'TModule

instance HasAttributes 'TDeclSecurity

instance HasAttributes 'TProperty

instance HasAttributes 'TEvent

instance HasAttributes 'TStandAloneSig

instance HasAttributes 'TModuleRef

instance HasAttributes 'TTypeSpec

instance HasAttributes 'TAssembly

instance HasAttributes 'TAssemblyRef

instance HasAttributes 'TFile

instance HasAttributes 'TExportedType

instance HasAttributes 'TManifestResource

instance HasAttributes 'TGenericParam

instance HasAttributes 'TGenericParamConstraint

instance HasAttributes 'TMethodSpec

-- | Rows of a table that refer to a given row through a coded index column.
referencing :: forall t. (KnownTable t) => Coded -> Table -> Int -> Row t -> [Int]
referencing c target col r = case encodeCoded c (tableOf @t) (rowIndex r) of
  Just v -> equalRange (rowDatabase r) target col v
  Nothing -> []

------------------------------------------------------------------------------
-- Module

moduleName :: Module -> Text
moduleName r = str r 1

------------------------------------------------------------------------------
-- TypeRef

typeRefName :: TypeRef -> Text
typeRefName r = str r 1

typeRefNamespace :: TypeRef -> Text
typeRefNamespace r = str r 2

typeRefScope :: TypeRef -> Maybe ResolutionScope
typeRefScope r =
  coded ResolutionScope r 0 >>= \case
    (TModule, x) -> Just (ScopeModule (retag x))
    (TModuleRef, x) -> Just (ScopeModuleRef (retag x))
    (TAssemblyRef, x) -> Just (ScopeAssemblyRef (retag x))
    (TTypeRef, x) -> Just (ScopeTypeRef (retag x))
    _ -> Nothing

------------------------------------------------------------------------------
-- TypeDef

typeDefs :: Database -> [TypeDef]
typeDefs = rowsOf

-- | Look up a top-level type definition by namespace and name. When a type
-- has several definitions (one per architecture), this returns the first;
-- use 'findTypeDefs' for all of them.
findTypeDef :: Database -> Text -> Text -> Maybe TypeDef
findTypeDef db ns name = listToMaybe (findTypeDefs db ns name)

-- | All top-level definitions of a type, in table order.
findTypeDefs :: Database -> Text -> Text -> [TypeDef]
findTypeDefs db ns name = map (unsafeRow db) (Map.findWithDefault [] (ns, name) (dbTypeIndex db))

-- | The definitions a type reference resolves to (II.22.38). A reference
-- scoped to another assembly resolves in the files linked with this one
-- ('WinMD.linkDatabases'); any other top-level reference in the same file.
-- A reference whose scope is another TypeRef names a nested type, and is
-- resolved inside the enclosing type's definitions.
resolveTypeRef :: TypeRef -> [TypeDef]
resolveTypeRef r = case typeRefScope r of
  Just (ScopeTypeRef enclosing) ->
    [ n
    | e <- resolveTypeRef enclosing
    , n <- typeDefNestedTypes e
    , typeDefName n == typeRefName r
    ]
  Just (ScopeAssemblyRef _) ->
    Map.findWithDefault [] (typeRefNamespace r, typeRefName r) (dbLinked (rowDatabase r))
  _ -> findTypeDefs (rowDatabase r) (typeRefNamespace r) (typeRefName r)

typeDefFlags :: TypeDef -> Word32
typeDefFlags r = column r 0

typeDefName :: TypeDef -> Text
typeDefName r = str r 1

typeDefNamespace :: TypeDef -> Text
typeDefNamespace r = str r 2

typeDefExtends :: TypeDef -> Maybe TypeDefOrRef
typeDefExtends r = typeDefOrRef r 3

typeDefFields :: TypeDef -> [Field]
typeDefFields r = list r 4 TField

typeDefMethods :: TypeDef -> [MethodDef]
typeDefMethods r = list r 5 TMethodDef

-- | The interfaces a type implements (or an interface requires).
typeDefInterfaces :: TypeDef -> [TypeDefOrRef]
typeDefInterfaces r =
  [ i
  | ix <- equalRange db TInterfaceImpl 0 (fromIntegral (rowIndex r + 1))
  , Just i <- [typeDefOrRef (unsafeRow db ix :: InterfaceImpl) 1]
  ]
  where
    db = rowDatabase r

-- | Packing size and class size, from the ClassLayout table.
typeDefClassLayout :: TypeDef -> Maybe (Word16, Word32)
typeDefClassLayout r = do
  ix <- listToMaybe (equalRange db TClassLayout 2 (fromIntegral (rowIndex r + 1)))
  pure (fromIntegral (rawColumn db TClassLayout ix 0), rawColumn db TClassLayout ix 1)
  where
    db = rowDatabase r

typeDefEnclosingType :: TypeDef -> Maybe TypeDef
typeDefEnclosingType r = do
  ix <- listToMaybe (equalRange db TNestedClass 0 (fromIntegral (rowIndex r + 1)))
  pure (unsafeRow db (fromIntegral (rawColumn db TNestedClass ix 1) - 1))
  where
    db = rowDatabase r

typeDefNestedTypes :: TypeDef -> [TypeDef]
typeDefNestedTypes r =
  map (unsafeRow db) (IntMap.findWithDefault [] (rowIndex r) (dbNestedIndex db))
  where
    db = rowDatabase r

typeDefGenericParams :: TypeDef -> [GenericParam]
typeDefGenericParams r =
  map (unsafeRow (rowDatabase r)) (referencing TypeOrMethodDef TGenericParam 2 r)

------------------------------------------------------------------------------
-- Field

fieldFlags :: Field -> Word16
fieldFlags r = fromIntegral (column r 0)

fieldName :: Field -> Text
fieldName r = str r 1

fieldSignatureBlob :: Field -> ByteString
fieldSignatureBlob r = blob r 2

fieldConstant :: Field -> Maybe Constant
fieldConstant r = listToMaybe (map (unsafeRow (rowDatabase r)) (referencing HasConstant TConstant 2 r))

-- | Explicit field offset, from the FieldLayout table.
fieldOffset :: Field -> Maybe Word32
fieldOffset r = do
  ix <- listToMaybe (equalRange db TFieldLayout 1 (fromIntegral (rowIndex r + 1)))
  pure (rawColumn db TFieldLayout ix 0)
  where
    db = rowDatabase r

-- | The type that declares a field.
fieldParent :: Field -> Maybe TypeDef
fieldParent r = owner TTypeDef 4 r

------------------------------------------------------------------------------
-- MethodDef

methodImplFlags :: MethodDef -> Word16
methodImplFlags r = fromIntegral (column r 1)

methodFlags :: MethodDef -> Word16
methodFlags r = fromIntegral (column r 2)

methodName :: MethodDef -> Text
methodName r = str r 3

methodSignatureBlob :: MethodDef -> ByteString
methodSignatureBlob r = blob r 4

-- | The Param rows of a method. Sequence 0, when present, describes the
-- return value; the parameters are numbered from 1.
methodParams :: MethodDef -> [Param]
methodParams r = list r 5 TParam

-- | The P/Invoke import of a method, if any.
methodImplMap :: MethodDef -> Maybe ImplMap
methodImplMap r = listToMaybe (map (unsafeRow (rowDatabase r)) (referencing MemberForwarded TImplMap 1 r))

-- | The type that declares a method.
methodParent :: MethodDef -> Maybe TypeDef
methodParent r = owner TTypeDef 5 r

-- | The owner of a row in a list column, found by binary search over the
-- owner table's list column.
owner :: Table -> Int -> Row t -> Maybe (Row o)
owner ownerTable col r =
  unsafeRow db <$> lastRowWhere db ownerTable col (fromIntegral (rowIndex r + 1))
  where
    db = rowDatabase r

------------------------------------------------------------------------------
-- Param

paramFlags :: Param -> Word16
paramFlags r = fromIntegral (column r 0)

paramSequence :: Param -> Word16
paramSequence r = fromIntegral (column r 1)

paramName :: Param -> Text
paramName r = str r 2

paramConstant :: Param -> Maybe Constant
paramConstant r = listToMaybe (map (unsafeRow (rowDatabase r)) (referencing HasConstant TConstant 2 r))

------------------------------------------------------------------------------
-- MemberRef

memberRefParent :: MemberRef -> Maybe MemberRefParent
memberRefParent r =
  coded MemberRefParent r 0 >>= \case
    (TTypeDef, x) -> Just (ParentTypeDef (retag x))
    (TTypeRef, x) -> Just (ParentTypeRef (retag x))
    (TModuleRef, x) -> Just (ParentModuleRef (retag x))
    (TMethodDef, x) -> Just (ParentMethodDef (retag x))
    (TTypeSpec, x) -> Just (ParentTypeSpec (retag x))
    _ -> Nothing

memberRefName :: MemberRef -> Text
memberRefName r = str r 1

memberRefSignatureBlob :: MemberRef -> ByteString
memberRefSignatureBlob r = blob r 2

------------------------------------------------------------------------------
-- TypeSpec

typeSpecSignatureBlob :: TypeSpec -> ByteString
typeSpecSignatureBlob r = blob r 0

------------------------------------------------------------------------------
-- ImplMap and ModuleRef

implMapFlags :: ImplMap -> Word16
implMapFlags r = fromIntegral (column r 0)

implMapImportName :: ImplMap -> Text
implMapImportName r = str r 2

implMapScope :: ImplMap -> ModuleRef
implMapScope r = unsafeRow (rowDatabase r) (fromIntegral (column r 3) - 1)

moduleRefName :: ModuleRef -> Text
moduleRefName r = str r 0

------------------------------------------------------------------------------
-- AssemblyRef

assemblyRefName :: AssemblyRef -> Text
assemblyRefName r = str r 6

------------------------------------------------------------------------------
-- GenericParam

genericParamNumber :: GenericParam -> Word16
genericParamNumber r = fromIntegral (column r 0)

genericParamName :: GenericParam -> Text
genericParamName r = str r 3

------------------------------------------------------------------------------
-- Constants

-- | A constant value (II.22.9), also used for primitive values in custom
-- attributes.
data Value
  = VBool !Bool
  | VChar !Char
  | VInt8 !Int8
  | VUInt8 !Word8
  | VInt16 !Int16
  | VUInt16 !Word16
  | VInt32 !Int32
  | VUInt32 !Word32
  | VInt64 !Int64
  | VUInt64 !Word64
  | VFloat32 !Float
  | VFloat64 !Double
  | VString !Text
  | -- | A null class reference.
    VNull
  deriving stock (Eq, Show)

-- | The element type code of a constant.
constantType :: Constant -> Word8
constantType r = fromIntegral (column r 0)

-- | Decode a constant. Throws 'WinMDError' if the value blob does not match
-- its type.
constantValue :: Constant -> Value
constantValue r =
  either (\e -> throwWinMD (show r ++ ": " ++ e)) id (decodeValue (constantType r) (blob r 3))

decodeValue :: Word8 -> ByteString -> Either String Value
decodeValue ty b = case ty of
  0x02 -> sized 1 (VBool (BS.head b /= 0))
  0x03 -> sized 2 (VChar (toEnum (fromIntegral (le16 b 0))))
  0x04 -> sized 1 (VInt8 (fromIntegral (BS.head b)))
  0x05 -> sized 1 (VUInt8 (BS.head b))
  0x06 -> sized 2 (VInt16 (fromIntegral (le16 b 0)))
  0x07 -> sized 2 (VUInt16 (le16 b 0))
  0x08 -> sized 4 (VInt32 (fromIntegral (le32 b 0)))
  0x09 -> sized 4 (VUInt32 (le32 b 0))
  0x0A -> sized 8 (VInt64 (fromIntegral (le64 b 0)))
  0x0B -> sized 8 (VUInt64 (le64 b 0))
  0x0C -> sized 4 (VFloat32 (castWord32ToFloat (le32 b 0)))
  0x0D -> sized 8 (VFloat64 (castWord64ToDouble (le64 b 0)))
  0x0E -> Right (VString (decodeUtf16LEWith lenientDecode b))
  0x12 -> Right VNull
  _ -> Left ("unknown constant type " ++ show ty)
  where
    sized :: Int -> Value -> Either String Value
    sized n v
      | BS.length b == n = Right v
      | otherwise = Left ("constant of type " ++ show ty ++ " has " ++ show (BS.length b) ++ " bytes, expected " ++ show n)

------------------------------------------------------------------------------
-- Custom attributes

-- | The custom attributes attached to a row.
customAttributes :: forall t. (HasAttributes t) => Row t -> [CustomAttribute]
customAttributes r = map (unsafeRow (rowDatabase r)) (referencing HasCustomAttribute TCustomAttribute 0 r)

customAttributeCtor :: CustomAttribute -> Maybe CustomAttributeCtor
customAttributeCtor r =
  coded CustomAttributeType r 1 >>= \case
    (TMethodDef, x) -> Just (CtorMethodDef (retag x))
    (TMemberRef, x) -> Just (CtorMemberRef (retag x))
    _ -> Nothing

customAttributeValueBlob :: CustomAttribute -> ByteString
customAttributeValueBlob r = blob r 2

-- | The (namespace, name) of an attribute's type.
customAttributeTypeName :: CustomAttribute -> Maybe (Text, Text)
customAttributeTypeName r =
  customAttributeCtor r >>= \case
    CtorMethodDef m -> nameOfDef <$> methodParent m
    CtorMemberRef m ->
      memberRefParent m >>= \case
        ParentTypeRef t -> Just (typeRefNamespace t, typeRefName t)
        ParentTypeDef t -> Just (nameOfDef t)
        _ -> Nothing
  where
    nameOfDef t = (typeDefNamespace t, typeDefName t)

------------------------------------------------------------------------------
-- Names

-- | The (namespace, name) of a type reference or definition. Type specs have
-- no name.
typeDefOrRefName :: TypeDefOrRef -> Maybe (Text, Text)
typeDefOrRefName = \case
  IsTypeDef t -> Just (typeDefNamespace t, typeDefName t)
  IsTypeRef t -> Just (typeRefNamespace t, typeRefName t)
  IsTypeSpec _ -> Nothing

-- | The dotted name of a type definition. Nested types are written
-- @Enclosing/Nested@.
typeDefFullName :: TypeDef -> Text
typeDefFullName t = case typeDefEnclosingType t of
  Just e -> typeDefFullName e <> "/" <> typeDefName t
  Nothing
    | T.null (typeDefNamespace t) -> typeDefName t
    | otherwise -> typeDefNamespace t <> "." <> typeDefName t

------------------------------------------------------------------------------
-- Helpers

str :: (KnownTable t) => Row t -> Int -> Text
str r c = heapString (rowDatabase r) (column r c)

blob :: (KnownTable t) => Row t -> Int -> ByteString
blob r c = heapBlob (rowDatabase r) (column r c)

list :: forall t x. (KnownTable t) => Row t -> Int -> Table -> [Row x]
list r c target = map (unsafeRow db) (listRange db (tableOf @t) c target (rowIndex r))
  where
    db = rowDatabase r
