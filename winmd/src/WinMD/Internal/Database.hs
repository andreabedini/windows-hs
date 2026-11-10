{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The physical layer of an ECMA-335 metadata file: streams, heaps, the
-- table schema (II.22) and raw column access.
--
-- 'loadDatabase' checks every heap and table reference in every row once,
-- so the accessors here can read columns and heaps without further bounds
-- checks.
module WinMD.Internal.Database
  ( -- * Tables
    Table (..)
  , tableName
  , KnownTable (..)
  , tableOf

    -- * Schema
  , Coded (..)
  , codedTables
  , codedBits
  , decodeCoded
  , encodeCoded
  , ColType (..)
  , schema

    -- * Databases
  , Database (..)
  , Layout (..)
  , loadDatabase
  , linkDatabases
  , rowCount

    -- * Rows
  , Row (..)
  , rowToken
  , rowsOf
  , unsafeRow

    -- * Raw access
  , rawColumn
  , column
  , heapString
  , heapBlob
  , heapGuid
  , equalRange
  , listRange
  , lastRowWhere

    -- * Errors
  , WinMDError (..)
  , throwWinMD
  ) where

import Control.Exception (Exception, throw)
import Control.Monad (forM_, unless, when)
import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U
import Data.Bits (popCount, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word32, Word64, Word8)
import WinMD.Internal.PE (metadataBlob)
import WinMD.Internal.Parser

-- | An error in a metadata file. Raised by 'loadDatabase' as a 'Left', and
-- thrown by accessors that decode blobs (signatures, constants, attributes)
-- when the blob is malformed.
newtype WinMDError = WinMDError String
  deriving stock (Show)

instance Exception WinMDError

throwWinMD :: String -> a
throwWinMD = throw . WinMDError

------------------------------------------------------------------------------
-- Tables

-- | The metadata tables, in table-number order (II.22).
data Table
  = TModule
  | TTypeRef
  | TTypeDef
  | TFieldPtr
  | TField
  | TMethodPtr
  | TMethodDef
  | TParamPtr
  | TParam
  | TInterfaceImpl
  | TMemberRef
  | TConstant
  | TCustomAttribute
  | TFieldMarshal
  | TDeclSecurity
  | TClassLayout
  | TFieldLayout
  | TStandAloneSig
  | TEventMap
  | TEventPtr
  | TEvent
  | TPropertyMap
  | TPropertyPtr
  | TProperty
  | TMethodSemantics
  | TMethodImpl
  | TModuleRef
  | TTypeSpec
  | TImplMap
  | TFieldRVA
  | TEncLog
  | TEncMap
  | TAssembly
  | TAssemblyProcessor
  | TAssemblyOS
  | TAssemblyRef
  | TAssemblyRefProcessor
  | TAssemblyRefOS
  | TFile
  | TExportedType
  | TManifestResource
  | TNestedClass
  | TGenericParam
  | TMethodSpec
  | TGenericParamConstraint
  deriving stock (Eq, Ord, Show, Enum, Bounded, Ix)

tableName :: Table -> String
tableName = drop 1 . show

-- | Tables known at the type level, so that rows can be typed by table.
class KnownTable (t :: Table) where
  tableSing :: Proxy t -> Table

tableOf :: forall t. (KnownTable t) => Table
tableOf = tableSing (Proxy :: Proxy t)

instance KnownTable 'TModule where tableSing _ = TModule
instance KnownTable 'TTypeRef where tableSing _ = TTypeRef
instance KnownTable 'TTypeDef where tableSing _ = TTypeDef
instance KnownTable 'TFieldPtr where tableSing _ = TFieldPtr
instance KnownTable 'TField where tableSing _ = TField
instance KnownTable 'TMethodPtr where tableSing _ = TMethodPtr
instance KnownTable 'TMethodDef where tableSing _ = TMethodDef
instance KnownTable 'TParamPtr where tableSing _ = TParamPtr
instance KnownTable 'TParam where tableSing _ = TParam
instance KnownTable 'TInterfaceImpl where tableSing _ = TInterfaceImpl
instance KnownTable 'TMemberRef where tableSing _ = TMemberRef
instance KnownTable 'TConstant where tableSing _ = TConstant
instance KnownTable 'TCustomAttribute where tableSing _ = TCustomAttribute
instance KnownTable 'TFieldMarshal where tableSing _ = TFieldMarshal
instance KnownTable 'TDeclSecurity where tableSing _ = TDeclSecurity
instance KnownTable 'TClassLayout where tableSing _ = TClassLayout
instance KnownTable 'TFieldLayout where tableSing _ = TFieldLayout
instance KnownTable 'TStandAloneSig where tableSing _ = TStandAloneSig
instance KnownTable 'TEventMap where tableSing _ = TEventMap
instance KnownTable 'TEventPtr where tableSing _ = TEventPtr
instance KnownTable 'TEvent where tableSing _ = TEvent
instance KnownTable 'TPropertyMap where tableSing _ = TPropertyMap
instance KnownTable 'TPropertyPtr where tableSing _ = TPropertyPtr
instance KnownTable 'TProperty where tableSing _ = TProperty
instance KnownTable 'TMethodSemantics where tableSing _ = TMethodSemantics
instance KnownTable 'TMethodImpl where tableSing _ = TMethodImpl
instance KnownTable 'TModuleRef where tableSing _ = TModuleRef
instance KnownTable 'TTypeSpec where tableSing _ = TTypeSpec
instance KnownTable 'TImplMap where tableSing _ = TImplMap
instance KnownTable 'TFieldRVA where tableSing _ = TFieldRVA
instance KnownTable 'TEncLog where tableSing _ = TEncLog
instance KnownTable 'TEncMap where tableSing _ = TEncMap
instance KnownTable 'TAssembly where tableSing _ = TAssembly
instance KnownTable 'TAssemblyProcessor where tableSing _ = TAssemblyProcessor
instance KnownTable 'TAssemblyOS where tableSing _ = TAssemblyOS
instance KnownTable 'TAssemblyRef where tableSing _ = TAssemblyRef
instance KnownTable 'TAssemblyRefProcessor where tableSing _ = TAssemblyRefProcessor
instance KnownTable 'TAssemblyRefOS where tableSing _ = TAssemblyRefOS
instance KnownTable 'TFile where tableSing _ = TFile
instance KnownTable 'TExportedType where tableSing _ = TExportedType
instance KnownTable 'TManifestResource where tableSing _ = TManifestResource
instance KnownTable 'TNestedClass where tableSing _ = TNestedClass
instance KnownTable 'TGenericParam where tableSing _ = TGenericParam
instance KnownTable 'TMethodSpec where tableSing _ = TMethodSpec
instance KnownTable 'TGenericParamConstraint where tableSing _ = TGenericParamConstraint

------------------------------------------------------------------------------
-- Schema

-- | Coded index kinds (II.24.2.6).
data Coded
  = TypeDefOrRef
  | HasConstant
  | HasCustomAttribute
  | HasFieldMarshal
  | HasDeclSecurity
  | MemberRefParent
  | HasSemantics
  | MethodDefOrRef
  | MemberForwarded
  | Implementation
  | CustomAttributeType
  | ResolutionScope
  | TypeOrMethodDef
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The tables a coded index can refer to, by tag. 'Nothing' marks a tag
-- that is reserved.
codedTables :: Coded -> [Maybe Table]
codedTables = \case
  TypeDefOrRef -> map Just [TTypeDef, TTypeRef, TTypeSpec]
  HasConstant -> map Just [TField, TParam, TProperty]
  HasCustomAttribute ->
    map
      Just
      [ TMethodDef
      , TField
      , TTypeRef
      , TTypeDef
      , TParam
      , TInterfaceImpl
      , TMemberRef
      , TModule
      , TDeclSecurity
      , TProperty
      , TEvent
      , TStandAloneSig
      , TModuleRef
      , TTypeSpec
      , TAssembly
      , TAssemblyRef
      , TFile
      , TExportedType
      , TManifestResource
      , TGenericParam
      , TGenericParamConstraint
      , TMethodSpec
      ]
  HasFieldMarshal -> map Just [TField, TParam]
  HasDeclSecurity -> map Just [TTypeDef, TMethodDef, TAssembly]
  MemberRefParent -> map Just [TTypeDef, TTypeRef, TModuleRef, TMethodDef, TTypeSpec]
  HasSemantics -> map Just [TEvent, TProperty]
  MethodDefOrRef -> map Just [TMethodDef, TMemberRef]
  MemberForwarded -> map Just [TField, TMethodDef]
  Implementation -> map Just [TFile, TAssemblyRef, TExportedType]
  CustomAttributeType -> [Nothing, Nothing, Just TMethodDef, Just TMemberRef, Nothing]
  ResolutionScope -> map Just [TModule, TModuleRef, TAssemblyRef, TTypeRef]
  TypeOrMethodDef -> map Just [TTypeDef, TMethodDef]

-- | Number of tag bits in a coded index.
codedBits :: Coded -> Int
codedBits c = length (takeWhile (< length (codedTables c)) (iterate (* 2) 1))

-- | Split a coded index into its table and 0-based row. 'Nothing' for a
-- null reference. The tag must be valid, which 'loadDatabase' checks.
decodeCoded :: Coded -> Word32 -> Maybe (Table, Int)
decodeCoded c v
  | ix == 0 = Nothing
  | otherwise = case drop tag (codedTables c) of
      Just t : _ -> Just (t, ix - 1)
      _ -> Nothing
  where
    bits = codedBits c
    tag = fromIntegral (v .&. ((1 `shiftL` bits) - 1))
    ix = fromIntegral (v `shiftR` bits)

-- | Encode a reference to a 0-based row as a coded index value.
encodeCoded :: Coded -> Table -> Int -> Maybe Word32
encodeCoded c t i = do
  tag <- lookup (Just t) (zip (codedTables c) [0 ..])
  pure ((fromIntegral (i + 1) `shiftL` codedBits c) .|. tag)

-- | Column types.
data ColType
  = U8
  | U16
  | U32
  | StrIdx
  | GuidIdx
  | BlobIdx
  | Idx Table
  | CodedIdx Coded
  deriving stock (Eq, Show)

-- | The columns of each table (II.22), with their names.
schema :: Table -> [(String, ColType)]
schema = \case
  TModule -> [("Generation", U16), ("Name", StrIdx), ("Mvid", GuidIdx), ("EncId", GuidIdx), ("EncBaseId", GuidIdx)]
  TTypeRef -> [("ResolutionScope", CodedIdx ResolutionScope), ("TypeName", StrIdx), ("TypeNamespace", StrIdx)]
  TTypeDef ->
    [ ("Flags", U32)
    , ("TypeName", StrIdx)
    , ("TypeNamespace", StrIdx)
    , ("Extends", CodedIdx TypeDefOrRef)
    , ("FieldList", Idx TField)
    , ("MethodList", Idx TMethodDef)
    ]
  TFieldPtr -> [("Field", Idx TField)]
  TField -> [("Flags", U16), ("Name", StrIdx), ("Signature", BlobIdx)]
  TMethodPtr -> [("Method", Idx TMethodDef)]
  TMethodDef ->
    [ ("RVA", U32)
    , ("ImplFlags", U16)
    , ("Flags", U16)
    , ("Name", StrIdx)
    , ("Signature", BlobIdx)
    , ("ParamList", Idx TParam)
    ]
  TParamPtr -> [("Param", Idx TParam)]
  TParam -> [("Flags", U16), ("Sequence", U16), ("Name", StrIdx)]
  TInterfaceImpl -> [("Class", Idx TTypeDef), ("Interface", CodedIdx TypeDefOrRef)]
  TMemberRef -> [("Class", CodedIdx MemberRefParent), ("Name", StrIdx), ("Signature", BlobIdx)]
  TConstant -> [("Type", U8), ("Padding", U8), ("Parent", CodedIdx HasConstant), ("Value", BlobIdx)]
  TCustomAttribute -> [("Parent", CodedIdx HasCustomAttribute), ("Type", CodedIdx CustomAttributeType), ("Value", BlobIdx)]
  TFieldMarshal -> [("Parent", CodedIdx HasFieldMarshal), ("NativeType", BlobIdx)]
  TDeclSecurity -> [("Action", U16), ("Parent", CodedIdx HasDeclSecurity), ("PermissionSet", BlobIdx)]
  TClassLayout -> [("PackingSize", U16), ("ClassSize", U32), ("Parent", Idx TTypeDef)]
  TFieldLayout -> [("Offset", U32), ("Field", Idx TField)]
  TStandAloneSig -> [("Signature", BlobIdx)]
  TEventMap -> [("Parent", Idx TTypeDef), ("EventList", Idx TEvent)]
  TEventPtr -> [("Event", Idx TEvent)]
  TEvent -> [("EventFlags", U16), ("Name", StrIdx), ("EventType", CodedIdx TypeDefOrRef)]
  TPropertyMap -> [("Parent", Idx TTypeDef), ("PropertyList", Idx TProperty)]
  TPropertyPtr -> [("Property", Idx TProperty)]
  TProperty -> [("Flags", U16), ("Name", StrIdx), ("Type", BlobIdx)]
  TMethodSemantics -> [("Semantics", U16), ("Method", Idx TMethodDef), ("Association", CodedIdx HasSemantics)]
  TMethodImpl ->
    [ ("Class", Idx TTypeDef)
    , ("MethodBody", CodedIdx MethodDefOrRef)
    , ("MethodDeclaration", CodedIdx MethodDefOrRef)
    ]
  TModuleRef -> [("Name", StrIdx)]
  TTypeSpec -> [("Signature", BlobIdx)]
  TImplMap ->
    [ ("MappingFlags", U16)
    , ("MemberForwarded", CodedIdx MemberForwarded)
    , ("ImportName", StrIdx)
    , ("ImportScope", Idx TModuleRef)
    ]
  TFieldRVA -> [("RVA", U32), ("Field", Idx TField)]
  TEncLog -> [("Token", U32), ("FuncCode", U32)]
  TEncMap -> [("Token", U32)]
  TAssembly ->
    [ ("HashAlgId", U32)
    , ("MajorVersion", U16)
    , ("MinorVersion", U16)
    , ("BuildNumber", U16)
    , ("RevisionNumber", U16)
    , ("Flags", U32)
    , ("PublicKey", BlobIdx)
    , ("Name", StrIdx)
    , ("Culture", StrIdx)
    ]
  TAssemblyProcessor -> [("Processor", U32)]
  TAssemblyOS -> [("OSPlatformID", U32), ("OSMajorVersion", U32), ("OSMinorVersion", U32)]
  TAssemblyRef ->
    [ ("MajorVersion", U16)
    , ("MinorVersion", U16)
    , ("BuildNumber", U16)
    , ("RevisionNumber", U16)
    , ("Flags", U32)
    , ("PublicKeyOrToken", BlobIdx)
    , ("Name", StrIdx)
    , ("Culture", StrIdx)
    , ("HashValue", BlobIdx)
    ]
  TAssemblyRefProcessor -> [("Processor", U32), ("AssemblyRef", Idx TAssemblyRef)]
  TAssemblyRefOS ->
    [ ("OSPlatformId", U32)
    , ("OSMajorVersion", U32)
    , ("OSMinorVersion", U32)
    , ("AssemblyRef", Idx TAssemblyRef)
    ]
  TFile -> [("Flags", U32), ("Name", StrIdx), ("HashValue", BlobIdx)]
  TExportedType ->
    [ ("Flags", U32)
    , ("TypeDefId", U32)
    , ("TypeName", StrIdx)
    , ("TypeNamespace", StrIdx)
    , ("Implementation", CodedIdx Implementation)
    ]
  TManifestResource -> [("Offset", U32), ("Flags", U32), ("Name", StrIdx), ("Implementation", CodedIdx Implementation)]
  TNestedClass -> [("NestedClass", Idx TTypeDef), ("EnclosingClass", Idx TTypeDef)]
  TGenericParam -> [("Number", U16), ("Flags", U16), ("Owner", CodedIdx TypeOrMethodDef), ("Name", StrIdx)]
  TMethodSpec -> [("Method", CodedIdx MethodDefOrRef), ("Instantiation", BlobIdx)]
  TGenericParamConstraint -> [("Owner", Idx TGenericParam), ("Constraint", CodedIdx TypeDefOrRef)]

------------------------------------------------------------------------------
-- Database

-- | Where a table lives in the @#~@ stream and how its rows are laid out.
data Layout = Layout
  { layoutRows :: !Int
  , layoutOffset :: !Int
  , layoutRowSize :: !Int
  , layoutColOffsets :: !(UArray Int Int)
  , layoutColWidths :: !(UArray Int Int)
  }

-- | A loaded metadata file.
data Database = Database
  { dbTables :: !ByteString
  -- ^ The @#~@ stream.
  , dbStrings :: !ByteString
  , dbBlobs :: !ByteString
  , dbGuids :: !ByteString
  , dbLayouts :: !(Array Table Layout)
  , dbSorted :: !Word64
  , dbMvid :: !ByteString
  -- ^ The module's GUID, used to tell rows of different files apart.
  , dbTypeIndex :: Map (Text, Text) [Int]
  -- ^ Top-level type definitions by (namespace, name), in table order.
  -- win32metadata defines some types once per architecture, so a name can
  -- have several definitions. Built on first use.
  , dbNestedIndex :: IntMap [Int]
  -- ^ Nested type definitions by enclosing type. Built on first use.
  , dbLinked :: Map (Text, Text) [Row 'TTypeDef]
  -- ^ Top-level type definitions of this file and of the files linked with
  -- it ('linkDatabases'), in file order and then table order. Built on
  -- first use.
  }

rowCount :: Database -> Table -> Int
rowCount db t = layoutRows (dbLayouts db ! t)

-- | Parse and validate a metadata file (a PE image containing CLI
-- metadata).
loadDatabase :: ByteString -> Either String Database
loadDatabase file = do
  md <- metadataBlob file
  streams <- runParser metadataRoot md
  let stream name = case find ((== name) . fst) streams of
        Just (_, (off, size))
          | off + size <= BS.length md -> Right (BS.take size (BS.drop off md))
          | otherwise -> Left ("stream " ++ show name ++ " extends past the end of the metadata")
        Nothing -> Right BS.empty
  when (any ((== "#-") . fst) streams) $
    Left "uncompressed (#-) metadata tables are not supported"
  unless (any ((== "#~") . fst) streams) $ Left "metadata has no #~ stream"
  tables <- stream "#~"
  strings <- stream "#Strings"
  blobs <- stream "#Blob"
  guids <- stream "#GUID"
  (heapSizes, valid, sorted, counts, start) <- runParser tablesHeader tables
  let rowsOfTable t = maybe 0 id (lookup t counts)
      layouts = buildLayouts heapSizes rowsOfTable start
      end = case [layoutOffset l + layoutRows l * layoutRowSize l | l <- foldr (:) [] layouts] of
        [] -> start
        xs -> maximum xs
  when (testBitsAbove valid) $
    Left "metadata has tables this reader does not know (table number > 0x2C)"
  when (end > BS.length tables) $
    Left ("metadata tables need " ++ show end ++ " bytes but the #~ stream has " ++ show (BS.length tables))
  unless (BS.null strings || BS.last strings == 0) $
    Left "#Strings heap does not end with a NUL byte"
  let db0 =
        Database
          { dbTables = tables
          , dbStrings = strings
          , dbBlobs = blobs
          , dbGuids = guids
          , dbLayouts = layouts
          , dbSorted = sorted
          , dbMvid = BS.empty
          , dbTypeIndex = Map.empty
          , dbNestedIndex = IntMap.empty
          , dbLinked = Map.empty
          }
  validate db0
  let mvid
        | rowCount db0 TModule > 0 = heapGuid db0 (rawColumn db0 TModule 0 2)
        | otherwise = BS.empty
      db =
        db0
          { dbMvid = mvid
          , dbTypeIndex = buildTypeIndex db0
          , dbNestedIndex = buildNestedIndex db0
          }
      self = db {dbLinked = Map.map (map (Row self)) (dbTypeIndex db)}
  pure self
  where
    testBitsAbove v = v `shiftR` (fromEnum (maxBound :: Table) + 1) /= 0

-- | Parse the metadata root (II.24.2.1) and return the stream headers as
-- (name, (offset, size)).
metadataRoot :: Parser [(ByteString, (Int, Int))]
metadataRoot = do
  sig <- u32
  unless (sig == 0x424A5342) $ parseError "bad metadata signature"
  skip 8
  versionLength <- u32
  skip (fromIntegral versionLength)
  _flags <- u16
  n <- u16
  sequence (replicate (fromIntegral n) streamHeader)
  where
    streamHeader = do
      off <- u32
      size <- u32
      name <- nulTerminated
      align 4
      pure (name, (fromIntegral off, fromIntegral size))
    nulTerminated = go []
      where
        go acc = do
          b <- u8
          if b == 0 then pure (BS.pack (reverse acc)) else go (b : acc)

-- | Parse the @#~@ stream header (II.24.2.6).
tablesHeader :: Parser (Word8, Word64, Word64, [(Table, Int)], Int)
tablesHeader = do
  skip 4
  _major <- u8
  _minor <- u8
  heapSizes <- u8
  skip 1
  valid <- u64
  sorted <- u64
  let present = [t | t <- [minBound .. maxBound], testBit valid (fromEnum t)]
      extra = popCount valid - length present
  counts <- mapM (\t -> (\c -> (t, fromIntegral c)) <$> u32) present
  skip (4 * extra)
  -- Bit 0x40 of HeapSizes marks an extra 4-byte field after the row counts.
  when (testBit heapSizes 6) (skip 4)
  start <- position
  pure (heapSizes, valid, sorted, counts, start)

buildLayouts :: Word8 -> (Table -> Int) -> Int -> Array Table Layout
buildLayouts heapSizes rows start =
  listArray (minBound, maxBound) (go start [minBound .. maxBound])
  where
    go _ [] = []
    go off (t : ts) =
      let widths = map (colWidth . snd) (schema t)
          offsets = scanl (+) 0 widths
          rowSize = sum widths
          n = length widths
          l =
            Layout
              { layoutRows = rows t
              , layoutOffset = off
              , layoutRowSize = rowSize
              , layoutColOffsets = U.listArray (0, n - 1) offsets
              , layoutColWidths = U.listArray (0, n - 1) widths
              }
       in l : go (off + rows t * rowSize) ts

    colWidth = \case
      U8 -> 1
      U16 -> 2
      U32 -> 4
      StrIdx -> heapWidth 0
      GuidIdx -> heapWidth 1
      BlobIdx -> heapWidth 2
      Idx t -> if rows t < 0x10000 then 2 else 4
      CodedIdx c ->
        let m = maximum (0 : [rows t | Just t <- codedTables c])
         in if m < (1 `shiftL` (16 - codedBits c)) then 2 else 4
    heapWidth bit = if testBit heapSizes bit then 4 else 2

-- | Check every column of every row against the heaps and tables it refers
-- to.
validate :: Database -> Either String ()
validate db =
  forM_ [minBound .. maxBound] $ \t ->
    forM_ (zip [0 ..] (schema t)) $ \(c, (colName, ty)) ->
      forM_ [0 .. rowCount db t - 1] $ \i -> do
        let v = rawColumn db t i c
            err msg = Left (tableName t ++ " row " ++ show (i + 1) ++ ", column " ++ colName ++ ": " ++ msg)
        case ty of
          StrIdx ->
            unless (fromIntegral v < max 1 (BS.length (dbStrings db))) $
              err ("string index " ++ show v ++ " is outside the #Strings heap")
          GuidIdx ->
            unless (fromIntegral v * 16 <= BS.length (dbGuids db)) $
              err ("GUID index " ++ show v ++ " is outside the #GUID heap")
          BlobIdx -> case blobBounds (dbBlobs db) (fromIntegral v) of
            Right _ -> pure ()
            Left msg -> err msg
          Idx target ->
            unless (fromIntegral v <= rowCount db target + 1) $
              err ("index " ++ show v ++ " is outside table " ++ tableName target)
          CodedIdx coded -> do
            let bits = codedBits coded
                tag = fromIntegral (v .&. ((1 `shiftL` bits) - 1))
                ix = fromIntegral (v `shiftR` bits)
            case drop tag (codedTables coded) of
              Just target : _ ->
                unless (ix <= rowCount db target) $
                  err ("index " ++ show ix ++ " is outside table " ++ tableName target)
              _ ->
                unless (ix == 0) $
                  err ("invalid " ++ show coded ++ " tag " ++ show tag)
          _ -> pure ()

-- | The start and length of the blob at an offset in the #Blob heap.
blobBounds :: ByteString -> Int -> Either String (Int, Int)
blobBounds heap off
  | off == 0 && BS.null heap = Right (0, 0)
  | off >= BS.length heap = Left ("blob index " ++ show off ++ " is outside the #Blob heap")
  | otherwise = do
      (len, start) <- runParserAt ((,) <$> compressedU32 <*> position) heap off
      let len' = fromIntegral len
      if start + len' <= BS.length heap
        then Right (start, len')
        else Left ("blob at " ++ show off ++ " extends past the end of the #Blob heap")

-- | Link metadata files that refer to each other's types, such as
-- @Windows.Win32.winmd@ and @Microsoft.Direct3D.D3D12.winmd@. A type
-- reference scoped to another assembly resolves to the definitions of that
-- name in any of the linked files, as windows-rs resolves names across all
-- the files it reads. The result has one database per module: a file given
-- twice (the same module GUID) appears once.
linkDatabases :: [Database] -> [Database]
linkDatabases dbs = linked
  where
    linked = [db {dbLinked = index} | db <- distinct Set.empty dbs]
    index = Map.unionsWith (++) [Map.map (map (Row db)) (dbTypeIndex db) | db <- linked]
    distinct _ [] = []
    distinct seen (db : rest)
      | dbMvid db `Set.member` seen = distinct seen rest
      | otherwise = db : distinct (Set.insert (dbMvid db) seen) rest

buildTypeIndex :: Database -> Map (Text, Text) [Int]
buildTypeIndex db =
  Map.fromListWith
    (flip (++))
    [ ((ns, heapString db (rawColumn db TTypeDef i 1)), [i])
    | i <- [0 .. rowCount db TTypeDef - 1]
    , let ns = heapString db (rawColumn db TTypeDef i 2)
    , not (T.null ns)
    ]

buildNestedIndex :: Database -> IntMap [Int]
buildNestedIndex db =
  IntMap.fromListWith
    (flip (++))
    [ (fromIntegral (rawColumn db TNestedClass i 1) - 1, [fromIntegral (rawColumn db TNestedClass i 0) - 1])
    | i <- [0 .. rowCount db TNestedClass - 1]
    ]

------------------------------------------------------------------------------
-- Rows

-- | A row of table @t@ in a database. Rows of different databases compare
-- by their module GUID first.
data Row (t :: Table) = Row
  { rowDatabase :: Database
  , rowIndex :: !Int
  -- ^ 0-based row index.
  }

instance Eq (Row t) where
  a == b = rowIndex a == rowIndex b && dbMvid (rowDatabase a) == dbMvid (rowDatabase b)

instance Ord (Row t) where
  compare a b = compare (dbMvid (rowDatabase a), rowIndex a) (dbMvid (rowDatabase b), rowIndex b)

instance (KnownTable t) => Show (Row t) where
  show r = tableName (tableOf @t) ++ "#" ++ show (rowIndex r + 1)

-- | The metadata token of a row (II.22).
rowToken :: forall t. (KnownTable t) => Row t -> Word32
rowToken r = (fromIntegral (fromEnum (tableOf @t)) `shiftL` 24) .|. fromIntegral (rowIndex r + 1)

-- | All rows of a table.
rowsOf :: forall t. (KnownTable t) => Database -> [Row t]
rowsOf db = map (Row db) [0 .. rowCount db (tableOf @t) - 1]

-- | Make a row without checking that its table matches @t@. The index must
-- have been validated.
unsafeRow :: Database -> Int -> Row t
unsafeRow = Row

------------------------------------------------------------------------------
-- Raw access

-- | Read a column of a row of a table as an unsigned integer.
rawColumn :: Database -> Table -> Int -> Int -> Word32
rawColumn db t i c =
  let l = dbLayouts db ! t
      off = layoutOffset l + i * layoutRowSize l + layoutColOffsets l U.! c
   in case layoutColWidths l U.! c of
        1 -> fromIntegral (BU.unsafeIndex (dbTables db) off)
        2 -> fromIntegral (le16 (dbTables db) off)
        _ -> le32 (dbTables db) off

column :: forall t. (KnownTable t) => Row t -> Int -> Word32
column (Row db i) = rawColumn db (tableOf @t) i

-- | The string at an offset in the #Strings heap.
heapString :: Database -> Word32 -> Text
heapString db off =
  let rest = BU.unsafeDrop (fromIntegral off) (dbStrings db)
   in decodeUtf8Lenient (BS.takeWhile (/= 0) rest)

-- | The blob at an offset in the #Blob heap.
heapBlob :: Database -> Word32 -> ByteString
heapBlob db off = case blobBounds (dbBlobs db) (fromIntegral off) of
  Right (start, len) -> BU.unsafeTake len (BU.unsafeDrop start (dbBlobs db))
  Left msg -> throwWinMD msg

-- | The GUID at a 1-based index in the #GUID heap, as 16 raw bytes. Index 0
-- is the null GUID and gives an empty string.
heapGuid :: Database -> Word32 -> ByteString
heapGuid _ 0 = BS.empty
heapGuid db i = BS.take 16 (BS.drop ((fromIntegral i - 1) * 16) (dbGuids db))

-- | The 0-based rows of a table whose column equals a value. Uses binary
-- search when the table is marked sorted.
equalRange :: Database -> Table -> Int -> Word32 -> [Int]
equalRange db t c v
  | testBit (dbSorted db) (fromEnum t) = [lowerBound (>= v) .. lowerBound (> v) - 1]
  | otherwise = filter (\i -> rawColumn db t i c == v) [0 .. n - 1]
  where
    n = rowCount db t
    lowerBound p = go 0 n
      where
        go lo hi
          | lo >= hi = lo
          | p (rawColumn db t mid c) = go lo mid
          | otherwise = go (mid + 1) hi
          where
            mid = (lo + hi) `div` 2

-- | The 0-based target rows of a list column (such as TypeDef.FieldList):
-- from this row's value up to the next row's value.
listRange :: Database -> Table -> Int -> Table -> Int -> [Int]
listRange db owner c target i =
  let start = fromIntegral (rawColumn db owner i c)
      end
        | i + 1 < rowCount db owner = fromIntegral (rawColumn db owner (i + 1) c)
        | otherwise = rowCount db target + 1
   in [max 1 start - 1 .. min (rowCount db target + 1) end - 2]

-- | The last 0-based row whose column is at most a value, in a table whose
-- column is non-decreasing (such as TypeDef.MethodList).
lastRowWhere :: Database -> Table -> Int -> Word32 -> Maybe Int
lastRowWhere db t c v =
  let n = rowCount db t
      go lo hi
        | lo >= hi = lo
        | rawColumn db t mid c > v = go lo mid
        | otherwise = go (mid + 1) hi
        where
          mid = (lo + hi) `div` 2
      k = go 0 n
   in if k == 0 then Nothing else Just (k - 1)
