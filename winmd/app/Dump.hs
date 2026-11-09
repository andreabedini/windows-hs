-- | Print the contents of a .winmd file.
--
-- > winmd-dump FILE                 list namespaces and type counts
-- > winmd-dump FILE NAMESPACE       list the types in a namespace
-- > winmd-dump FILE NAMESPACE NAME  show one type in detail
module Main (main) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import WinMD

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : rest -> do
      db <- readDatabase path >>= either (\e -> die' (show e)) pure
      case rest of
        [] -> namespaces db
        [ns] -> types db (T.pack ns)
        [ns, n] -> case findTypeDef db (T.pack ns) (T.pack n) of
          Just t -> showType t
          Nothing -> die' ("no type " ++ ns ++ "." ++ n)
        _ -> usage
    _ -> usage
  where
    usage = die' "usage: winmd-dump FILE [NAMESPACE [NAME]]"
    die' msg = hPutStrLn stderr msg >> exitFailure

namespaces :: Database -> IO ()
namespaces db = do
  let counts = Map.fromListWith (+) [(typeDefNamespace t, 1 :: Int) | t <- typeDefs db]
  mapM_ (\(ns, n) -> T.putStrLn (ns <> " " <> tshow n)) (Map.toList counts)

types :: Database -> Text -> IO ()
types db ns =
  mapM_ (T.putStrLn . line) (sortOn typeDefName [t | t <- typeDefs db, typeDefNamespace t == ns])
  where
    line t = kind t <> " " <> typeDefName t

kind :: TypeDef -> Text
kind t
  | hasFlag typeInterface (typeDefFlags t) = "interface"
  | otherwise = case typeDefExtends t >>= typeDefOrRefName of
      Just ("System", "Enum") -> "enum"
      Just ("System", "ValueType") -> "struct"
      Just ("System", "MulticastDelegate") -> "delegate"
      _ -> "class"

showType :: TypeDef -> IO ()
showType t = do
  mapM_ (T.putStrLn . ("[" <>) . (<> "]") . renderAttribute) (attributes t)
  T.putStrLn (kind t <> " " <> typeDefFullName t <> extends)
  mapM_ (\(p, s) -> T.putStrLn ("  layout: pack " <> tshow p <> ", size " <> tshow s)) (typeDefClassLayout t)
  mapM_ (\i -> T.putStrLn ("  implements " <> maybe "?" dotted (typeDefOrRefName i))) (typeDefInterfaces t)
  mapM_ showField (typeDefFields t)
  mapM_ showMethod (typeDefMethods t)
  mapM_ (\n -> T.putStrLn ("  nested " <> typeDefName n)) (typeDefNestedTypes t)
  where
    extends = maybe "" (\e -> " : " <> maybe "?" dotted (typeDefOrRefName e)) (typeDefExtends t)

showField :: Field -> IO ()
showField f = do
  mapM_ (T.putStrLn . ("  [" <>) . (<> "]") . renderAttribute) (attributes f)
  T.putStrLn $
    "  "
      <> (if hasFlag fieldStatic (fieldFlags f) then "static " else "")
      <> renderType (fieldType f)
      <> " "
      <> fieldName f
      <> maybe "" (\c -> " = " <> renderValue (constantValue c)) (fieldConstant f)
      <> maybe "" (\o -> " @" <> tshow o) (fieldOffset f)

showMethod :: MethodDef -> IO ()
showMethod m = do
  let sig = methodSignature m
      params = [p | p <- methodParams m, paramSequence p > 0]
      names = map paramName params ++ repeat "_"
  mapM_ (T.putStrLn . ("  [" <>) . (<> "]") . renderAttribute) (attributes m)
  case methodImplMap m of
    Just im ->
      T.putStrLn
        ( "  [DllImport(\""
            <> moduleRefName (implMapScope im)
            <> "\", EntryPoint = \""
            <> implMapImportName im
            <> "\", flags = "
            <> tshow (implMapFlags im)
            <> ")]"
        )
    Nothing -> pure ()
  T.putStrLn $
    "  "
      <> renderType (sigReturnType sig)
      <> " "
      <> methodName m
      <> "("
      <> T.intercalate ", " (zipWith3 renderParam (map Just params ++ repeat Nothing) (sigParams sig) names)
      <> ")"
  where
    renderParam mp ty name =
      maybe "" (\p -> T.concat ["[" <> a <> "] " | a <- paramAttrs p]) mp
        <> renderType ty
        <> " "
        <> name
    paramAttrs p =
      [ x
      | (flag, x) <- [(paramIn, "In"), (paramOut, "Out"), (paramOptional, "Optional")]
      , hasFlag flag (paramFlags p)
      ]
        ++ map renderAttribute (attributes p)

renderAttribute :: Attribute -> Text
renderAttribute a =
  T.dropEnd (T.length "Attribute") (attrName a)
    <> if null args then "" else "(" <> T.intercalate ", " args <> ")"
  where
    args = map renderAttrValue (attrArgs a) ++ [n <> " = " <> renderAttrValue v | (n, v) <- attrNamedArgs a]

renderAttrValue :: AttributeValue -> Text
renderAttrValue = \case
  AttrValue v -> renderValue v
  AttrString s -> maybe "null" tshow s
  AttrType s -> "typeof(" <> maybe "null" id s <> ")"
  AttrEnum n v -> n <> "(" <> renderValue v <> ")"
  AttrArray Nothing -> "null"
  AttrArray (Just vs) -> "[" <> T.intercalate ", " (map renderAttrValue vs) <> "]"

renderValue :: Value -> Text
renderValue = \case
  VBool b -> tshow b
  VChar c -> tshow c
  VInt8 x -> tshow x
  VUInt8 x -> tshow x
  VInt16 x -> tshow x
  VUInt16 x -> tshow x
  VInt32 x -> tshow x
  VUInt32 x -> tshow x
  VInt64 x -> tshow x
  VUInt64 x -> tshow x
  VFloat32 x -> tshow x
  VFloat64 x -> tshow x
  VString s -> tshow s
  VNull -> "null"

dotted :: (Text, Text) -> Text
dotted (ns, n)
  | T.null ns = n
  | otherwise = ns <> "." <> n

tshow :: (Show a) => a -> Text
tshow = T.pack . show
