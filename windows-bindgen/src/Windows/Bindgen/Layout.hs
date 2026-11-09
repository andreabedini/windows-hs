-- | Sizes, alignments and field offsets on x64, computed from metadata the
-- way MSVC lays out C structs.
module Windows.Bindgen.Layout
  ( Layout (..)
  , typeLayout
  , defLayout
  , structFields
  ) where

import Control.Monad (foldM)
import Data.Text qualified as T
import WinMD
import Windows.Bindgen.Model

data Layout = Layout
  { layoutSize :: !Int
  , layoutAlign :: !Int
  }
  deriving stock (Eq, Show)

pointer :: Layout
pointer = Layout 8 8

-- | The layout of a value of a type.
typeLayout :: Type -> Either String Layout
typeLayout = \case
  TBool -> Right (Layout 1 1)
  TInt8 -> Right (Layout 1 1)
  TUInt8 -> Right (Layout 1 1)
  TChar -> Right (Layout 2 2)
  TInt16 -> Right (Layout 2 2)
  TUInt16 -> Right (Layout 2 2)
  TInt32 -> Right (Layout 4 4)
  TUInt32 -> Right (Layout 4 4)
  TFloat32 -> Right (Layout 4 4)
  TInt64 -> Right (Layout 8 8)
  TUInt64 -> Right (Layout 8 8)
  TFloat64 -> Right (Layout 8 8)
  TIntPtr -> Right pointer
  TUIntPtr -> Right pointer
  TPtr _ -> Right pointer
  TFnPtr _ -> Right pointer
  -- Interfaces are referenced through pointers, and delegates are function
  -- pointers.
  TClass _ -> Right pointer
  TModified _ _ t -> typeLayout t
  TArray t shape -> do
    Layout size align <- typeLayout t
    let n = product (arraySizes shape)
    Right (Layout (size * n) align)
  TValueType r ->
    resolveTarget r >>= \case
      TargetGuid -> Right (Layout 16 4)
      TargetDef d -> defLayout d
  t -> Left ("no layout for " ++ T.unpack (renderType t))

-- | The layout of a value type definition.
defLayout :: TypeDef -> Either String Layout
defLayout d = case classify d of
  KEnum -> single
  KNativeTypedef -> single
  KStruct -> fst <$> aggregate d
  KUnion -> fst <$> aggregate d
  k -> Left ("no layout for " ++ show k ++ " " ++ T.unpack (typeDefFullName d))
  where
    single = case instanceFields d of
      [f] -> typeLayout (fieldType f)
      fs -> Left (T.unpack (typeDefFullName d) ++ " has " ++ show (length fs) ++ " instance fields, expected 1")

-- | The fields of a struct or union with their offsets and layouts.
structFields :: TypeDef -> Either String [(Field, Int, Layout)]
structFields d = snd <$> aggregate d

aggregate :: TypeDef -> Either String (Layout, [(Field, Int, Layout)])
aggregate d = do
  fields <- mapM (\f -> (,) f <$> typeLayout (fieldType f)) (instanceFields d)
  let explicit = classify d == KUnion
      (packing, classSize) = case typeDefClassLayout d of
        Just (p, s) -> (fromIntegral p, fromIntegral s)
        Nothing -> (0, 0)
      -- A packing of 0 means the default, which never lowers an alignment.
      cap a = if packing == 0 then a else min a packing
  placed <-
    if explicit
      then pure [(f, maybe 0 fromIntegral (fieldOffset f), l) | (f, l) <- fields]
      else reverse . snd <$> foldM place (0, []) fields
  let align = maximum (1 : [cap (layoutAlign l) | (_, _, l) <- placed])
      end = maximum (0 : [off + layoutSize l | (_, off, l) <- placed])
      size = max classSize (roundUp align (max end (if null placed then 1 else 0)))
  pure (Layout size align, placed)
  where
    place (off, acc) (f, l) =
      let a = capFor l
          start = roundUp a off
       in Right (start + layoutSize l, (f, start, l) : acc)
    capFor l = case typeDefClassLayout d of
      Just (p, _) | p /= 0 -> min (layoutAlign l) (fromIntegral p)
      _ -> layoutAlign l

roundUp :: Int -> Int -> Int
roundUp a n = ((n + a - 1) `div` a) * a
