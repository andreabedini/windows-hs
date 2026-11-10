-- | Passing structs by value under the x64 Windows calling convention.
--
-- Haskell's FFI cannot pass or return C structs by value. On x64 Windows a
-- struct of 1, 2, 4 or 8 bytes is passed as an integer of the same size,
-- and a C function returns one that way too. Any other struct is passed as
-- a pointer to a copy made by the caller. Other struct results, and every
-- struct result of a C++ member function such as a COM method, are written
-- through a pointer the caller passes as a hidden argument: the first, or
-- the second after @this@. Generated bindings use these helpers to follow
-- those rules.
module Windows.Core.Marshal
  ( structToWord
  , wordToStruct
  , withStructCopy
  , returnStruct
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

-- | Reinterpret a struct as an integer of the same size.
structToWord :: (Storable s, Storable w) => s -> IO w
structToWord s = with s (peek . castPtr)

-- | Reinterpret an integer as a struct of the same size.
wordToStruct :: (Storable s, Storable w) => w -> IO s
wordToStruct w = with w (peek . castPtr)

-- | Pass a pointer to a temporary copy of a struct.
withStructCopy :: (Storable s) => s -> (Ptr s -> IO r) -> IO r
withStructCopy = with

-- | Call a function that returns a struct through a hidden pointer. The
-- function also returns that pointer, which is ignored.
returnStruct :: (Storable s) => (Ptr s -> IO (Ptr s)) -> IO s
returnStruct f = alloca $ \p -> f p >> peek p
