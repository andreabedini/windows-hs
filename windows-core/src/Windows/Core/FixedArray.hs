-- | Fixed-size arrays embedded in structs, such as @WCHAR szCSDVersion[128]@.
module Windows.Core.FixedArray
  ( FixedArray (..)
  , fixedArrayLength
  ) where

import Data.Proxy (Proxy (..))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat, Nat, natVal)

-- | An array of exactly @n@ elements stored inline.
--
-- 'poke' writes at most @n@ elements. If the list is shorter, the remaining
-- elements in memory are left as they were.
newtype FixedArray (n :: Nat) a = FixedArray [a]
  deriving stock (Eq, Ord, Show)

fixedArrayLength :: forall n a. (KnownNat n) => FixedArray n a -> Int
fixedArrayLength _ = fromIntegral (natVal (Proxy :: Proxy n))

instance (KnownNat n, Storable a) => Storable (FixedArray n a) where
  sizeOf arr = fixedArrayLength arr * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek p = FixedArray <$> peekArray (fixedArrayLength (undefined :: FixedArray n a)) (castPtr p)
  poke p arr@(FixedArray xs) = pokeArray (castPtr p) (take (fixedArrayLength arr) xs)
