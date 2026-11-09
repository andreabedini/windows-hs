-- | The Windows @GUID@ structure, which win32metadata refers to as
-- @System.Guid@.
module Windows.Core.GUID
  ( GUID (..)
  ) where

import Data.Word (Word16, Word32, Word8)
import Foreign.Storable (Storable (..))
import Windows.Core.FixedArray (FixedArray)

data GUID = GUID
  { data1 :: !Word32
  , data2 :: !Word16
  , data3 :: !Word16
  , data4 :: !(FixedArray 8 Word8)
  }
  deriving stock (Eq, Ord, Show)

instance Storable GUID where
  sizeOf _ = 16
  alignment _ = 4
  peek p = GUID <$> peekByteOff p 0 <*> peekByteOff p 4 <*> peekByteOff p 6 <*> peekByteOff p 8
  poke p (GUID a b c d) = pokeByteOff p 0 a >> pokeByteOff p 4 b >> pokeByteOff p 6 c >> pokeByteOff p 8 d
