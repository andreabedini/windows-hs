{-# LANGUAGE AllowAmbiguousTypes #-}

-- | COM interfaces.
--
-- A COM object is reached through interface pointers. Each points to a
-- pointer to the interface's vtable, the table of its methods: those of
-- the interfaces it derives from first, then its own. Generated bindings
-- make each interface a newtype over its pointer, with an 'Interface'
-- instance and an 'Inherits' instance for every interface it derives from,
-- and call methods through the vtable ('vtableEntry').
--
-- Almost every interface derives from 'IUnknown', which counts references
-- to the object. An interface returned by Windows holds one reference,
-- which the caller must give back with 'release' when done with it, as
-- windows-rs does when the value is dropped. Release it on a thread of the
-- object's apartment: the one that created it, for an object of a
-- single-threaded apartment such as a dialog. 'withCom' releases it when
-- an action returns or throws.
module Windows.Core.Com
  ( -- * Interfaces
    Interface (..)
  , Inherits
  , upcast
  , orNull
  , isNull
  , vtableEntry

    -- * IUnknown
  , IUnknown (..)
  , cast
  , addRef
  , release
  , withCom
  , query
  ) where

import Control.Exception (bracket)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Windows.Core.Error (check)
import Windows.Core.FixedArray (FixedArray (..))
import Windows.Core.GUID (GUID (..))
import Windows.Core.HRESULT (HRESULT (..))

-- | A COM interface: a pointer to an object whose first field points to
-- the interface's vtable.
class Interface i where
  -- | The interface identifier (IID), used by 'cast' and 'query'. Zero for
  -- the few interfaces the metadata gives none, as in windows-rs.
  interfaceId :: GUID

  toRaw :: i -> Ptr ()
  fromRaw :: Ptr () -> i

-- | @Inherits base i@: @i@ derives from @base@, or is @base@, so its vtable
-- starts with that of @base@, and a pointer to @i@ can be used as a pointer
-- to @base@. Generated methods of @base@ take any such @i@.
class (Interface base, Interface i) => Inherits base i

-- | Use an interface as one it derives from. The pointer is the same, and so
-- is the reference: release one or the other, not both.
upcast :: forall base i. (Inherits base i) => i -> base
upcast = fromRaw . toRaw

-- | The interface, or NULL, for parameters that are optional.
orNull :: (Interface i) => Maybe i -> i
orNull = maybe (fromRaw nullPtr) id

-- | Whether this is a NULL pointer. Some methods succeed without returning
-- an interface.
isNull :: (Interface i) => i -> Bool
isNull i = toRaw i == nullPtr

-- | The method in slot @n@ of the interface's vtable, counting from 0 with
-- the methods of the interfaces it derives from.
vtableEntry :: forall f i. (Interface i) => i -> Int -> IO (FunPtr f)
vtableEntry i n = do
  vtable <- peek (castPtr (toRaw i)) :: IO (Ptr (FunPtr f))
  peekElemOff vtable n

------------------------------------------------------------------------------
-- IUnknown

-- | @IUnknown@, which every COM interface derives from: reference counting
-- ('addRef', 'release') and asking an object for another of its interfaces
-- ('cast').
newtype IUnknown = IUnknown (Ptr ())
  deriving stock (Show)
  deriving newtype (Eq, Ord, Storable)

instance Interface IUnknown where
  interfaceId = GUID 0 0 0 (FixedArray [0xC0, 0, 0, 0, 0, 0, 0, 0x46 :: Word8])
  toRaw (IUnknown p) = p
  fromRaw = IUnknown

instance Inherits IUnknown IUnknown

type QueryInterface = Ptr () -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

type AddRefOrRelease = Ptr () -> IO Word32

foreign import ccall safe "dynamic"
  callQueryInterface :: FunPtr QueryInterface -> QueryInterface

foreign import ccall safe "dynamic"
  callAddRefOrRelease :: FunPtr AddRefOrRelease -> AddRefOrRelease

-- | Ask the object for another of its interfaces with @QueryInterface@.
-- Throws 'Windows.Core.WindowsError' (@E_NOINTERFACE@) if it does not
-- have it. The result holds a new reference: release it as well as @i@.
cast :: forall t i. (Interface t, Inherits IUnknown i) => i -> IO t
cast i = do
  f <- vtableEntry i 0
  query (callQueryInterface f (toRaw i))

-- | Call a function that writes an interface for an IID to a pointer
-- (@riid@ and @[ComOutPtr] void**@, as @QueryInterface@ and
-- @CoCreateInstance@ do), passing the IID of @t@. Throws
-- 'Windows.Core.WindowsError' if the result is a failure.
query :: forall t. (Interface t) => (Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT) -> IO t
query f = with (interfaceId @t) $ \riid -> alloca $ \out -> do
  poke out nullPtr
  f riid out >>= check
  fromRaw <$> peek out

-- | Add a reference to the object, for a copy of the interface that is
-- released separately.
addRef :: (Inherits IUnknown i) => i -> IO i
addRef i = do
  f <- vtableEntry i 1
  _ <- callAddRefOrRelease f (toRaw i)
  pure i

-- | Give back a reference. Do not use the interface afterwards. Does nothing
-- for a NULL pointer.
release :: (Inherits IUnknown i) => i -> IO ()
release i
  | isNull i = pure ()
  | otherwise = do
      f <- vtableEntry i 2
      _ <- callAddRefOrRelease f (toRaw i)
      pure ()

-- | Run an action with an interface, and release it when the action returns
-- or throws.
withCom :: (Inherits IUnknown i) => IO i -> (i -> IO r) -> IO r
withCom acquire = bracket acquire release
