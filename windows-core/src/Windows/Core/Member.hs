{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Members of unions and explicit-layout structs.
--
-- Generated bindings keep these types as raw bytes, because their members
-- overlap. Each member gets a 'Member' instance giving its offset, and a
-- @HasField@ instance, so that it is read like a struct field:
--
-- > input.anonymous.ki :: KEYBDINPUT
--
-- A value is built by setting members, starting from 'zeroed', as windows-rs
-- builds one with @INPUT_0 { ki: ... }@:
--
-- > setMember @"ki" keyboardInput zeroed :: INPUT_0
module Windows.Core.Member
  ( Member (..)
  , getMember
  , setMember
  , zeroed
  ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Storable (Storable (..))
import GHC.TypeLits (Symbol)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The member @name@ of @u@ has type @a@ and starts @memberOffset@ bytes
-- into it.
class (Storable u, Storable a) => Member (name :: Symbol) u a | name u -> a where
  memberOffset :: Int

-- | Read a member: the bytes at its offset, taken as its type.
getMember :: forall name u a. (Member name u a) => u -> a
getMember u = unsafeDupablePerformIO $ with u $ \p -> peekByteOff p (memberOffset @name @u @a)

-- | Overwrite a member, leaving the bytes outside it as they were.
setMember :: forall name u a. (Member name u a) => a -> u -> u
setMember a u = unsafeDupablePerformIO $ with u $ \p -> do
  pokeByteOff p (memberOffset @name @u @a) a
  peek p

-- | The value whose bytes are all zero, like @Default::default()@ for a
-- windows-rs struct or @= {0}@ in C.
zeroed :: forall a. (Storable a) => a
zeroed = unsafeDupablePerformIO $ allocaBytesAligned size (alignment (undefined :: a)) $ \p -> do
  fillBytes p 0 size
  peek p
  where
    size = sizeOf (undefined :: a)
