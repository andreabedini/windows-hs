{-# LANGUAGE DataKinds #-}

-- | Tests for the parts of the runtime that do not call Windows.
module Main (main) where

import Control.Monad (unless)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable (..))
import System.Exit (exitFailure)
import Windows.Core.FixedArray
import Windows.Core.HRESULT
import Windows.Core.Member

-- | A union of a 64-bit integer with two 32-bit halves, as generated
-- bindings declare one.
newtype U = U (FixedArray 8 Word8)
  deriving stock (Eq, Show)

instance Storable U where
  sizeOf _ = 8
  alignment _ = 8
  peek p = U <$> peekByteOff p 0
  poke p (U b) = pokeByteOff p 0 b

instance Member "whole" U Word64 where memberOffset = 0
instance Member "low" U Word32 where memberOffset = 0
instance Member "high" U Word32 where memberOffset = 4
instance Member "third" U Word16 where memberOffset = 4

main :: IO ()
main = do
  let failures =
        concat
          [ -- Values from winerror.h: E_ACCESSDENIED is
            -- HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED).
            hresultFromWin32 5 `shouldBe` HRESULT (-2147024891)
          , hresultFromWin32 2 `shouldBe` HRESULT (-2147024894)
          , hresultFromWin32 0 `shouldBe` HRESULT 0
          , -- A value that is already an HRESULT (E_POINTER) is unchanged.
            hresultFromWin32 0x80004003 `shouldBe` HRESULT (-2147467261)
          , map succeeded [HRESULT 0, HRESULT 1, HRESULT (-2147467261)] `shouldBe` [True, True, False]
          , show (WindowsError (HRESULT (-2147024894)) "The system cannot find the file specified.")
              `shouldBe` "WindowsError 0x80070002: The system cannot find the file specified."
          , show (WindowsError (HRESULT (-2147467261)) "") `shouldBe` "WindowsError 0x80004003"
          , zeroed `shouldBe` U (FixedArray (replicate 8 0))
          , getMember @"whole" (setMember @"high" 1 (setMember @"low" 2 u0)) `shouldBe` 0x100000002
          , getMember @"third" (setMember @"whole" 0x0001000200030004 u0) `shouldBe` 2
          , -- Setting a member leaves the other bytes alone.
            getMember @"low" (setMember @"third" 0xffff (setMember @"whole" 0x0001000200030004 u0)) `shouldBe` 0x00030004
          ]
  mapM_ putStrLn failures
  unless (null failures) exitFailure
  putStrLn "ok"

u0 :: U
u0 = zeroed

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]
