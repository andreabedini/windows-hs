-- | Locate the CLI metadata inside a PE/COFF image (ECMA-335 II.25).
module WinMD.Internal.PE
  ( metadataBlob
  ) where

import Control.Monad (replicateM, unless, when)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Word (Word32)
import WinMD.Internal.Parser

data Section = Section
  { secVirtualAddress :: !Word32
  , secVirtualSize :: !Word32
  , secRawSize :: !Word32
  , secRawPointer :: !Word32
  }

-- | Return the metadata blob (starting at the metadata root, II.24.2.1)
-- of a PE image.
metadataBlob :: ByteString -> Either String ByteString
metadataBlob = runParser $ do
  mz <- u16
  unless (mz == 0x5A4D) $ parseError "not a PE image: missing MZ signature"
  seek 0x3C
  peOffset <- u32
  seek (fromIntegral peOffset)
  sig <- u32
  unless (sig == 0x00004550) $ parseError "not a PE image: missing PE signature"
  -- COFF file header (II.25.2.2)
  _machine <- u16
  sectionCount <- u16
  skip 12
  optionalSize <- u16
  _characteristics <- u16
  optionalStart <- position
  -- Optional header (II.25.2.3). The data directories start at a different
  -- offset in PE32 and PE32+ images.
  magic <- u16
  directories <- case magic of
    0x10B -> pure 96
    0x20B -> pure 112
    _ -> parseError ("unknown optional header magic " ++ show magic)
  seek (optionalStart + directories - 4)
  directoryCount <- u32
  when (directoryCount < 15) $ parseError "PE image has no CLI header directory"
  -- Directory 14 is the CLI header.
  seek (optionalStart + directories + 14 * 8)
  cliRva <- u32
  _cliSize <- u32
  seek (optionalStart + fromIntegral optionalSize)
  sections <- replicateM (fromIntegral sectionCount) section
  cliOffset <- rvaToOffset sections cliRva
  -- CLI header (II.25.3.3): the metadata directory is at offset 8.
  seek (cliOffset + 8)
  metadataRva <- u32
  metadataSize <- u32
  metadataOffset <- rvaToOffset sections metadataRva
  seek metadataOffset
  bytes (fromIntegral metadataSize)

section :: Parser Section
section = do
  skip 8 -- name
  virtualSize <- u32
  virtualAddress <- u32
  rawSize <- u32
  rawPointer <- u32
  skip 16
  pure (Section virtualAddress virtualSize rawSize rawPointer)

rvaToOffset :: [Section] -> Word32 -> Parser Int
rvaToOffset sections rva =
  case find contains sections of
    Nothing -> parseError ("RVA " ++ show rva ++ " is not in any section")
    Just s -> pure (fromIntegral (rva - secVirtualAddress s + secRawPointer s))
  where
    contains s =
      secVirtualAddress s <= rva
        && rva < secVirtualAddress s + max (secVirtualSize s) (secRawSize s)
