-- | Read ECMA-335 metadata files (@.winmd@), such as @Windows.Win32.winmd@
-- from Microsoft's win32metadata.
--
-- @
-- Right db <- 'readDatabase' "Windows.Win32.winmd"
-- let Just apis = 'findTypeDef' db "Windows.Win32.System.SystemInformation" "Apis"
-- [ m | m <- 'typeDefMethods' apis, 'methodName' m == "GetTickCount" ]
-- @
module WinMD
  ( -- * Loading
    Database
  , readDatabase
  , parseDatabase
  , WinMDError (..)

    -- * Tables
  , module WinMD.Tables

    -- * Signatures
  , module WinMD.Signature

    -- * Custom attributes
  , module WinMD.Attribute
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import WinMD.Attribute
import WinMD.Internal.Database (Database, WinMDError (..), loadDatabase)
import WinMD.Signature
import WinMD.Tables

-- | Read and validate a metadata file. I/O errors are thrown; a malformed
-- file gives 'Left'.
readDatabase :: FilePath -> IO (Either WinMDError Database)
readDatabase path = parseDatabase <$> BS.readFile path

-- | Parse and validate a metadata file held in memory.
parseDatabase :: ByteString -> Either WinMDError Database
parseDatabase = either (Left . WinMDError) Right . loadDatabase
