-- | Flag values for metadata table columns (ECMA-335 II.23.1).
module WinMD.Tables.Flags
  ( hasFlag

    -- * TypeAttributes (II.23.1.15)
  , typeVisibilityMask
  , typeLayoutMask
  , typeAutoLayout
  , typeSequentialLayout
  , typeExplicitLayout
  , typeInterface
  , typeAbstract
  , typeSealed
  , typeWindowsRuntime

    -- * FieldAttributes (II.23.1.5)
  , fieldStatic
  , fieldInitOnly
  , fieldLiteral
  , fieldHasDefault

    -- * MethodAttributes (II.23.1.10)
  , methodStatic
  , methodVirtual
  , methodAbstract
  , methodSpecialName
  , methodPInvokeImpl

    -- * ParamAttributes (II.23.1.13)
  , paramIn
  , paramOut
  , paramOptional
  , paramHasDefault

    -- * PInvokeAttributes (II.23.1.8)
  , pinvokeNoMangle
  , pinvokeCharSetMask
  , pinvokeCharSetAnsi
  , pinvokeCharSetUnicode
  , pinvokeSupportsLastError
  , pinvokeCallConvMask
  , pinvokeCallConvWinapi
  , pinvokeCallConvCdecl
  , pinvokeCallConvStdcall
  ) where

import Data.Bits (Bits, (.&.))
import Data.Word (Word16, Word32)

-- | @hasFlag flag flags@ is true when every bit of @flag@ is set in @flags@.
hasFlag :: (Bits a) => a -> a -> Bool
hasFlag flag flags = flags .&. flag == flag

typeVisibilityMask, typeLayoutMask, typeAutoLayout, typeSequentialLayout, typeExplicitLayout :: Word32
typeVisibilityMask = 0x7
typeLayoutMask = 0x18
typeAutoLayout = 0x0
typeSequentialLayout = 0x8
typeExplicitLayout = 0x10

typeInterface, typeAbstract, typeSealed, typeWindowsRuntime :: Word32
typeInterface = 0x20
typeAbstract = 0x80
typeSealed = 0x100
typeWindowsRuntime = 0x4000

fieldStatic, fieldInitOnly, fieldLiteral, fieldHasDefault :: Word16
fieldStatic = 0x10
fieldInitOnly = 0x20
fieldLiteral = 0x40
fieldHasDefault = 0x8000

methodStatic, methodVirtual, methodAbstract, methodSpecialName, methodPInvokeImpl :: Word16
methodStatic = 0x10
methodVirtual = 0x40
methodAbstract = 0x400
methodSpecialName = 0x800
methodPInvokeImpl = 0x2000

paramIn, paramOut, paramOptional, paramHasDefault :: Word16
paramIn = 0x1
paramOut = 0x2
paramOptional = 0x10
paramHasDefault = 0x1000

pinvokeNoMangle, pinvokeCharSetMask, pinvokeCharSetAnsi, pinvokeCharSetUnicode :: Word16
pinvokeNoMangle = 0x1
pinvokeCharSetMask = 0x6
pinvokeCharSetAnsi = 0x2
pinvokeCharSetUnicode = 0x4

pinvokeSupportsLastError :: Word16
pinvokeSupportsLastError = 0x40

pinvokeCallConvMask, pinvokeCallConvWinapi, pinvokeCallConvCdecl, pinvokeCallConvStdcall :: Word16
pinvokeCallConvMask = 0x700
pinvokeCallConvWinapi = 0x100
pinvokeCallConvCdecl = 0x200
pinvokeCallConvStdcall = 0x300
