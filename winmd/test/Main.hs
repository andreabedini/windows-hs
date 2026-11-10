-- | Tests against Windows.Win32.winmd, and Microsoft.Direct3D.D3D12.winmd
-- in the same directory. The files come from scripts/fetch-metadata.sh;
-- WINMD_PATH overrides the location of Windows.Win32.winmd.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.IORef
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import WinMD

main :: IO ()
main = do
  path <- fromMaybe "../metadata/Windows.Win32.winmd" <$> lookupEnv "WINMD_PATH"
  let load p =
        readDatabase p >>= \case
          Right db -> pure db
          Left e -> do
            putStrLn ("cannot load " ++ p ++ ": " ++ show e)
            putStrLn "run scripts/fetch-metadata.sh or set WINMD_PATH"
            exitFailure
  db <- load path
  d3d12 <- load (takeDirectory path </> "Microsoft.Direct3D.D3D12.winmd")
  failures <- newIORef (0 :: Int)
  let test name body = do
        putStr (name ++ ": ")
        hFlush stdout
        r <- try (body >>= evaluate)
        case r of
          Right [] -> putStrLn "ok"
          Right errs -> do
            putStrLn "FAILED"
            mapM_ (putStrLn . ("  " ++)) errs
            modifyIORef' failures (+ 1)
          Left (e :: SomeException) -> do
            putStrLn ("FAILED with exception: " ++ show e)
            modifyIORef' failures (+ 1)

  test "module name" $
    pure (map moduleName (rowsOf db) `shouldBe` ["Windows.Win32.winmd"])

  test "GetTickCount import" $ do
    m <- method db "Windows.Win32.System.SystemInformation" "Apis" "GetTickCount"
    let im = methodImplMap m
        sig = methodSignature m
    pure $
      concat
        [ fmap (moduleRefName . implMapScope) im `shouldBe` Just "KERNEL32.dll"
        , fmap implMapImportName im `shouldBe` Just "GetTickCount"
        , fmap (\i -> hasFlag pinvokeSupportsLastError (implMapFlags i)) im `shouldBe` Just False
        , sigReturnType sig `shouldBe` TUInt32
        , sigParams sig `shouldBe` []
        ]

  test "MessageBoxW signature" $ do
    m <- method db "Windows.Win32.UI.WindowsAndMessaging" "Apis" "MessageBoxW"
    let sig = methodSignature m
        params = [p | p <- methodParams m, paramSequence p > 0]
    pure $
      concat
        [ fmap (moduleRefName . implMapScope) (methodImplMap m) `shouldBe` Just "USER32.dll"
        , fmap (hasFlag pinvokeSupportsLastError . implMapFlags) (methodImplMap m) `shouldBe` Just True
        , map paramName params `shouldBe` ["hWnd", "lpText", "lpCaption", "uType"]
        , map typeName (sigParams sig)
            `shouldBe` map
              Just
              [ "Windows.Win32.Foundation.HWND"
              , "Windows.Win32.Foundation.PWSTR"
              , "Windows.Win32.Foundation.PWSTR"
              , "Windows.Win32.UI.WindowsAndMessaging.MESSAGEBOX_STYLE"
              ]
        , typeName (sigReturnType sig) `shouldBe` Just "Windows.Win32.UI.WindowsAndMessaging.MESSAGEBOX_RESULT"
        , map (hasFlag paramOptional . paramFlags) params `shouldBe` [True, True, True, False]
        , map (hasAttribute "ConstAttribute") params `shouldBe` [False, True, True, False]
        ]

  test "RECT fields" $ do
    t <- typeDef db "Windows.Win32.Foundation" "RECT"
    pure $
      concat
        [ map fieldName (typeDefFields t) `shouldBe` ["left", "top", "right", "bottom"]
        , map fieldType (typeDefFields t) `shouldBe` replicate 4 TInt32
        , (typeDefFlags t `hasLayout` typeSequentialLayout) `shouldBe` True
        ]

  test "HWND is a native typedef of void*" $ do
    t <- typeDef db "Windows.Win32.Foundation" "HWND"
    pure $
      concat
        [ hasAttribute "NativeTypedefAttribute" t `shouldBe` True
        , fmap attrArgs (findAttribute "AlsoUsableForAttribute" t) `shouldBe` Just [AttrString (Just "HANDLE")]
        , map fieldType (typeDefFields t) `shouldBe` [TPtr TVoid]
        ]

  test "constants" $ do
    apis <- typeDef db "Windows.Win32.Foundation" "Apis"
    let value name = [constantValue c | f <- typeDefFields apis, fieldName f == name, Just c <- [fieldConstant f]]
        ty name = [typeName (fieldType f) | f <- typeDefFields apis, fieldName f == name]
    pure $
      concat
        [ value "MAX_PATH" `shouldBe` [VUInt32 260]
        , value "S_OK" `shouldBe` [VInt32 0]
        , value "E_POINTER" `shouldBe` [VInt32 (-2147467261)]
        , ty "E_POINTER" `shouldBe` [Just "Windows.Win32.Foundation.HRESULT"]
        ]

  test "IUnknown" $ do
    t <- typeDef db "Windows.Win32.System.Com" "IUnknown"
    pure $
      concat
        [ hasFlag typeInterface (typeDefFlags t) `shouldBe` True
        , map methodName (typeDefMethods t) `shouldBe` ["QueryInterface", "AddRef", "Release"]
        , fmap attrArgs (findAttribute "GuidAttribute" t)
            `shouldBe` Just
              ( [AttrValue (VUInt32 0), AttrValue (VUInt16 0), AttrValue (VUInt16 0)]
                  ++ map (AttrValue . VUInt8) [0xC0, 0, 0, 0, 0, 0, 0, 0x46]
              )
        , map (sigHasThis . methodSignature) (typeDefMethods t) `shouldBe` [True, True, True]
        ]

  test "WNDPROC delegate" $ do
    t <- typeDef db "Windows.Win32.UI.WindowsAndMessaging" "WNDPROC"
    let invoke = [m | m <- typeDefMethods t, methodName m == "Invoke"]
    pure $
      concat
        [ fmap attrArgs (findAttribute "UnmanagedFunctionPointerAttribute" t)
            `shouldBe` Just [AttrEnum "System.Runtime.InteropServices.CallingConvention" (VInt32 1)]
        , map (typeName . sigReturnType . methodSignature) invoke `shouldBe` [Just "Windows.Win32.Foundation.LRESULT"]
        , map (length . sigParams . methodSignature) invoke `shouldBe` [4]
        ]

  test "architecture-specific definitions" $ do
    let defs = findTypeDefs db "Windows.Win32.System.Diagnostics.Debug" "CONTEXT"
        arch t = case findAttribute "SupportedArchitectureAttribute" t of
          Just (Attribute {attrArgs = [AttrEnum _ (VInt32 a)]}) -> Right a
          other -> Left other
        archs = map arch defs
        -- Architecture is a flags enum (X86 = 1, X64 = 2, Arm64 = 4), so the
        -- definitions must cover disjoint sets of architectures.
        disjoint :: [Int32] -> Bool
        disjoint = \case
          [] -> True
          a : rest -> all (\b -> a .&. b == 0) rest && disjoint rest
    pure $
      concat
        [ check (length defs >= 2) ("expected several CONTEXT definitions, got " ++ show (length defs))
        , case sequence archs of
            Right as -> check (disjoint as) ("CONTEXT architectures overlap: " ++ show as)
            Left other -> ["CONTEXT without a SupportedArchitecture enum argument: " ++ show other]
        ]

  test "nested anonymous union" $ do
    t <- typeDef db "Windows.Win32.System.SystemInformation" "SYSTEM_INFO"
    let anon = [f | f <- typeDefFields t, fieldName f == "Anonymous"]
        resolved = case map fieldType anon of
          [TValueType (IsTypeRef r)] -> resolveTypeRef r
          _ -> []
    pure $
      concat
        [ map typeDefName (typeDefNestedTypes t) `shouldBe` ["_Anonymous_e__Union"]
        , map typeDefName resolved `shouldBe` ["_Anonymous_e__Union"]
        , map typeDefEnclosingType resolved `shouldBe` [Just t]
        , map (\u -> typeDefFlags u `hasLayout` typeExplicitLayout) resolved `shouldBe` [True]
        , map typeDefFullName resolved `shouldBe` ["Windows.Win32.System.SystemInformation.SYSTEM_INFO/_Anonymous_e__Union"]
        ]

  test "type references across linked files" $ do
    -- Windows.Win32.winmd uses Direct3D 12 types, and the Direct3D 12 file
    -- uses Win32 types, through TypeRefs scoped to the other assembly
    -- (II.22.38). They resolve only once the files are linked.
    let fieldRef d ns ty name = do
          t <- typeDef d ns ty
          case [fieldType f | f <- typeDefFields t, fieldName f == name] of
            [TClass (IsTypeRef r)] -> pure r
            [TValueType (IsTypeRef r)] -> pure r
            other -> fail ("unexpected type of " ++ T.unpack name ++ ": " ++ show other)
        toD3D12 d = fieldRef d "Windows.Win32.AI.MachineLearning.DirectML" "DML_BUFFER_BINDING" "Buffer"
        toWin32 d = fieldRef d "Windows.Win32.Graphics.Direct3D12" "D3D12_RESOURCE_DESC" "Format"
        resolved r = [(typeDefFullName t, map moduleName (rowsOf (rowDatabase t))) | t <- resolveTypeRef r]
    (w, d) <- case linkDatabases [db, d3d12, db] of
      [w, d] -> pure (w, d)
      ds -> fail ("expected two linked databases, got " ++ show (length ds))
    unlinked <- (,) <$> toD3D12 db <*> toWin32 d3d12
    linked <- (,) <$> toD3D12 w <*> toWin32 d
    pure $
      concat
        [ map resolved [fst unlinked, snd unlinked] `shouldBe` [[], []]
        , resolved (fst linked) `shouldBe` [("Windows.Win32.Graphics.Direct3D12.ID3D12Resource", ["Microsoft.Direct3D.D3D12.winmd"])]
        , resolved (snd linked) `shouldBe` [("Windows.Win32.Graphics.Dxgi.Common.DXGI_FORMAT", ["Windows.Win32.winmd"])]
        ]

  test "method and field owners" $
    pure $
      take 5 $
        [ "method " ++ show m ++ " of " ++ show t ++ " has parent " ++ show (methodParent m)
        | t <- typeDefs db
        , m <- typeDefMethods t
        , methodParent m /= Just t
        ]
          ++ [ "field " ++ show f ++ " of " ++ show t ++ " has parent " ++ show (fieldParent f)
             | t <- typeDefs db
             , f <- typeDefFields t
             , fieldParent f /= Just t
             ]

  test "every signature, constant and attribute in both files decodes" $ do
    counts <- newIORef (0 :: Int)
    let force :: (Show a) => a -> IO ()
        force x = length (show x) `seq` modifyIORef' counts (+ 1)
        attrs :: (HasAttributes t) => Row t -> IO ()
        attrs r = mapM_ (\a -> either (fail . ((show a ++ ": ") ++)) force (decodeAttributeEither a)) (customAttributes r)
    mapM_
      ( \t -> do
          attrs t
          mapM_ (\f -> force (fieldType f) >> mapM_ (force . constantValue) (fieldConstant f) >> attrs f) (typeDefFields t)
          mapM_ (\m -> force (methodSignature m) >> attrs m >> mapM_ (\p -> attrs p >> mapM_ (force . constantValue) (paramConstant p)) (methodParams m)) (typeDefMethods t)
      )
      (concatMap typeDefs [db, d3d12])
    mapM_ (force . typeSpecType) (concatMap rowsOf [db, d3d12] :: [TypeSpec])
    n <- readIORef counts
    putStr ("(" ++ show n ++ " items) ")
    pure []

  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " test(s) failed")
    exitFailure

method :: Database -> Text -> Text -> Text -> IO MethodDef
method db ns ty name = do
  t <- typeDef db ns ty
  case [m | m <- typeDefMethods t, methodName m == name] of
    [m] -> pure m
    ms -> fail ("expected one method " ++ T.unpack name ++ ", found " ++ show (length ms))

typeDef :: Database -> Text -> Text -> IO TypeDef
typeDef db ns name = maybe (fail ("no type " ++ T.unpack ns ++ "." ++ T.unpack name)) pure (findTypeDef db ns name)

typeName :: Type -> Maybe Text
typeName = \case
  TValueType t -> dotted <$> typeDefOrRefName t
  TClass t -> dotted <$> typeDefOrRefName t
  _ -> Nothing
  where
    dotted (ns, n) = ns <> "." <> n

hasLayout :: Word32 -> Word32 -> Bool
hasLayout flags layout = flags .&. typeLayoutMask == layout

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]

check :: Bool -> String -> [String]
check ok msg = if ok then [] else [msg]
