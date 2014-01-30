-- | Platform-specific dynamic linking support. Add new platforms to this file
-- through conditional compilation.
{-# LANGUAGE ExistentialQuantification, CPP #-}
module Util.DynamicLinker where

#ifdef IDRIS_FFI
import Foreign.LibFFI
import Foreign.Ptr (Ptr(), nullPtr, FunPtr, nullFunPtr, castPtrToFunPtr)
import System.Directory
#ifndef WINDOWS
import System.Posix.DynamicLinker
#else
import qualified Control.Exception as Exception (catch, IOException)
import System.Win32.DLL
import System.Win32.Types
type DL = HMODULE
#endif

hostDynamicLibExt :: String
#ifdef LINUX
hostDynamicLibExt = "so"
#elif MACOSX
hostDynamicLibExt = "dylib"
#elif WINDOWS
hostDynamicLibExt = "dll"
#elif FREEBSD
hostDynamicLibExt = "so"
#elif DRAGONFLY
hostDynamicLibExt = "so"
#else
hostDynamicLibExt = error $ unwords
  [ "Undefined file extension for dynamic libraries"
  , "in Idris' Util.DynamicLinker."
  ]
#endif

data ForeignFun = forall a. Fun { fun_name :: String
                                , fun_handle :: FunPtr a
                                }

data DynamicLib = Lib { lib_name :: String
                      , lib_handle :: DL
                      }

instance Eq DynamicLib where
    (Lib a _) == (Lib b _) = a == b

#ifndef WINDOWS
tryLoadLib :: String -> IO (Maybe DynamicLib)
tryLoadLib lib = do exactName <- doesFileExist lib
                    let filename = if exactName then lib else lib ++ "." ++ hostDynamicLibExt
                    handle <- dlopen filename [RTLD_NOW, RTLD_GLOBAL]
                    if undl handle == nullPtr
                      then return Nothing
                      else return . Just $ Lib lib handle


tryLoadFn :: String -> DynamicLib -> IO (Maybe ForeignFun)
tryLoadFn fn (Lib _ h) = do cFn <- dlsym h fn
                            if cFn == nullFunPtr
                               then return Nothing
                               else return . Just $ Fun fn cFn
#else
tryLoadLib :: String -> IO (Maybe DynamicLib)
tryLoadLib lib = do exactName <- doesFileExist lib
                    let filename = if exactName then lib else lib ++ "." ++ hostDynamicLibExt
                    handle <- Exception.catch (loadLibrary filename) nullPtrOnException
                    if handle == nullPtr
                        then return Nothing
                        else return . Just $ Lib lib handle
  where nullPtrOnException :: Exception.IOException -> IO DL
        nullPtrOnException e = return nullPtr
        -- `show e` will however give broken error message

tryLoadFn :: String -> DynamicLib -> IO (Maybe ForeignFun)
tryLoadFn fn (Lib _ h) = do cFn <- getProcAddress h fn
                            if cFn == nullPtr
                                then return Nothing
                                else return . Just $ Fun fn (castPtrToFunPtr cFn)
#endif
#else
-- no libffi, just add stubbs.

data DynamicLib = Lib { lib_name :: String
                      , lib_handle :: ()
                      }
    deriving Eq

tryLoadLib :: String -> IO (Maybe DynamicLib)
tryLoadLib lib = do putStrLn $ "WARNING: Cannot load '" ++ lib ++ "' at compile time because Idris was compiled without libffi support."
                    return Nothing


#endif


