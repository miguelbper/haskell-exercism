{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_run_length_encoding (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,1,0,5] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\bin"
libdir     = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4\\run-length-encoding-1.1.0.5-GFc3gFE5tZE7yZRyRTLinx-test"
dynlibdir  = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\share\\x86_64-windows-ghc-8.8.4\\run-length-encoding-1.1.0.5"
libexecdir = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\libexec\\x86_64-windows-ghc-8.8.4\\run-length-encoding-1.1.0.5"
sysconfdir = "C:\\Users\\Miguel\\Exercism\\haskell\\run-length-encoding\\.stack-work\\install\\e0f1c74b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "run_length_encoding_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "run_length_encoding_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "run_length_encoding_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "run_length_encoding_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "run_length_encoding_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "run_length_encoding_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
