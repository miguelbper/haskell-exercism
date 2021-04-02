{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_binary_search (
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
version = Version [1,3,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\bin"
libdir     = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\lib\\x86_64-windows-ghc-8.8.4\\binary-search-1.3.0.0-HXTGSf7fLQIBmHNKtM9L6j-test"
dynlibdir  = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\share\\x86_64-windows-ghc-8.8.4\\binary-search-1.3.0.0"
libexecdir = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\libexec\\x86_64-windows-ghc-8.8.4\\binary-search-1.3.0.0"
sysconfdir = "C:\\Users\\Miguel\\Exercism\\haskell\\binary-search\\.stack-work\\install\\72dbaa13\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "binary_search_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "binary_search_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "binary_search_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "binary_search_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "binary_search_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "binary_search_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
