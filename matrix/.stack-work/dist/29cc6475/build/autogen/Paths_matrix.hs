{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_matrix (
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
version = Version [1,3,0,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\bin"
libdir     = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\lib\\x86_64-windows-ghc-8.8.4\\matrix-1.3.0.9-1ykszt8KJYTDDlOpMwaIVk"
dynlibdir  = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\share\\x86_64-windows-ghc-8.8.4\\matrix-1.3.0.9"
libexecdir = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\libexec\\x86_64-windows-ghc-8.8.4\\matrix-1.3.0.9"
sysconfdir = "C:\\Users\\Miguel\\Exercism\\haskell\\matrix\\.stack-work\\install\\761f8111\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "matrix_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "matrix_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "matrix_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "matrix_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "matrix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "matrix_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
