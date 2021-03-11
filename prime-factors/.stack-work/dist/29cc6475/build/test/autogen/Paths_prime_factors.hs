{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_prime_factors (
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

bindir     = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\bin"
libdir     = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4\\prime-factors-1.1.0.5-4dSLpi0mnalLXFiyMZm75q-test"
dynlibdir  = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\share\\x86_64-windows-ghc-8.8.4\\prime-factors-1.1.0.5"
libexecdir = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\libexec\\x86_64-windows-ghc-8.8.4\\prime-factors-1.1.0.5"
sysconfdir = "C:\\Users\\Miguel\\Exercism\\haskell\\prime-factors\\.stack-work\\install\\e0f1c74b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prime_factors_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prime_factors_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prime_factors_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prime_factors_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prime_factors_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prime_factors_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)