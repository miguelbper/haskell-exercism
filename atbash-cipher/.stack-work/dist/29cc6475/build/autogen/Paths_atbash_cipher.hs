{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_atbash_cipher (
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
version = Version [1,2,0,7] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\bin"
libdir     = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4\\atbash-cipher-1.2.0.7-Gm4Q55f8AoKYjmGmcEELk"
dynlibdir  = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\share\\x86_64-windows-ghc-8.8.4\\atbash-cipher-1.2.0.7"
libexecdir = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\libexec\\x86_64-windows-ghc-8.8.4\\atbash-cipher-1.2.0.7"
sysconfdir = "C:\\Users\\Miguel\\Exercism\\haskell\\atbash-cipher\\.stack-work\\install\\e0f1c74b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "atbash_cipher_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "atbash_cipher_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "atbash_cipher_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "atbash_cipher_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "atbash_cipher_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "atbash_cipher_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
