{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_zlib_bindings (
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
version = Version [0,1,1,5] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/bin"
libdir     = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/lib"
dynlibdir  = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/lib"
datadir    = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/share"
libexecdir = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/libexec"
sysconfdir = "/home/sheep/.cabal/store/ghc-8.6.5/zlib-bindings-0.1.1.5-62e2822ad9800067137b3aae03a05d9877608bc3199551c26d683f081cd488e3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "zlib_bindings_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "zlib_bindings_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "zlib_bindings_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "zlib_bindings_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "zlib_bindings_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "zlib_bindings_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
