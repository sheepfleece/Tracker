{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_basement (
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
version = Version [0,0,11] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/bin"
libdir     = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/lib"
dynlibdir  = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/lib"
datadir    = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/share"
libexecdir = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/libexec"
sysconfdir = "/home/sheep/.cabal/store/ghc-8.6.5/basement-0.0.11-5a63091ebecfdad766ec9df60794ce6f2f6fef8aece88eeb4a4b567cbc7c4db5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "basement_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "basement_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "basement_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "basement_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "basement_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "basement_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
