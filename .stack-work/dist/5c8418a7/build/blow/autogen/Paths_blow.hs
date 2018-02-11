{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_blow (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\bin"
libdir     = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\lib\\x86_64-windows-ghc-8.2.2\\blow-0.1.0.0-8jGx6YNhyyJ3knTkpATjMx-blow"
dynlibdir  = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\share\\x86_64-windows-ghc-8.2.2\\blow-0.1.0.0"
libexecdir = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\libexec\\x86_64-windows-ghc-8.2.2\\blow-0.1.0.0"
sysconfdir = "E:\\dev\\prj\\haskell\\blow\\.stack-work\\install\\c1f464f3\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blow_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blow_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "blow_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "blow_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blow_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blow_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
