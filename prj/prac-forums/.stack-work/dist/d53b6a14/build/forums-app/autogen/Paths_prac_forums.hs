{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_prac_forums (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\bin"
libdir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\lib\\x86_64-windows-ghc-9.0.2\\prac-forums-0.0.0.1-1rprr2DZU2NJdAxZdI9G9L-forums-app"
dynlibdir  = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\share\\x86_64-windows-ghc-9.0.2\\prac-forums-0.0.0.1"
libexecdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\libexec\\x86_64-windows-ghc-9.0.2\\prac-forums-0.0.0.1"
sysconfdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\prj\\.stack-work\\install\\06d9469a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prac_forums_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prac_forums_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prac_forums_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prac_forums_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prac_forums_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prac_forums_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
