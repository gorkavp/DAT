{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_drawing (
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
version = Version [0,2,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\bin"
libdir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\lib\\x86_64-windows-ghc-9.0.2\\drawing-0.2.2.0-2SLlrKbUgLP4IfD3r1VUQK"
dynlibdir  = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\share\\x86_64-windows-ghc-9.0.2\\drawing-0.2.2.0"
libexecdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\libexec\\x86_64-windows-ghc-9.0.2\\drawing-0.2.2.0"
sysconfdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "drawing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "drawing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "drawing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "drawing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "drawing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "drawing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
