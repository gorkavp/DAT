{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_prac_prog_fun (
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

bindir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\bin"
libdir     = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\lib\\x86_64-windows-ghc-9.0.2\\prac-prog-fun-0.0.0.1-LduzV01u2I2AYzWoQtnUWy"
dynlibdir  = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\share\\x86_64-windows-ghc-9.0.2\\prac-prog-fun-0.0.0.1"
libexecdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\libexec\\x86_64-windows-ghc-9.0.2\\prac-prog-fun-0.0.0.1"
sysconfdir = "C:\\Users\\Gorka\\VisualStudioCodeProjects\\Haskell\\DAT\\dat-2023p\\prj\\.stack-work\\install\\7d2a49d5\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prac_prog_fun_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prac_prog_fun_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prac_prog_fun_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prac_prog_fun_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prac_prog_fun_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prac_prog_fun_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
