{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_shoot_em_up (
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

bindir     = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\bin"
libdir     = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\lib\\x86_64-windows-ghc-8.6.5\\shoot-em-up-0.1.0.0-4CWnk7L5G1g1oFPHmai5T-shoot-em-up"
dynlibdir  = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\share\\x86_64-windows-ghc-8.6.5\\shoot-em-up-0.1.0.0"
libexecdir = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\libexec\\x86_64-windows-ghc-8.6.5\\shoot-em-up-0.1.0.0"
sysconfdir = "D:\\University\\Functioneel Programmeren\\Game\\Shoot-Em-Up\\.stack-work\\install\\2a26e89c\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shoot_em_up_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shoot_em_up_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "shoot_em_up_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "shoot_em_up_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shoot_em_up_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shoot_em_up_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
