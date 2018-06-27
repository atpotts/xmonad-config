{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_xmonad_config (
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

bindir     = "/home/alistairtpotts/.cabal/bin"
libdir     = "/home/alistairtpotts/.cabal/lib/x86_64-linux-ghc-8.2.2/xmonad-config-0.1.0.0-3pA8xc1d40S1LEjnpDu2aH-xmonad-config"
dynlibdir  = "/home/alistairtpotts/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/alistairtpotts/.cabal/share/x86_64-linux-ghc-8.2.2/xmonad-config-0.1.0.0"
libexecdir = "/home/alistairtpotts/.cabal/libexec/x86_64-linux-ghc-8.2.2/xmonad-config-0.1.0.0"
sysconfdir = "/home/alistairtpotts/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_config_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmonad_config_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmonad_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_config_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
