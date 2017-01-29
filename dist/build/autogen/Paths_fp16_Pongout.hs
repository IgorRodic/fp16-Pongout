{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fp16_Pongout (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Igor\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Igor\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\fp16-Pongout-0.1.0.0-IbaPkVAcpMP3IqiQ1elLcj"
datadir    = "C:\\Users\\Igor\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\fp16-Pongout-0.1.0.0"
libexecdir = "C:\\Users\\Igor\\AppData\\Roaming\\cabal\\fp16-Pongout-0.1.0.0-IbaPkVAcpMP3IqiQ1elLcj"
sysconfdir = "C:\\Users\\Igor\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fp16_Pongout_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fp16_Pongout_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fp16_Pongout_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fp16_Pongout_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fp16_Pongout_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
