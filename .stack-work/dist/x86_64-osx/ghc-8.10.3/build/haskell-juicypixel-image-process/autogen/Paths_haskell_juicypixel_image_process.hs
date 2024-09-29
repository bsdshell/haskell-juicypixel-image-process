{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_juicypixel_image_process (
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

bindir     = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/bin"
libdir     = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/lib/x86_64-osx-ghc-8.10.3/haskell-juicypixel-image-process-0.1.0.0-DE9DgKqkEZqK4kqj95yRrN-haskell-juicypixel-image-process"
dynlibdir  = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/share/x86_64-osx-ghc-8.10.3/haskell-juicypixel-image-process-0.1.0.0"
libexecdir = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/libexec/x86_64-osx-ghc-8.10.3/haskell-juicypixel-image-process-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/github/haskell-juicypixel-image-process/.stack-work/install/x86_64-osx/2c84aec8038c668e8bb96f9fd6ee56eaa461166f8e8398f2123e0b8dcc0931d1/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_juicypixel_image_process_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_juicypixel_image_process_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_juicypixel_image_process_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_juicypixel_image_process_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_juicypixel_image_process_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_juicypixel_image_process_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
