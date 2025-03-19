{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_url_shortener (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\bin"
libdir     = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\lib\\x86_64-windows-ghc-9.8.4\\url-shortener-0.1.0.0-CqB6ta71MNjLLZ9Ut55sDd-url-shortener"
dynlibdir  = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\share\\x86_64-windows-ghc-9.8.4\\url-shortener-0.1.0.0"
libexecdir = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\libexec\\x86_64-windows-ghc-9.8.4\\url-shortener-0.1.0.0"
sysconfdir = "D:\\CODE\\url-shortner\\url-shortener\\.stack-work\\install\\6787729f\\etc"

getBinDir     = catchIO (getEnv "url_shortener_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "url_shortener_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "url_shortener_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "url_shortener_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "url_shortener_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "url_shortener_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
