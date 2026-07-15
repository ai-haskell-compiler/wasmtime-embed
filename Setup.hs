module Main (main) where

import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (UserHooks (buildHook), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir, hostPlatform)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.UserHooks (Args)
import Distribution.System (Arch (AArch64), OS (OSX), Platform (Platform))
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks =
      simpleUserHooks
        { buildHook = prepareStaticArchive
        }

prepareStaticArchive ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
prepareStaticArchive package localBuildInfo userHooks flags = do
  let outputDirectory = getSymbolicPath (buildDir localBuildInfo)
      source =
        "vendor"
          </> "wasmtime"
          </> targetKey (hostPlatform localBuildInfo)
          </> "lib"
          </> "libwasmtime.a"
  available <- doesFileExist source
  if available
    then pure ()
    else
      fail $
        "missing pinned Wasmtime artifact "
          ++ source
          ++ "; run scripts/prepare-wasmtime.py first"
  createDirectoryIfMissing True outputDirectory
  copyFile source (outputDirectory </> "libCwasmtime.a")
  buildHook simpleUserHooks package localBuildInfo userHooks flags

targetKey :: Platform -> FilePath
targetKey (Platform AArch64 OSX) = "aarch64-darwin"
targetKey platform =
  error $ "no Wasmtime artifact mapping for Cabal platform " ++ show platform
