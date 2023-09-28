import Prelude

import Data.Maybe
  ( fromJust
  )
import Distribution.Simple
  ( Args
  , UserHooks (..)
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (..),
  )
import Distribution.Simple.Setup
  ( BuildFlags (..)
  , ConfigFlags (..)
  , fromFlag
  )
import Distribution.Simple.UserHooks
  ( UserHooks (..)
  )
import Distribution.Simple.Utils
  ( rawSystemExit
  )
import System.Directory
  ( getCurrentDirectory
  )

import qualified Distribution.PackageDescription as Pkg

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = rustConfHook
      , buildHook = rustBuildHook
      }

rustConfHook
  :: (Pkg.GenericPackageDescription, Pkg.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
rustConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
  let library = fromJust $ Pkg.library packageDescription
  let libraryBuildInfo = Pkg.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo
    { localPkgDescr = packageDescription
      { Pkg.library = Just library
        { Pkg.libBuildInfo = libraryBuildInfo
          { Pkg.extraLibDirs = (dir ++ "/target/release") : Pkg.extraLibDirs libraryBuildInfo
          }
        }
      }
    }

rustBuildHook
  :: Pkg.PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
rustBuildHook description localBuildInfo hooks flags = do
  putStrLn "🦀 Compiling Rust dependencies..."
  putStrLn "🦀 cargo build --release"
  rawSystemExit (fromFlag $ buildVerbosity flags) "cargo" ["build", "--release"]
  buildHook simpleUserHooks description localBuildInfo hooks flags
