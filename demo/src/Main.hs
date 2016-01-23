{-# LANGUAGE ViewPatterns #-}

import           Data.Maybe
import           Network.Wai.Ghcjs
import           Network.Wai.Handler.Warp
import qualified System.Logging.Facade as Log
import           WithCli

main :: IO ()
main = withCli $ \ (fromMaybe 8080 -> port) -> do
  app <- mkDevelopmentApp $ BuildConfig {
    mainFile = "Main.hs",
    customIndexFile = Nothing,
    sourceDirs = [".", "../src"],
    projectDir = "client",
    projectExec = Stack,
    buildDir = "_build"
  }
  let settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings app
