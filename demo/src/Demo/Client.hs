{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Demo.Client where

import           Network.Wai
import           Network.Wai.Ghcjs
import           Servant

type ClientApi = Capture "path" String :> Raw

mkClientApp :: IO (String -> Application)
mkClientApp = do
  app <- mkDevelopmentApp $ BuildConfig {
    mainFile = "Main.hs",
    customIndexFile = Just "static/index.html",
    sourceDirs = [".", "../src"],
    projectDir = "client",
    projectExec = Stack,
    buildDir = "_build"
  }
  return $ const app
