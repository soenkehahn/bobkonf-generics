{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import           Data.Maybe
import           Network.Wai
import           Network.Wai.Ghcjs
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Logging.Facade as Log
import           WithCli

main :: IO ()
main = withCli $ \ (fromMaybe 8080 -> port) -> do
  let settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  app :: Application <- serve demoApi <$> demoApp
  runSettings settings app

type DemoApi =
  "a" :> HtmlFormApi :<|>
  "b" :> Get '[PlainText] String

demoApi :: Proxy DemoApi
demoApi = Proxy

demoApp :: IO (Server DemoApi)
demoApp = do
  htmlFormApp <- mkHtmlFormApp
  return $
    htmlFormApp :<|>
    return ("huhu" :: String)

type HtmlFormApi = Raw

mkHtmlFormApp :: IO Application
mkHtmlFormApp = mkDevelopmentApp $ BuildConfig {
  mainFile = "Main.hs",
  customIndexFile = Just "static/index.html",
  sourceDirs = [".", "../src"],
  projectDir = "client",
  projectExec = Stack,
  buildDir = "_build"
}
