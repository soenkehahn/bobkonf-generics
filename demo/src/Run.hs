{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Run (Run.run, demoApi, demoApp) where

import           Data.Maybe
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Logging.Facade as Log
import           WithCli

import           Demo.Default
import           Demo.HtmlForm
import           Demo.JSON ()
import           Demo.Links
import           Demo.Swagger
import           Demo.Utils

run :: IO ()
run = withCli $ \ (fromMaybe 8080 -> port) -> do
  let settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  app :: Application <- serve demoApi <$> demoApp
  runSettings settings app

type DemoApi =
  "default-value" :> DefaultApi :<|>
  "html-form" :> HtmlFormApi :<|>
  "swagger" :> SwaggerApi :<|>
  Get '[HtmlString] String

demoApi :: Proxy DemoApi
demoApi = Proxy

demoApp :: IO (Server DemoApi)
demoApp = do
  htmlFormApp <- mkHtmlFormApp
  return $
    defaultApp :<|>
    htmlFormApp :<|>
    swaggerApp "/swagger" :<|>
    return (mkLinks (Proxy :: Proxy DemoApi))
