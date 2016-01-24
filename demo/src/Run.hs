{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Run (Run.run) where

import           Control.Monad.Trans.Except
import           Data.Maybe
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Logging.Facade as Log
import           WithCli

import           Demo.Default
import           Demo.HtmlForm
import           Demo.JSON ()
import           Demo.Swagger

run :: IO ()
run = withCli $ \ (fromMaybe 8080 -> port) -> do
  let settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  app :: Application <- serve demoApi <$> demoApp
  runSettings settings app

type DemoApi =
  "a" :> DefaultApi :<|>
  "b" :> HtmlFormApi :<|>
  "swagger" :> SwaggerApi :<|>
  Get '[] ()

demoApi :: Proxy DemoApi
demoApi = Proxy

demoApp :: IO (Server DemoApi)
demoApp = do
  htmlFormApp <- mkHtmlFormApp
  return $
    defaultApp :<|>
    htmlFormApp :<|>
    swaggerApp "/swagger" :<|>
    (throwE err404 {
      errBody = "404 - not found"
    })
