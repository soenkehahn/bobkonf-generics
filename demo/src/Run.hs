{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Run (Run.run, demoApi, demoApplication) where

import           Data.Maybe
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Logging.Facade as Log
import           WithCli

import           Demo.ADT
import           Demo.Arbitrary
import           Demo.Client
import           Demo.Default
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
  app :: Application <- demoApplication
  runSettings settings app

type DemoApi =
  "default" :> DefaultApi :<|>
  "arbitrary" :> ArbitraryApi :<|>
  "json" :> ClientApi :<|>
  "html-form" :> ClientApi :<|>
  "swagger" :> SwaggerApi :<|>
  Get '[HtmlString] String

demoApi :: Proxy DemoApi
demoApi = Proxy

demoApplication :: IO Application
demoApplication = serve demoApi <$> demoApp

demoApp :: IO (Server DemoApi)
demoApp = do
  clientApp <- mkClientApp
  return $
    defaultApp :<|>
    arbitraryApp :<|>
    clientApp :<|>
    clientApp :<|>
    swaggerApp :<|>
    return (mkLinks (Proxy :: Proxy DemoApi))

-- default app

type DefaultApi = GetString

defaultApp :: Server DefaultApi
defaultApp = return $ show (genericDefault :: DemoADT)
