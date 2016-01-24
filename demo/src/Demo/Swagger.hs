{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demo.Swagger (swaggerApp, SwaggerApi) where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.String.Conversions
import           Data.Swagger
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Swagger
import qualified System.Logging.Facade as Log

import           Demo.ADT
import           Demo.Default

type SwaggerApi =
  "swagger.json" :> Raw :<|>
  "api" :> SimpleApi :<|>
  "swagger-ui" :> Raw :<|>
  Get '[] ()

swaggerApp :: Server SwaggerApi
swaggerApp =
  simpleSwaggerSpec :<|>
  simpleApp :<|>
  serveDirectory "static/swagger-ui/dist" :<|>
  swaggerUiRedirect

swaggerUiRedirect :: ExceptT ServantErr IO ()
swaggerUiRedirect = redirect $ "./swagger-ui/?url=" ++
    cs (urlEncode False "../swagger.json")

simpleSwaggerSpec :: Application
simpleSwaggerSpec request respond = do
  let base = rawPathInfo request
  respond $ responseLBS ok200 [("content-type", "application/json")] $
    encode $
      basePath .~ Just (cs base ++ "/../api") $
      toSwagger simpleApi

instance ToSchema SingleConstructorADT

type SimpleApi =
  Get '[JSON] SingleConstructorADT :<|>
  ReqBody '[JSON] SingleConstructorADT :> Post '[JSON] ()

simpleApi :: Proxy SimpleApi
simpleApi = Proxy

simpleApp :: Server SimpleApi
simpleApp =
  return genericDefault :<|>
  (\ animal -> do
    Log.info ("POST: " ++ show animal)
    return ())

-- * redirects

redirect :: String -> ExceptT ServantErr IO ()
redirect target = do
  throwE $ err307 { errHeaders = [("Location", cs target)] }
