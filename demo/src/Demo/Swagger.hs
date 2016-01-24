{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demo.Swagger (swaggerApp, SwaggerApi) where

import           Control.Lens
import           Data.String.Conversions
import           Data.Swagger
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Swagger
import           System.FilePath
import qualified System.Logging.Facade as Log

import           Demo.ADT
import           Demo.Default

type SwaggerApi =
  "swagger.json" :> Get '[JSON] Swagger :<|>
  "api" :> SimpleApi :<|>
  "swagger-ui" :> Raw :<|>
  Raw

swaggerApp :: String -> Server SwaggerApi
swaggerApp base =
  return (simpleSwaggerSpec base) :<|>
  simpleApp :<|>
  serveDirectory "static/swagger-ui/dist" :<|>
  swaggerUiRedirect

swaggerUiRedirect :: Application
swaggerUiRedirect = redirect $ \ base ->
  base </> "swagger-ui/?url=" ++
    cs (urlEncode False (cs (base </> "swagger.json")))

simpleSwaggerSpec :: String -> Swagger
simpleSwaggerSpec base =
  basePath .~ Just (base ++ "/api") $
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

redirect :: (String -> String) -> Application
redirect target request respond = do
  let url = target (cs (rawPathInfo request))
  respond $ responseLBS (Status 307 "Temporary Redirect") [("Location", cs url)] ""
