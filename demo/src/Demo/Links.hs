{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Ok, this is a bit over the top... but hey, it's a demo for generic
-- programming. ;)
module Demo.Links (mkLinks) where

import           Data.Proxy
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           GHC.TypeLits
import           Servant.API

mkLinks :: MkPaths api => Proxy api -> String
mkLinks proxy = unindent [i|
  <html>
    <head>
      <title>
        DGP Demo -- BOB Konferenz 2016
      </title>
    </head>
    <body>
      <h4>Demos:</h4>
      #{links}
    </body>
  </html>
|]
  where
    links :: String
    links = concatMap (\ u -> [i|<a href="/#{u}/">#{u}</a><br/>\n|])
      (mkUrls proxy)

class MkPaths api where
  mkUrls :: Proxy api -> [String]

instance {-# OVERLAPPING #-} (KnownSymbol path, MkPaths rest) =>
  MkPaths (path :> sub :<|> rest) where

  mkUrls _ =
    path : mkUrls rest
    where
      path = symbolVal (Proxy :: Proxy path)
      rest :: Proxy rest
      rest = Proxy

instance {-# OVERLAPPING #-} MkPaths a where
  mkUrls _ = []
