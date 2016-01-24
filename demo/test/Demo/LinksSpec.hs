{-# LANGUAGE OverloadedStrings #-}

module Demo.LinksSpec where

import           Data.String.Conversions
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai

import           Run

spec :: Spec
spec = do
  with demoApplication $ do
    describe "/" $ do
      it "contains a link to /swagger" $ do
        r <- get "/"
        liftIO $
          cs (simpleBody r) `shouldContain` ("href=\"/swagger\"" :: String)
