{-# LANGUAGE OverloadedStrings #-}

module Demo.ArbitrarySpec where

import           Data.Aeson
import           Data.ByteString.Lazy
import           Network.Wai.Test
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai

import           Demo.Adt
import           Run

spec :: Spec
spec = do
  with demoApplication $ do
    describe "/arbitrary" $ do
      it "serves valid json" $ do
        r <- get "/arbitrary"
        let d = decode
            d :: ByteString -> Maybe [Either DemoADT SingleConstructorADT]
        liftIO $ simpleBody r `shouldSatisfy` isJust . d

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
