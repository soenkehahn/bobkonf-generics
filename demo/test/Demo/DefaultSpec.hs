{-# LANGUAGE DeriveGeneric #-}

module Demo.DefaultSpec where

import           GHC.Generics
import           Test.Hspec

import           Demo.Default

spec :: Spec
spec = do
  describe "genericDefault" $ do
    it "returns a default" $ do
      genericDefault `shouldBe` Foo1 {
        foo = 42,
        bar = "some string"
      }

data Foo
  = Foo1 {
    foo :: Int,
    bar :: String
  }
  deriving (Generic, Show, Eq)
