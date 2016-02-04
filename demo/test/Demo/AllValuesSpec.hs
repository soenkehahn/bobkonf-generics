{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Demo.AllValuesSpec where

import           GHC.Generics
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.QuickCheck

import           Demo.AllValues

data Foo
  = Foo (Either () Bool)
  | Bar
  deriving (Show, Eq, Generic, AllValues)

spec = do
  describe "combine" $ do
    it "" $ do
      let expected =
            ('a', 'x') :
            ('a', 'y') :
            ('b', 'y') :
            ('b', 'x') :
            []
      combine "ab" "xy" `shouldBe` expected

  describe "allValues" $ do
    it "returns all values" $ do
      let list =
            Foo (Left ()) :
            Bar :
            Foo (Right False) :
            Foo (Right True) :
            []
      allValues `shouldBe` list

    it "returns positive and negative Ints" $ do
      take 5 allValues `shouldBe` [0, 1, -1, 2, -2 :: Int]

    it "returns readable characters first" $ do
      take 3 allValues `shouldBe` ['a', 'b', 'c' :: Char]

    it "has a nice systematic for tuples" $ do
      let list :: [(Char, Char)]
          list =
            ('a', 'a') :
            []
      take (length list) allValues `shouldBe` list

    context "lists" $ do
      it "has a nice systematic for finite elements" $ do
        let list :: [[Bool]]
            list =
              [] :
              [False] :
              [False, False] :
              [True, False] :
              [True] :
              [False, False, False] :
              []
        take 6 allValues `shouldBe` list

      it "has a nice systematic for infinite elements" $ do
        let list :: [[Int]]
            list =
              [] :
              [0] :
              [0, 0] :
              [1, 0] :
              [1] :
              [0, 0, 0] :
              [1, 0, 0] :
              []
        take 7 allValues `shouldBe` list

  describe "allValuesApp" $ do
    with (return $ serve allValuesApi allValuesApp) $ do
      it "returns html" $ do
        get "/" `shouldRespondWith` 200
