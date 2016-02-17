{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Demo.SchemaSpec where

import           Control.Exception
import           Data.Proxy
import           Database.PostgreSQL.Simple hiding (query)
import           Test.Hspec
import           Test.QuickCheck

import           Demo.Adt
import           Demo.Arbitrary
import           Demo.Schema

spec :: Spec
spec = do
  withDB $ do
    describe "schema" $ do
      it "creates a valid schema" $ \ connection -> do
        createSchema connection (Proxy :: Proxy SingleConstructorADT) "foo"

    describe "query" $ do
      it "allows to insert and query values" $ \ connection ->
        property $ \ (list :: [SingleConstructorADT]) -> do
          createSchema connection (Proxy :: Proxy SingleConstructorADT) "foo"
          insert connection "foo" list
          output <- query connection "foo"
          output `shouldMatchList` list

withDB :: SpecWith Connection -> Spec
withDB = around $ \ test -> bracket start stop $ \ connection -> do
  withTransaction connection $ do
    test connection
    rollback connection
  where
    start = do
      connectPostgreSQL "host=localhost user=postgres"
    stop = close
