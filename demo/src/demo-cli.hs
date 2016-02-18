{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.ByteString.Lazy.Char8 as LBS
import           WithCli

data Person
  = Person {
    name :: String,
    age :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, HasArguments)

alice :: Person
alice = Person {
  name = "Alice",
  age = 42
}

bobEncoded :: LBS.ByteString
bobEncoded = "{\"name\": \"Bob\", \"age\": 23}"

main :: IO ()
main = withCli $ \ person -> do
  print (person :: Person)
