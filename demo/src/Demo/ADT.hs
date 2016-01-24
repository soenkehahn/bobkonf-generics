{-# LANGUAGE DeriveGeneric #-}

module Demo.ADT where

import           GHC.Generics

type DemoADT = Person
type SingleConstructorADT = Animal

data Person
  = Person {
    name :: String,
    age :: Int
  }
  | Unknown
  deriving (Generic, Show)

data Animal
  = Animal {
    animalName :: Maybe String,
    species :: String,
    lives :: Int
  }
  deriving (Generic, Show)
