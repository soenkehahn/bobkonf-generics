{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Demo.ADT where

import           GHC.Generics
import qualified Generics.SOP

type DemoADT = Person
type SingleConstructorADT = Animal

data Person
  = Person {
    name :: String,
    age :: Int
  }
  | Unknown
  deriving (Generic, Show)

instance Generics.SOP.Generic DemoADT

data Animal
  = Animal {
    animalName :: Maybe String,
    species :: String,
    lives :: Int
  }
  deriving (Generic, Show)

instance Generics.SOP.Generic SingleConstructorADT
