{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Demo.Adt where

import           GHC.Generics
import qualified Generics.SOP

type DemoADT = User
type SingleConstructorADT = Animal

data User
  = User {
    name :: String,
    age :: Int
  }
  | Anonymous
  deriving (Generic, Show)

instance Generics.SOP.Generic DemoADT

data Animal
  = Animal {
    animalName :: Maybe String,
    species :: String,
    lives :: Int
  }
  deriving (Generic, Show, Eq)

instance Generics.SOP.Generic SingleConstructorADT
