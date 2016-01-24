{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demo.JSON where

import           Data.Aeson

import           Demo.ADT

instance ToJSON SingleConstructorADT
instance FromJSON SingleConstructorADT

instance ToJSON DemoADT
instance FromJSON DemoADT
