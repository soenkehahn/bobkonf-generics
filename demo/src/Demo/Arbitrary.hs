{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demo.Arbitrary where

import           Control.Monad.IO.Class
import           Generics.SOP.Arbitrary
import           Servant
import           Test.QuickCheck

import           Demo.ADT

type ArbitraryApi =
  Get '[JSON] [Either DemoADT SingleConstructorADT]

arbitraryApp :: Server ArbitraryApi
arbitraryApp = do
  liftIO $ sample' arbitrary

instance Arbitrary DemoADT where
  arbitrary = garbitrary

instance Arbitrary SingleConstructorADT where
  arbitrary = garbitrary
