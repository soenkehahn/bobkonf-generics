{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Demo.Default where

import           Generics.Eot

genericDefault :: (HasEot a, EotDefault (Eot a)) => a
genericDefault = fromEot eotDefault

class EotDefault eot where
  eotDefault :: eot

instance EotDefault x => EotDefault (Either x xs) where
  eotDefault = Left eotDefault

instance (Default x, EotDefault xs) => EotDefault (x, xs) where
  eotDefault = (def, eotDefault)

instance EotDefault () where
  eotDefault = ()

class Default a where
  def :: a

instance Default Int where
  def = 42

instance Default String where
  def = "some string"

instance Default a => Default (Maybe a) where
  def = Just def

instance Default Bool where
  def = True

