{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Json where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Map
import           Data.Map as Map
import           Data.String.Conversions
import           Data.Typeable
import           Generics.Eot
import           Reflex.Dom
import           Safe

import           Demo.Default (genericDefault, EotDefault)
import           Demo.JSON

run :: forall a b .
  (HasEot a, EotDefault (Eot a), Show a, ToJSON a, FromJSON a,
   HasEot b, EotDefault (Eot b), Show b, ToJSON b, FromJSON b) =>
  Proxy a -> Proxy b -> IO ()
run a b = do
  mainWidget $ do
    jsonExample a
    el "hr" (return ())
    jsonExample b

jsonExample :: forall adt m t .
  (MonadWidget t m,
   HasEot adt, EotDefault (Eot adt), Show adt, ToJSON adt, FromJSON adt) =>
  Proxy adt -> m ()
jsonExample proxy = do
  el "div" $ do
    el "p" $ mdo
      el "h3" $ text (datatypeName (datatype proxy) ++ ":")
      el "hr" (return ())
      el "pre" $ do
        dynText =<< (mapDyn (parse proxy) $ _textArea_value t)
      el "hr" (return ())
      let attrs = constDyn $ Map.fromList [("style", "height: 130px; width: 600px")]
      t <- textArea $ def
        & textAreaConfig_initialValue .~ (cs $ encodePretty (genericDefault :: adt))
        & textAreaConfig_attributes .~ attrs
      return ()

parse :: forall adt . (FromJSON adt, Show adt) =>
  Proxy adt -> String -> String
parse _ s = show (eitherDecode' (cs s) :: Either String adt)
