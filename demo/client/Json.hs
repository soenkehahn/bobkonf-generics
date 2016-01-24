{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Json where

import           Control.Monad
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

run :: forall adt . (HasEot adt, EotDefault (Eot adt), Show adt, ToJSON adt, FromJSON adt) =>
  Proxy adt -> IO ()
run proxy = do
  mainWidget $ el "div" $ do
    el "p" $ el "pre" $ mdo
      dynText =<< (mapDyn (parse proxy) $ _textArea_value t)
      el "hr" (return ())
      let attrs = constDyn $ Map.fromList [("style", "height: 400px; width: 600px")]
      t <- textArea $ def
        & textAreaConfig_initialValue .~ (cs $ encodePretty (genericDefault :: adt))
        & textAreaConfig_attributes .~ attrs
      return ()

parse :: forall adt . (FromJSON adt, Show adt) =>
  Proxy adt -> String -> String
parse _ s = show (eitherDecode' (cs s) :: Either String adt)
