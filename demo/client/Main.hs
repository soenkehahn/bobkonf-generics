{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

module Main where

import           Data.Proxy
import           GHCJS.Prim (fromJSString)
import           JavaScript.Object
import           Reflex.Dom

import           Demo.ADT
import           HtmlForm
import           Json

foreign import javascript unsafe "(function () { return location; })()"
  js_location :: IO Object

main :: IO ()
main = do
  location <- js_location
  path <- fromJSString <$> getProp "pathname" location
  case path of
    "/html-form" -> HtmlForm.run
    "/json" -> Json.run (Proxy :: Proxy DemoADT)
    _ -> do
      mainWidget $ el "div" $ do
        text ("unknown client demo: " ++ path)

