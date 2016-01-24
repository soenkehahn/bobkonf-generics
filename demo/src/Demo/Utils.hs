{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Demo.Utils where

import           Data.String.Conversions
import           Servant.API

type GetString = Get '[PlainText] String

data HtmlString

instance MimeRender HtmlString String where
  mimeRender _ = cs

instance Accept HtmlString where
  contentType _ = "text/html"
