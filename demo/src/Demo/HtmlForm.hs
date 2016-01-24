
module Demo.HtmlForm where

import           Network.Wai
import           Network.Wai.Ghcjs
import           Servant

type HtmlFormApi = Raw

mkHtmlFormApp :: IO Application
mkHtmlFormApp = mkDevelopmentApp $ BuildConfig {
  mainFile = "Main.hs",
  customIndexFile = Just "static/index.html",
  sourceDirs = [".", "../src"],
  projectDir = "client",
  projectExec = Stack,
  buildDir = "_build"
}
