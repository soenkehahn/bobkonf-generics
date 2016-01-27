{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import           WithCli

main :: IO ()
main = withCli $ \ config -> do
  print (config :: Config)

data Config
  = Config {
    host :: String,
    port :: String
  }
  deriving (Show, Generic, HasArguments)
