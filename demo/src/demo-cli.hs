{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           WithCli

import           DemoADT

main :: IO ()
main = withCli $ \ (x :: SingleConstructorADT) -> do
  print x

instance HasArguments SingleConstructorADT
