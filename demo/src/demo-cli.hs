{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           WithCli

import           Demo.ADT

main :: IO ()
main = withCli $ \ (x :: SingleConstructorADT) -> do
  print x

instance HasArguments SingleConstructorADT
