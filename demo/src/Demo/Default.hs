
module Demo.Default where

import           Demo.ADT

-- fixme: make generic
genericDefault :: SingleConstructorADT
genericDefault = Animal {
  species = "cat",
  lives = 9,
  animalName = Just "Tom"
}
