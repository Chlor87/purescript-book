module Test.MySolutions where

import Data.Function.Uncurried (Fn3)
import Test.Examples (Complex)

-- Note to reader: Add your solutions to this file

foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex