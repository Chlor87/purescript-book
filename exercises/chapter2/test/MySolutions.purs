module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal a b = sqrt (a * a + b * b)

circleArea :: Number -> Number
circleArea r = pi * r * r

leftoverCents :: Int -> Int
leftoverCents = flip rem 100
