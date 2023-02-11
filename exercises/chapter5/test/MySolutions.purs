module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Person (Person)
import Data.Picture (Bounds, Picture, Point, Shape(..), origin)
import Data.Picture as P

factorial :: Int -> Int
factorial = go 1
  where
  go :: Int -> Int -> Int
  go _ 0 = 1
  go acc 1 = acc
  go acc n = go (acc * n) (n - 1)

binomial :: Int -> Int -> Int
binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal n k
  | k == 0 = 1
  | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity = eq `on` _.address.city

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x
fromSingleton def _ = def

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin $ r * 2.0
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (2.0 * w) (2.0 * h)
doubleScaleAndCenter (Line { x: x1, y: y1 } { x: x2, y: y2 }) = Line { x: -dx, y: -dy } { x: dx, y: dy }
  where
  dx = x2 - x1
  dy = y2 - y1

doubleScaleAndCenter (Text _ t) = Text origin t

shapeText :: Shape -> Maybe String
shapeText (Text _ t) = pure t
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt $ a * v

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area _ = 0.0

data ShapeExt
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBounds :: ShapeExt -> Bounds
shapeBounds (Shape s) = P.shapeBounds s
shapeBounds (Clipped p o w h) = P.intersect (P.bounds p) (P.shapeBounds $ Rectangle o w h)
