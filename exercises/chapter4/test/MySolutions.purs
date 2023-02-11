module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, head, length, uncons, (..), (:))
import Data.Foldable (foldl)
import Data.Int (rem)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Ord (lessThanOrEq)
import Data.Path (Path, filename, isDirectory, ls)
import Test.Examples (factorsV3)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n
  | n < 0 = isEven $ negate n
  | n == 0 = true
  | otherwise = not $ isEven $ n - 1

countEven :: Array Int -> Int
countEven xs
  | Just { head, tail } <- uncons xs = (length $ guard $ isEven head) + countEven tail
  | otherwise = 0

squared :: forall f. Functor f => f Number -> f Number
squared = map $ flip pow 2.0

infixl 4 filter as <$?>

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter $ lessThanOrEq 0.0

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = lessThanOrEq 0.0 <$?> xs

isPrime :: Int -> Boolean
isPrime n = n /= 1 && length (factorsV3 n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  a' <- a
  b' <- b
  pure [ a', b' ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n = go n 2
  where
  go :: Int -> Int -> Array Int
  go 1 _ = []
  go n div
    | n `rem` div == 0 = div : (go (n / div) div)
    | otherwise = go n (div + 1)

allTrue :: Array Boolean -> Boolean
allTrue = foldl conj true

fibTailRec :: Int -> Int
fibTailRec n = go 0 1 n
  where
  go :: Int -> Int -> Int -> Int
  go p c n'
    | n' == 0 = 0
    | n' == 1 = c
    | otherwise = go c (p + c) (n' - 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (flip (:)) []

onlyFiles :: Path -> Array Path
onlyFiles = ls >=> \x -> if isDirectory x then onlyFiles x else pure x

onlyDirs :: Path -> Array Path
onlyDirs = ls >=> \x -> if isDirectory x then pure x else onlyDirs x

whereIs :: Path -> String -> Maybe Path
whereIs root file = head $ do
  dir <- onlyDirs root
  f <- onlyFiles dir
  guard $ filename dir <> file == filename f
  pure dir

largestSmallest :: Path -> Array Path
largestSmallest = foldl go [] <<< onlyFiles
  where
  go :: Array Path -> Path -> Array Path
  go p@[ min, max ] curr
    | curr < min = [ curr, max ]
    | curr > max = [ min, curr ]
    | otherwise = p
  go [ prev ] curr
    | curr < prev = [ curr, prev ]
    | otherwise = [ prev, curr ]
  go _ curr = [ curr ]
