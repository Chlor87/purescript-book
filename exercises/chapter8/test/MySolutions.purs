module Test.MySolutions where

import Prelude

import Control.Monad.ST.Internal (for, modify, new, read, run)
import Data.Array (head, sort, tail)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Int (even, toNumber)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Exception (error, throwException)

-- Note to reader: Add your solutions to this file

third' :: ∀ a. Array a -> Maybe a
third' xs = do
  a <- tail xs
  b <- tail a
  head b

third :: forall a. Array a -> Maybe a
third = head <=< tail <=< tail

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< foldM (\p c -> [ p, p + c ]) 0

filterM :: ∀ m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  ok <- f x
  xs' <- filterM f xs
  pure $ if ok then x : xs' else xs'

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure $ a / b

estimatePi :: Int -> Number
estimatePi k = run do
  ref <- new 0.0
  for 1 k (\k' -> modify (go k') ref)
  res <- read ref
  pure $ res * 4.0
  where
  go :: Int -> Number -> Number
  go k = (+) (a / (2.0 * (toNumber k) - 1.0))
    where
    a = if even k then -1.0 else 1.0

fibonacci :: Int -> Int
fibonacci n = run do
  ref <- new { p: 0, c: 1 }
  for 1 n \_ -> modify (\{ p, c } -> { p: c, c: p + c }) ref
  res <- read ref
  pure res.c

main :: Effect Unit
main = do
  logShow ""
