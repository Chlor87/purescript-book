module Test.MySolutions where

import Prelude

import Data.Array (head, sort, tail)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file

third' :: forall a. Array a -> Maybe a
third' xs = do
  a <- tail xs
  b <- tail a
  head b

third :: forall a. Array a -> Maybe a
third = head <=< tail <=< tail

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< foldM (\p c -> [ p, p + c ]) 0

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  ok <- f x
  xs' <- filterM f xs
  pure $ if ok then x : xs' else xs'
