module Data.PTree where

import Prelude hiding (add)

import Data.Generic.Rep (class Generic)
import Data.List (List(..), foldl, foldr, (:))
import Data.List (length) as L
import Data.Map (insert, lookup)
import Data.Map.Internal (Map(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Class.Console (logShow)

newtype PTree = PTree (Map Char PTree)

derive instance Generic PTree _

instance Show PTree where
  show t = genericShow t

empty :: PTree
empty = PTree Leaf

toCharList :: String -> List Char
toCharList str = go (length str - 1) Nil
  where
  go :: Int -> List Char -> List Char
  go idx acc
    | Just x <- charAt idx str = go (idx - 1) (x : acc)
    | otherwise = acc

add :: String -> PTree -> PTree
add = go <<< toCharList
  where
  go :: List Char -> PTree -> PTree
  go (x : xs) (PTree t) = PTree
    $ insert x (go xs <<< fromMaybe empty <<< lookup x $ t) t
  go _ t = t

infixr 6 add as ++

has :: String -> PTree -> Boolean
has = go <<< toCharList
  where
  go :: List Char -> PTree -> Boolean
  go (x : xs) (PTree t)
    | Just next <- lookup x t = go xs next
    | otherwise = false
  go _ _ = true

example :: PTree
example = "armed" ++ "forces" ++ "americas" ++ empty

mapWithIndex :: forall a b. (Int -> a -> b) -> List a -> List b
mapWithIndex f xs = _.acc
  $ foldr
      (\c { i, acc } -> { acc: ((f i c) : acc), i: i - 1 })
      { i: L.length xs - 1, acc: Nil }
      xs

main :: Effect Unit
main = do
  -- logShow $ example
  -- logShow $ (<@>) has example <$> [ "armed", "forces", "americas", "nothing" ]
  logShow $ mapWithIndex (\i x -> { i, x }) $ 1 : 2 : 3 : Nil
