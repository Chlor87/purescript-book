module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file

newtype Point = Point { x :: Number, y :: Number }

instance Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex = Complex { real :: Number, imaginary :: Number }

instance Show Complex where
  show (Complex { real: r, imaginary: i }) = show r <> (if i >= 0.0 then "+" else "") <> show i <> "i"

derive newtype instance Eq Complex
derive instance Newtype Complex _

-- (a + bi)(c + di) = ac - bidi +  adi + bic
instance Semiring Complex where
  zero = wrap { real: zero, imaginary: zero }
  one = wrap { real: one, imaginary: zero }
  add = over2 Complex (+)
  mul = over2 Complex \{ real: a, imaginary: bi } { real: c, imaginary: di } ->
    { real: a * c - bi * di, imaginary: a * di + c * bi }

derive newtype instance Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance Generic Shape _

instance Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance Generic (NonEmpty a) _
instance Show a => Show (NonEmpty a) where
  show = genericShow

derive instance Eq a => Eq (NonEmpty a)

-- manual
-- instance Eq a => Eq (NonEmpty a) where
--   eq (NonEmpty x xs) (NonEmpty y ys) = x == y && eq xs ys

instance Semigroup (Array a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x $ xs <> [ y ] <> ys

derive instance Functor NonEmpty

-- manual
-- instance Functor NonEmpty where
--   map f (NonEmpty x xs) = NonEmpty (f x) $ f <$> xs

data Extended a
  = Infinite
  | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = a `compare` b

instance Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f (f acc x) xs
  foldr f acc (NonEmpty x xs) = f x $ foldr f acc xs
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldr f acc (OneMore x xs) = f x $ foldr f acc xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs

derive instance Eq Point
derive instance Eq Shape

derive instance Ord Point
derive instance Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs | Just x <- maximum xs = x

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive instance Newtype Multiply _

derive newtype instance Show Multiply

derive instance Eq Multiply

instance Semigroup Multiply where
  append (Multiply a) (Multiply b) = wrap (a * b)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply a) = (*) a

instance Action Multiply String where
  act (Multiply a) = (<@>) power a

instance Action m a => Action m (Array a) where
  -- act m a = act m <$> a
  act = map <<< act

newtype Self m = Self m

derive newtype instance Show m => Show (Self m)

derive instance Eq m => Eq (Self m)

instance Monoid m => Action m (Self m) where
  act m (Self a) = Self $ m <> a

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = (notEq `on` length) (nubByEq (\a b -> hashEqual a b && a == b) xs) (xs)

newtype Hour = Hour Int

derive instance Newtype Hour _

instance Eq Hour where
  eq = eq `on` (flip mod 12 <<< unwrap)

instance Hashable Hour where
  hash = hash <<< flip mod 12 <<< unwrap
