module Test.MySolutions where

import Prelude

import Data.AddressBook (Address, PhoneNumber, address, person)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumber)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex, search)
import Data.String.Regex.Flags (ignoreCase, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)
import Effect (Effect)
import Effect.Class.Console (logShow)

-- Note to reader: Add your solutions to this file

combineList :: ∀ f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (x : xs) = ado
  x' <- x
  xs' <- combineList xs
  in x' : xs'

addApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
-- addMaybe a b = (+) <$> a <*> b
-- addMaybe = lift2 (+)
addApply = apply <<< map (+)

mulApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = apply <<< map (*)

subApply :: ∀ f a. Apply f => Ring a => f a -> f a -> f a
subApply = apply <<< map (-)

divApply :: ∀ f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = apply <<< map (/)

addMaybe ∷ ∀ f a. Apply f => Semiring a => f a -> f a -> f a
addMaybe = addApply

mulMaybe ∷ ∀ f a. Apply f => Semiring a => f a -> f a -> f a
mulMaybe = mulApply

subMaybe ∷ ∀ f a. Apply f => Ring a => f a -> f a -> f a
subMaybe = subApply

divMaybe ∷ ∀ f a. Apply f => EuclideanRing a => f a -> f a -> f a
divMaybe = divApply

combineMaybe :: ∀ f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = pure <$> x
combineMaybe _ = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-z]{2}$" ignoreCase

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags

validateAddressImproved' :: Address -> V Errors Address
validateAddressImproved' { street, city, state } = ado
  street' <- matches "Street" nonEmptyRegex street
  city' <- matches "City" nonEmptyRegex city
  state' <- matches "State" stateRegex state
  in address street' city' state'

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved { street, city, state } = address
  <$> matches "Street" nonEmptyRegex street
  <*> matches "City" nonEmptyRegex city
  <*> matches "State" stateRegex state

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show t = genericShow t

derive instance Eq a => Eq (Tree a)

derive instance Functor Tree
derive instance Foldable Tree

instance Apply Tree where
  apply Leaf _ = Leaf
  apply _ Leaf = Leaf
  apply (Branch _ f _) (Branch _ x _) = Branch Leaf (f x) Leaf

instance Applicative Tree where
  pure a = Branch Leaf a Leaf

instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l m r) = ado
    l' <- traverse f l
    m' <- f m
    r' <- traverse f r
    in Branch l' m' r'
  sequence Leaf = pure Leaf
  sequence t = traverse identity t

-- root - left - right
traversePreOrder
  :: ∀ a m b
   . Applicative m
  => (a -> m b)
  -> Tree a
  -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch left root right) = ado
  root' <- f root
  left' <- f `traversePreOrder` left
  right' <- f `traversePreOrder` right
  in Branch left' root' right'

traversePostOrder
  :: ∀ a m b
   . Applicative m
  => (a -> m b)
  -> Tree a
  -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch left root right) = ado
  left' <- f `traversePostOrder` left
  right' <- f `traversePostOrder` right
  root' <- f root
  in Branch left' root' right'

type Person =
  { firstName :: String
  , lastName :: String
  , homeAddress :: Maybe Address
  , phones :: Array PhoneNumber
  }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress { firstName, lastName, homeAddress, phones } = ado
  f <- nonEmpty "First name" firstName
  l <- nonEmpty "Last name" lastName
  a <- validateAddress `traverse` homeAddress
  p <- validatePhoneNumber `traverse` phones
  in { firstName: f, lastName: l, homeAddress: a, phones: p }

sequenceUsingTraverse
  :: ∀ f a m
   . Traversable f
  => Applicative m
  => f (m a)
  -> m (f a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence
  :: ∀ f a m b
   . Traversable f
  => Applicative m
  => (a -> m b)
  -> f a
  -> m (f b)
traverseUsingSequence f xs = sequence (f <$> xs)

main :: Effect Unit
main = do
  logShow $ sequence $ Just 1 : Just 2 : Just 3 : Nil
