module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonParser, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Test.Examples (Complex, Quadratic, Undefined)

-- Note to reader: Add your solutions to this file

foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl
  :: (forall a. a -> a -> Pair a)
  -> Quadratic
  -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

foreign import toMaybeImpl
  :: forall a
   . (forall x. x -> Maybe x)
  -> (forall x. Maybe x)
  -> Undefined a
  -> Maybe a

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe = toMaybeImpl Just Nothing

foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapJson <<< encodeJson

valuesOfMapGeneric
  :: forall k v
   . Ord k
  => Ord v
  => EncodeJson k
  => EncodeJson v
  => DecodeJson v
  => Map k v
  -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = decodeJson <<< valuesOfMapJson <<< encodeJson

foreign import quadraticRootsJson :: Quadratic -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = decodeJson <<< quadraticRootsJson

newtype MyPair a = MyPair (Pair a)

instance DecodeJson a => DecodeJson (MyPair a) where
  decodeJson json = decodeJson json >>= case _ of
    [ a, b ] -> Pair <$> decodeJson a <*> decodeJson b <#> MyPair
    _ -> Left $ TypeMismatch "MyPair"

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = map (\(MyPair p) -> p) <<< decodeJson <<< quadraticRootsJson

parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D = jsonParser >=> decodeJson >>> lmap printJsonDecodeError

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance Generic (Tree a) _

derive instance Eq a => Eq (Tree a)
instance Show a => Show (Tree a) where
  show t = genericShow t

instance EncodeJson a => EncodeJson (Tree a) where
  encodeJson t = genericEncodeJson t

instance DecodeJson a => DecodeJson (Tree a) where
  decodeJson t = genericDecodeJson t

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance Generic IntOrString _

derive instance Eq IntOrString

instance Show IntOrString where
  show = genericShow

instance EncodeJson IntOrString where
  encodeJson (IntOrString_Int v) = encodeJson v
  encodeJson (IntOrString_String v) = encodeJson v

instance DecodeJson IntOrString where
  decodeJson x =
    (Left $ TypeMismatch "Neither String nor Int")
      <|> IntOrString_Int <$> decodeJson x
      <|> IntOrString_String <$> decodeJson x

main :: Effect Unit
main = do
  logShow $ quadraticRoots { a: 4.0, b: 0.0, c: 16.0 }
  logShow $ (Right (Pair 1 1) :: Either Void _) >>= (\(Pair n m) -> n + m) >>> pure
  logShow $ Left 1 <|> Left 2 <|> Right 3
