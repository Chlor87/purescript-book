module Test.MySolutions where

import Prelude

import Affjax.Node (get)
import Affjax.ResponseFormat (string)
import Control.Parallel (parOneOf, parTraverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (length)
import Data.Traversable (fold)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, delay, launchAff_)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

-- Note to reader: Add your solutions to this file

concatenateFiles :: String -> String -> String -> Aff Unit
concatenateFiles a b c = do
  a' <- readTextFile UTF8 a
  b' <- readTextFile UTF8 b
  writeTextFile UTF8 c $ a' <> b'

concatenateMany :: Array String -> String -> Aff Unit
concatenateMany srcs dst =
  readTextFile UTF8 `parTraverse` srcs >>= fold >>> writeTextFile UTF8 dst

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters src = attempt do
  f <- readTextFile UTF8 src
  pure $ length f

writeGet :: String -> FilePath -> Aff Unit
writeGet url file = do
  res <- get string url
  case res of
    Right { body } -> writeTextFile UTF8 file body
    Left _ -> pure unit

concatenateManyParallel ∷ Array String → String → Aff Unit
concatenateManyParallel = concatenateMany

-- getWithTimeout :: Number -> String -> Aff (Maybe String)
-- getWithTimeout n url = do
--   res <- parOneOf [ delay n, get string url ]
--   pure $ pure unit

main :: Effect Unit
main = launchAff_ do
  pure unit
