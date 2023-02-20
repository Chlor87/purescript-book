module Test.MySolutions where

import Prelude

import Affjax.Node (get)
import Affjax.ResponseFormat (string)
import Control.Parallel (parOneOf, parTraverse)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (fold, sequence)
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, launchAff_)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, concat, dirname)

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

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout n url = parOneOf
  [ get string url >>= map _.body >>> hush >>> pure
  , delay (Milliseconds n) $> Nothing
  ]

recurseFiles :: FilePath -> Aff (Array String)
recurseFiles root = do
  files <- pure <<< split (Pattern "\n") =<< readTextFile UTF8 root
  case files of
    [ "" ] -> pure [ root ]
    _ -> do
      let
        dir = dirname root
        files' = (\x -> concat [ dir, x ]) <$> files
      next <- pure <<< fold =<< recurseFiles `parTraverse` files'
      pure $ [ root ] <> next

main :: Effect Unit
main = launchAff_ do
  logShow $ sequence $ Just <$> [ 1, 2, 3 ]
  logShow $ hush <<< map _.body $ Right { body: "test" }
  logShow $ hush <<< map _.body $ (Left { body: "test" } :: Either { body :: String } { body :: String })
