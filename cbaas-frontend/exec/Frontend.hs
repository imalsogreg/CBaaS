module Main where

import qualified Data.Text as T
import Data.Either
import Reflex.Dom
import Frontend.Function
import Frontend.ImageWidget
import Frontend.WebcamWidget

main :: IO ()
main = mainWidget $ do
  text "Hello"
  webcamWidget
  -- loader <- fileImageLoader
  -- displayImg =<< holdDyn (T.pack "") (fmap snd loader)


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing
