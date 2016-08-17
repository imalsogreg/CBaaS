{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans (lift)
import qualified Data.Text as T
import Data.Either
import Reflex.Dom
import Frontend.Function
import Frontend.ImageWidget
import Frontend.WebcamWidget

main :: IO ()
main = mainWidget $ do
  text "Hello"
  d <- lift askDocument
  webcamWidget d
  -- loader <- fileImageLoader
  -- displayImg =<< holdDyn (T.pack "") (fmap snd loader)


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing
