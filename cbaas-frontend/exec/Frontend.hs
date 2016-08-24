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
  text "HelloNew"
  d <- lift askDocument
  imageInputWidget d def
  -- webcamWidget d (constDyn mempty)
  -- loader <- fileImageLoader
  -- displayImg =<< holdDyn (T.pack "") (fmap snd loader)
  blank


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing
