module Main where

import Control.Lens
import Codec.Picture
import Codec.Picture.Types
import Model
import WorkerClient

main :: IO ()
main = runWorker $ \x -> do
  -- print x
  let r = compute x
  print r
  return r

compute :: DynamicImage -> Int
compute i = dynamicMap imageWidth i * dynamicMap imageHeight i
