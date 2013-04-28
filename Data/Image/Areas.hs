{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-# OPTIONS -O2 #-}

module Data.Image.Areas(area) where

import Control.Monad
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.ST

import Data.Image.Imageable

area :: (Imageable img,
         MaxMin (Pixel img),
         RealFrac (Pixel img)) => img -> Array Int Double
area img@(dimensions -> (rows, cols)) = runST $ do 
  histogram <- newArray (0, (floor $ maxIntensity img)) 0 :: ST s (STUArray s Int Double)
  forM_ (pixelList img) (\ (floor -> p) -> do
                            currVal <- readArray histogram p
                            writeArray histogram p (currVal + 1))
  freeze histogram
