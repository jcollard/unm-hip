{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-# OPTIONS -O2 #-}

module Data.Image.Areas(areas) where

import Control.Monad
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.ST

import Data.Image.Imageable

areas :: (Imageable img,
         MaxMin (Pixel img),
         RealFrac (Pixel img)) => img -> Array Int Double
areas img@(dimensions -> (rows, cols)) = runST $ do 
  histogram <- newArray (0, (floor $ maxIntensity img)) 0 :: ST s (STUArray s Int Double)
  forM_ (pixelList img) (\ (floor -> p) -> do
                            currVal <- readArray histogram p
                            writeArray histogram p (currVal + 1))
  freeze histogram
