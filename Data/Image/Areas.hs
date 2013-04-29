{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-# OPTIONS -O2 #-}

module Data.Image.Areas(areas) where

import Control.Monad
import Control.Monad.ST

import Data.Image.Internal

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

areas :: (Image img,
         MaxMin (Pixel img),
         RealFrac (Pixel img)) => img -> V.Vector Double
areas img@(dimensions -> (rows, cols)) = runST $ do 
  histogram <- VM.replicate ((floor $ maxIntensity img)+1) 0 :: ST s (VM.STVector s Double)
  forM_ (pixelList img) (\ (floor -> p) -> do
                            currVal <- VM.read histogram p
                            VM.write histogram p (currVal + 1))
  V.freeze histogram
