{-# LANGUAGE TypeFamilies, ViewPatterns #-}
{-# OPTIONS -O2 #-}
module Data.Image.MedianFilter(medianFilter) where

import Data.Image.Internal

medianFilter :: (Image img,
                 Pixel img ~ Double) => Int -> Int -> img -> img
medianFilter m n img@(dimensions -> (rows, cols)) = makeImage rows cols avg where
  [moff, noff] = map (`div` 2) [m,n]
  avg r c = (sum px)/(fromIntegral . length $ px) where
    px = [ ref img i j | 
                   i <- [rm..rm+m], 
                   j <- [cm..cm+n], 
                   i >= 0, i < rows, j >= 0, j < cols]
    rm = r - moff
    cm = c - noff
