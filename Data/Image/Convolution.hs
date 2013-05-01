{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-# OPTIONS -O2 #-}
module Data.Image.Convolution(convolveRows,
                              convolveCols,
                              convolve) where

import Data.Array.IArray

import Data.Image.Internal

type Kernel a = Array Int a
type Kernel2D a = Array (Int, Int) a

kernel :: [a] -> Kernel a
kernel ls = listArray (0, (length ls) - 1) (reverse ls)

kernel2d :: [[a]] -> Kernel2D a
kernel2d ls = listArray ((0,0), (length ls-1, length (head ls) - 1)) (reverse . concat $ ls)
  
{-| Given a list consisting solely of pixel values representing a 1D 
    convolution kernel and an image, convolveRows returns the 1D discrete 
    periodic convolution of the rows of the image with the kernel.
 -}
convolveRows :: (Image img,
                 Num (Pixel img)) => [Pixel img] -> img -> img
convolveRows = convolveRows' . kernel

convolveRows' :: (Image img, 
                 Num (Pixel img)) => Kernel (Pixel img) -> img -> img
convolveRows' k = convolve' k' where
  k' = listArray ((0,0), (0,cols-1)) ls where
    ls = elems k
    cols = length ls
    
{-| Given a list consisting solely of pixel values representing a 1D 
    convolution kernel and an image, convolveCols returns the 1D discrete 
    periodic convolution of the columns of the image with the kernel.
 -}    
convolveCols :: (Image img,
                 Num (Pixel img)) => [Pixel img] -> img -> img
convolveCols = convolveCols' . kernel

convolveCols' :: (Image img, 
                 Num (Pixel img)) => Kernel (Pixel img) -> img -> img
convolveCols' k = convolve' k' where
  k' = listArray ((0,0), (rows-1,0)) ls where
    ls = elems k
    rows = length ls

{-| Given a 2D list consisting solely of pixels representing a 2D 
    convolution kernel and an image, convolve returns the 2D discrete 
    periodic convolution of the image with the kernel.
 -}
convolve :: (Image img,
             Num (Pixel img)) => [[Pixel img]] -> img -> img
convolve = convolve' . kernel2d

convolve' :: (Image img,
             Num (Pixel img)) => Kernel2D (Pixel img) -> img -> img            
convolve' k img@(dimensions -> (rows, cols)) = makeImage rows cols conv where
  conv r c = px where
    imgVal = map (uncurry (periodRef img) . (\ (r', c') -> (r+r', c+c'))) imgIx
    imgIx = map (\ (r, c) -> (r - cR, c - cC)) . indices $ k
    kVal = elems k
    px = sum . map (\ (p,k') -> p*k') . zip imgVal $ kVal
  ((minR, minC), (maxR, maxC)) = bounds k
  cR = (maxR - minR) `div` 2
  cC = (maxC - minC) `div` 2
  recenter = map (\ (r, c) -> ((r-cR), (c-cC))) . indices $ k

periodRef :: (Image img) => img -> Int -> Int -> (Pixel img)
periodRef img@(dimensions -> (rows, cols)) r c = ref img (r `mod` rows) (c `mod` cols)
