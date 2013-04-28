{-# LANGUAGE ViewPatterns, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.Outline(outline) where

import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.Array.Unboxed

import Data.Image.Imageable

outline :: (Imageable img,
            Pixel img ~ Double) => img -> img
outline img = outline' img 0.0 1.0

-- Outline support code
outline' :: (Imageable img,
             Pixel img ~ Double) => img -> Double -> Double -> img
outline' img@(dimensions -> (rows, cols)) nonEdge edge = makeImage rows cols func
  where arr = getOutlineArray img edge nonEdge
        func r c = arr ! ((cols*r)+c)

getOutlineArray :: (Imageable img,
                    Pixel img ~ Double) => img -> Double -> Double -> UArray Int Double
getOutlineArray img@(dimensions -> (rows, cols)) edge nonEdge = runSTUArray $ do
  let data1 = listArray (0, (rows*cols-1)) (pixelList img) :: Array Int Double
  data4 <- newArray_ (0, rows*cols-1) :: ST s (STUArray s Int Double)
  
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [0..cols-1] $ \ j -> do
      writeArray data4 (index0+j) nonEdge

  forM [0..rows-2] $ \ i -> do
    let index0 = i*cols
    let index1 = (i+1)*cols
    forM [0..cols-1] $ \ j -> do
      let val = (data1 ! (index0+j)) + (data1 ! (index1+j))
      if (val == 1) 
        then 
          writeArray data4 (index0+j) edge
        else return ()

  let index0 = (rows-1)*cols
  forM [0..cols-1] $ \ j -> do
    let val = (data1 ! (index0+j)) + (data1 ! j)
    if (val == 1)
      then writeArray data4 (index0+j) edge
      else return ()

  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [1..cols-2] $ \ j -> do
      let val = (data1 ! (index0+j)) + (data1 ! (index0 + j + 1))
      if (val == 1)
        then writeArray data4 (index0+j) edge
        else return ()
   
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    let val = (data1 ! (index0+cols-1)) + (data1 ! index0)
    if (val == 1)
      then writeArray data4 (index0+cols-1) edge
      else return ()  
   
  return data4
-- End outline support code
