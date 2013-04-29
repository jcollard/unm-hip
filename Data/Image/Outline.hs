{-# LANGUAGE ViewPatterns, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.Outline(outline) where

import Control.Monad
import Control.Monad.ST

import Data.Image.Internal

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

outline :: (Image img,
            Pixel img ~ Double) => img -> img
outline img = outline' img 0.0 1.0

-- Outline support code
outline' :: (Image img,
             Pixel img ~ Double) => img -> Double -> Double -> img
outline' img@(dimensions -> (rows, cols)) nonEdge edge = makeImage rows cols func
  where arr = getOutlineArray img edge nonEdge
        func r c = arr V.! ((cols*r)+c)

getOutlineArray :: (Image img,
                    Pixel img ~ Double) => img -> Double -> Double -> V.Vector Double
getOutlineArray img@(dimensions -> (rows, cols)) edge nonEdge = runST $ do
  let data1 = V.fromList (pixelList img) :: V.Vector Double
  data4 <- VM.replicate (rows*cols) 0 :: ST s (VM.STVector s Double)
  
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [0..cols-1] $ \ j -> do
      VM.write data4 (index0+j) nonEdge

  forM [0..rows-2] $ \ i -> do
    let index0 = i*cols
    let index1 = (i+1)*cols
    forM [0..cols-1] $ \ j -> do
      let val = (data1 V.! (index0+j)) + (data1 V.! (index1+j))
      if (val == 1) 
        then 
          VM.write data4 (index0+j) edge
        else return ()

  let index0 = (rows-1)*cols
  forM [0..cols-1] $ \ j -> do
    let val = (data1 V.! (index0+j)) + (data1 V.! j)
    if (val == 1)
      then VM.write data4 (index0+j) edge
      else return ()

  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [1..cols-2] $ \ j -> do
      let val = (data1 V.! (index0+j)) + (data1 V.! (index0 + j + 1))
      if (val == 1)
        then VM.write data4 (index0+j) edge
        else return ()
   
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    let val = (data1 V.! (index0+cols-1)) + (data1 V.! index0)
    if (val == 1)
      then VM.write data4 (index0+cols-1) edge
      else return ()  
   
  V.freeze data4
-- End outline support code
