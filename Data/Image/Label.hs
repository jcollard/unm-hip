{-# LANGUAGE TypeFamilies, ViewPatterns #-}
{-# OPTIONS -O2 #-}
module Data.Image.Label(label) where

import Control.Monad
import Control.Monad.ST
import Data.Image.Internal
import qualified Data.Map as M
import Data.STRef

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

label :: (Image img,
          Pixel img ~ Double) => img -> img
label img@(dimensions -> (rows, cols)) = makeImage rows cols labels where
  labels r c = arr V.! (r*cols+c)
  arr = getLabels img :: V.Vector Double
  
getLabels img@(dimensions -> (rows, cols)) = runST $ do 
  currentLabel <- newSTRef 1 :: ST s (STRef s Int)
  equivalences <- newSTRef M.empty :: ST s (STRef s (M.Map Double Double))
  labels <- VM.replicate (rows*cols) 0 :: ST s (VM.STVector s Double)
  
  --Label Groups
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1] ] (\ (r, c) -> do
    if (ref img r c) > 0 then do
      smn <- neighbors labels rows cols r c
      if (smn == []) then newLabel labels currentLabel cols r c equivalences
        else writeLabel labels cols r c smn equivalences
      else return ())
  
  --Relabel with Lowest Equivalence
  eq <- readSTRef equivalences
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1] ] (\ (r, c) -> do
    if (ref img r c) > 0 then do
      currLabel <- VM.read labels (r*cols + c)
      let newLabel = (eq M.! currLabel)
      VM.write labels (r*cols + c) newLabel
      else return ())
  V.freeze labels

writeLabel labels cols r c smn equiv = do
  oldMap <- readSTRef equiv
  let min = minimum smn
      insert k m = M.insert k min m
      newMap = foldr insert oldMap smn
  VM.write labels (r*cols + c) min
  writeSTRef equiv newMap

newLabel labels currentLabel cols r c equivalences = do
  l <- readSTRef currentLabel
  modifySTRef currentLabel (+1)
  VM.write labels (cols*r + c) (fromIntegral l)
  eq <- readSTRef equivalences
  let newMap = M.insert (fromIntegral l) (fromIntegral l) eq
  writeSTRef equivalences newMap

neighbors :: VM.STVector s Double -> Int -> Int -> Int -> Int -> ST s [Double]
neighbors labels rows cols r c = do
  let n' = neighbor labels rows cols
  center <- n' r c
  north <- n' (r-1) c
  ne <- n' (r-1) (c+1)
  east <- n' r (c+1)
  se <- n' (r+1) (c+1)
  south <- n' (r+1) c
  sw <- n' (r+1) (c-1)
  west <- n' r (c-1)
  nw <- n' (r-1) (c-1)
  return $ filter (> 0) [center, north, ne, east, se, south, sw, west, nw]

neighbor :: VM.STVector s Double -> Int -> Int -> Int -> Int -> ST s Double
neighbor labels rows cols r c
  | r >= 0 && r < rows && c >= 0 && c < cols = VM.read labels (r*cols + c)
  | otherwise = return 0.0
