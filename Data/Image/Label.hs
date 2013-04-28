{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Data.Image.Label(label) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Unboxed
import Data.Array.ST

import Data.Image.Imageable

import Data.List

import qualified Data.Map as M

import Data.STRef

label :: (Imageable img,
          Pixel img ~ Double) => img -> img
label img@(dimensions -> (rows, cols)) = makeImage rows cols labels where
  labels r c = arr ! (r*cols+c)
  arr = getLabels img :: UArray Int Double
  
getLabels img@(dimensions -> (rows, cols)) = runSTUArray $ do 
  currentLabel <- newSTRef 1 :: ST s (STRef s Int)
  equivalences <- newSTRef M.empty :: ST s (STRef s (M.Map Double Double))
  labels <- newArray (0, (rows*cols-1)) 0 :: ST s (STUArray s Int Double)
  
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
      currLabel <- readArray labels (r*cols + c)
      let newLabel = (eq M.! currLabel)
      writeArray labels (r*cols + c) newLabel
      else return ())
  return labels

writeLabel labels cols r c smn equiv = do
  oldMap <- readSTRef equiv
  let min = minimum smn
      insert k m = M.insert k min m
      newMap = foldr insert oldMap smn
  writeArray labels (r*cols + c) min
  writeSTRef equiv newMap

newLabel labels currentLabel cols r c equivalences = do
  l <- readSTRef currentLabel
  modifySTRef currentLabel (+1)
  writeArray labels (cols*r + c) (fromIntegral l)
  eq <- readSTRef equivalences
  let newMap = M.insert (fromIntegral l) (fromIntegral l) eq
  writeSTRef equivalences newMap

neighbors :: STUArray s Int Double -> Int -> Int -> Int -> Int -> ST s [Double]
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

neighbor :: STUArray s Int Double -> Int -> Int -> Int -> Int -> ST s Double
neighbor labels rows cols r c
  | r >= 0 && r < rows && c >= 0 && c < cols = readArray labels (r*cols + c)
  | otherwise = return 0.0
