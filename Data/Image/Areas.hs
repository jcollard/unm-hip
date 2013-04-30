{-# LANGUAGE ViewPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.Areas(areas,
                        label,
                        perimeters,
                        boundingBoxes,
                        centersOfMass) where

import Control.Monad
import Control.Monad.ST


import Data.Image.Internal
import qualified Data.Map as M
import Data.STRef

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

perimeters :: (Image img,
               MaxMin (Pixel img),
               Pixel img ~ Double) => img -> V.Vector Double
perimeters img@(dimensions -> (rows, cols)) = runST $ do
  vector <- VM.replicate ((floor $ maxIntensity img)+1) 0 :: ST s (VM.STVector s Double)
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) -> do
   let ns = filter (== 0) . neighborList img r $ c
   if null ns then return ()
     else do
       let (floor -> group) = ref img r c
       currPerimeter <- VM.read vector group
       VM.write vector group (currPerimeter+1)
   )
  VM.write vector 0 0
  vals <- V.freeze vector
  VM.write vector 0 (V.sum vals)
  V.freeze vector
  
neighborList :: (Image img) => img -> Int -> Int -> [Pixel img]
neighborList img@(dimensions -> (rows, cols)) r c = 
  [ ref img r' c' | r' <- [r-1..r+1], c' <- [c-1..c+1], 
    r' /= r && c' /= c, r' >= 0, r' < rows, c' >= 0, c' < cols]

boundingBoxes :: (Image img,
                  MaxMin (Pixel img),
                  Pixel img ~ Double) => img -> [(Int, Int, Int, Int)]
boundingBoxes img@(dimensions -> (rows, cols)) = runST $ do 
  let n = floor . maxIntensity $ img
  boxes <- VM.new (n*4) :: ST s (VM.STVector s Int)
  
  -- Initialize boxes to be the entire image
  forM_ [0..n-1] (\ n -> do 
    VM.write boxes (n*4) rows
    VM.write boxes (n*4 + 1) cols
    VM.write boxes (n*4 + 2) 0
    VM.write boxes (n*4 + 3) 0)
    
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) ->
    let (floor -> px) = ref img r c in updateAt px r c boxes)
  
  boxes' <- V.freeze boxes
  
  return . toQuads . V.toList $ boxes'
  
updateAt :: Int -> Int -> Int -> VM.STVector s Int -> ST s ()
updateAt ((flip (-) 1) -> group) r c boxes 
  | group < 0 = return ()
  | otherwise = do
  let read = VM.read boxes
      write = VM.write boxes
  minR <- read (group*4)
  minC <- read (group*4 + 1)
  maxR <- read (group*4 + 2)
  maxC <- read (group*4 + 3)
  let minR' = min minR r
      minC' = min minC c
      maxR' = max maxR r
      maxC' = max maxC c
  write (group*4) minR'
  write (group*4 + 1) minC'
  write (group*4 + 2) maxR'
  write (group*4 + 3) maxC'
  
toQuads :: [a] -> [(a,a,a,a)]
toQuads = toQuads' [] where
  toQuads' acc [] = reverse acc
  toQuads' acc (a:b:c:d:xs) = toQuads' ((a,b,c,d):acc) xs
  
centersOfMass :: (Image img,
                  MaxMin (Pixel img),
                  Pixel img ~ Double) => img -> [(Double, Double)]
centersOfMass img@(dimensions -> (rows, cols)) = runST $ do
  let n = floor . maxIntensity $ img
  centers <- VM.replicate n (0,0,0) :: ST s (VM.STVector s (Int, Int, Int))
  
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) ->
    let (floor -> px) = ref img r c in updateMass px r c centers)
  
  centers' <- V.freeze centers
  
  return . map averageMass . V.toList $ centers'
  
updateMass :: Int -> Int -> Int -> VM.STVector s (Int, Int, Int) -> ST s ()
updateMass ((flip (-) 1) -> group) r c centers
  | group < 0 = return ()
  | otherwise = do
    (pixels, totalRow, totalCol) <- VM.read centers group
    VM.write centers group (pixels+1, totalRow+r, totalCol+c)
    
averageMass :: (Int, Int, Int) -> (Double, Double)
averageMass ((fromIntegral -> total), (fromIntegral -> rows), (fromIntegral -> cols)) = (rows/total, cols/total)