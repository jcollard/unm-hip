{-# LANGUAGE 
   TypeFamilies, 
   FlexibleContexts, 
   FlexibleInstances, 
   ViewPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Imageable(Imageable(..), 
                            PixelOp,
                            Zero(..),
                            MaxMin(..),
                            RGB(..),
                            Divisible(..),
                            Kernel,
                            Kernel2D,
                            imageOp, 
                            dimensions, 
                            maxIntensity,
                            minIntensity,
                            leftToRight,
                            leftToRight',
                            topToBottom,
                            topToBottom',
                            downsampleRows,
                            downsampleCols,
                            downsample,
                            upsampleRows,
                            upsampleCols,
                            upsample,
                            pad,
                            crop,
                            kernel,
                            kernel2d,
                            convolveRows,
                            convolveCols,
                            convolve,
                            normalize,
                            imageFold,
                            arrayToImage,
                            imageToArray) where

import Data.Array.IArray

type PixelOp px = (Int -> Int -> px)

class Imageable i where
  type Pixel i :: *
  makeImage :: Int -> Int -> PixelOp (Pixel i) -> i
  ref  :: i -> Int -> Int -> (Pixel i)
  rows :: i -> Int
  cols :: i -> Int
  pixelList :: i -> [Pixel i]
  pixelList i = [ ref i r c | r <- [0..(rows i - 1)], c <- [0..(cols i - 1)]]
  
class RGB px where
  rgb :: px -> (Double, Double, Double)

class Divisible d where
  divide :: (Integral a) => a -> d -> d 

instance Divisible Double where
  divide f d = fromIntegral f / d

class MaxMin m where
  maximal :: [m] -> m
  minimal :: [m] -> m

instance MaxMin Double where
  maximal = maximum
  minimal = minimum
  
class Zero z where
  zero :: z

instance Zero Double where
  zero = 0.0

imageOp :: (Imageable img) => 
           (Pixel img -> Pixel img -> Pixel img) -> img -> img -> img
imageOp op i0 i1 = makeImage (rows i0) (cols i0) operate where
  operate r c = op (ref i0 r c) (ref i1 r c)

dimensions :: Imageable i => i -> (Int, Int)
dimensions i = (rows i, cols i)

downsampleRows :: (Imageable img) => img -> img
downsampleRows img@(dimensions -> (rows, cols)) = makeImage (rows `div` 2) cols downsample where
  downsample r c = ref img (r*2) c
  
downsampleCols :: (Imageable img) => img -> img
downsampleCols img@(dimensions -> (rows, cols)) = makeImage rows (cols `div` 2) downsample where
  downsample r c = ref img r (c*2)
  
downsample :: (Imageable img) => img -> img
downsample = downsampleRows . downsampleCols

upsampleRows :: (Imageable img, Zero (Pixel img)) => img -> img
upsampleRows img@(dimensions -> (rows, cols)) = makeImage (rows*2) cols upsample where
  upsample r c 
    | even r = ref img (r `div` 2) c
    | otherwise = zero

upsampleCols :: (Imageable img, Zero (Pixel img)) => img -> img
upsampleCols img@(dimensions -> (rows, cols)) = makeImage rows (cols*2) upsample where
  upsample r c 
    | even r = ref img r (c `div` 2)
    | otherwise = zero

upsample :: (Imageable img, Zero (Pixel img)) => img -> img
upsample img@(dimensions -> (rows, cols)) = makeImage (rows*2) (cols*2) upsample where
  upsample r c
    | even r && even c = ref img (r `div` 2) (c `div` 2)
    | otherwise = zero
      
pad :: (Imageable img, Zero (Pixel img)) => Int -> Int -> img -> img    
pad rs cs img@(dimensions -> (rows, cols)) = makeImage rs cs pad where
  pad r c 
    | r < rows && c < cols = ref img r c
    | otherwise = zero
  
crop :: (Imageable img) => Int -> Int -> Int -> Int -> img -> img
crop r0 c0 w h img = makeImage w h crop where
  crop r c = ref img (r+r0) (c+c0)
                  
maxIntensity :: (Imageable img, MaxMin (Pixel img)) => img -> Pixel img
maxIntensity = maximal . pixelList

minIntensity :: (Imageable img, MaxMin (Pixel img)) => img -> Pixel img
minIntensity = minimal . pixelList

leftToRight :: (Imageable img) => img -> img -> img
leftToRight i0 i1 = makeImage (rows i0) cols' concat where
  cols' = sum . map cols $ [i0, i1]
  concat r c 
    | c < (cols i0) = ref i0 r c
    | otherwise = ref i1 r (c - (cols i0))
                  
leftToRight' :: (Imageable img) => [img] -> img
leftToRight' = foldr1 leftToRight

topToBottom :: (Imageable img) => img -> img -> img
topToBottom i0 i1 = makeImage rows' (cols i0) concat where
  rows' = sum . map rows $ [i0, i1]
  concat r c
    | r < (rows i0) = ref i0 r c
    | otherwise = ref i1 (r - (rows i0)) c

topToBottom' :: (Imageable img) => [img] -> img
topToBottom' = foldr1 topToBottom

type Kernel a = Array Int a

kernel :: [a] -> Kernel a
kernel ls = listArray (0, (length ls) - 1) (reverse ls)

type Kernel2D a = Array (Int, Int) a
kernel2d :: [[a]] -> Kernel2D a
kernel2d ls = listArray ((0,0), (length ls-1, length (head ls) - 1)) (reverse . concat $ ls)
  
convolveRows :: (Imageable img,
                 Num (Pixel img)) => [Pixel img] -> img -> img
convolveRows = convolveRows' . kernel

convolveRows' :: (Imageable img, 
                 Num (Pixel img)) => Kernel (Pixel img) -> img -> img
convolveRows' k = convolve' k' where
  k' = listArray ((0,0), (0,cols-1)) ls where
    ls = elems k
    cols = length ls
    
convolveCols :: (Imageable img,
                 Num (Pixel img)) => [Pixel img] -> img -> img
convolveCols = convolveCols' . kernel

convolveCols' :: (Imageable img, 
                 Num (Pixel img)) => Kernel (Pixel img) -> img -> img
convolveCols' k = convolve' k' where
  k' = listArray ((0,0), (rows-1,0)) ls where
    ls = elems k
    rows = length ls

convolve :: (Imageable img,
             Num (Pixel img)) => [[Pixel img]] -> img -> img
convolve = convolve' . kernel2d

convolve' :: (Imageable img,
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

periodRef :: (Imageable img) => img -> Int -> Int -> (Pixel img)
periodRef img@(dimensions -> (rows, cols)) r c = ref img (r `mod` rows) (c `mod` cols)

transpose :: (Imageable img) => img -> img
transpose img@(dimensions -> (rows, cols)) = makeImage rows cols trans where
  trans r c = ref img c r

normalize :: (Imageable img,
              Divisible (Pixel img),
              MaxMin (Pixel img),
              Num (Pixel img)) => img -> img
normalize img@(dimensions -> (rows, cols)) = makeImage rows cols map where
  map r c = ((ref img r c) - min)*scale
  (min, max) = (minimal px, maximal px)
  scale = 1 `divide` (max - min)
  px = pixelList img

imageFold :: Imageable img => (Pixel img -> b -> b) -> b -> img -> b
imageFold f init img = foldr f init (pixelList img)

arrayToImage :: (Imageable img) => Array (Int, Int) (Pixel img) -> img
arrayToImage arr = makeImage rows cols ref where
  ((rmin,cmin), (rmax, cmax)) = bounds arr
  rows = rmax - rmin + 1
  cols = cmax - cmin + 1
  ref r c = arr ! (r, c)
  
imageToArray :: (Imageable img) => img -> Array (Int, Int) (Pixel img)
imageToArray img@(dimensions -> (rows, cols)) = listArray bounds elems where
  bounds = ((0,0), (rows-1,cols-1))
  elems = pixelList img
  