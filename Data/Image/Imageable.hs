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
                            normalize,
                            imageFold,
                            arrayToImage,
                            imageToArray,
                            imageMap) where

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

transpose :: (Imageable img) => img -> img
transpose img@(dimensions -> (rows, cols)) = makeImage rows cols trans where
  trans r c = ref img c r

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
    
imageMap :: (Imageable a, Imageable b) => (Pixel a -> Pixel b) -> a -> b
imageMap f img@(dimensions -> (rows, cols)) = makeImage rows cols map where
  map r c = f (ref img r c)
