{-# LANGUAGE 
   TypeFamilies, 
   FlexibleContexts, 
   FlexibleInstances, 
   ViewPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Internal(Image(..), 
                           PixelOp,
                           MaxMin(..),
                           GrayPixel(..),
                           RGBPixel(..),
                           HSIPixel(..),
                           Scaleable(..),
                           Listable(..),
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
import Data.Monoid

type PixelOp px = (Int -> Int -> px)

class Image i where
  type Pixel i :: *
  makeImage :: Int -> Int -> PixelOp (Pixel i) -> i
  ref  :: i -> Int -> Int -> (Pixel i)
  rows :: i -> Int
  cols :: i -> Int
  pixelList :: i -> [Pixel i]
  pixelList i = [ ref i r c | r <- [0..(rows i - 1)], c <- [0..(cols i - 1)]]
  
class Listable a where
  type Elem a :: *
  toList :: a -> [Elem a]
  
instance Listable [a] where
  type Elem [a] = a
  toList = id

instance Listable (a,a) where
  type Elem (a,a) = a
  toList (a,b) = [a,b]
  
instance Listable (a,a,a) where
  type Elem (a,a,a) = a
  toList (a,b,c) = [a,b,c]
  
class GrayPixel px where
  toGray :: px -> Double

class RGBPixel px where
  toRGB :: px -> (Double, Double, Double)

class HSIPixel px where
  toHSI :: px -> (Double, Double, Double)

class Scaleable d where
  divide :: Double -> d -> Double
  mult   :: Double -> d -> d

class MaxMin m where
  maximal :: [m] -> m
  minimal :: [m] -> m

transpose :: (Image img) => img -> img
transpose img@(dimensions -> (rows, cols)) = makeImage rows cols trans where
  trans r c = ref img c r

dimensions :: Image i => i -> (Int, Int)
dimensions i = (rows i, cols i)

downsampleRows :: (Image img) => img -> img
downsampleRows img@(dimensions -> (rows, cols)) = makeImage (rows `div` 2) cols downsample where
  downsample r c = ref img (r*2) c
  
downsampleCols :: (Image img) => img -> img
downsampleCols img@(dimensions -> (rows, cols)) = makeImage rows (cols `div` 2) downsample where
  downsample r c = ref img r (c*2)
  
downsample :: (Image img) => img -> img
downsample = downsampleRows . downsampleCols

upsampleRows :: (Image img, Monoid (Pixel img)) => img -> img
upsampleRows img@(dimensions -> (rows, cols)) = makeImage (rows*2) cols upsample where
  upsample r c 
    | even r = ref img (r `div` 2) c
    | otherwise = mempty

upsampleCols :: (Image img, Monoid (Pixel img)) => img -> img
upsampleCols img@(dimensions -> (rows, cols)) = makeImage rows (cols*2) upsample where
  upsample r c 
    | even r = ref img r (c `div` 2)
    | otherwise = mempty

upsample :: (Image img, Monoid (Pixel img)) => img -> img
upsample img@(dimensions -> (rows, cols)) = makeImage (rows*2) (cols*2) upsample where
  upsample r c
    | even r && even c = ref img (r `div` 2) (c `div` 2)
    | otherwise = mempty
      
pad :: (Image img, Monoid (Pixel img)) => Int -> Int -> img -> img    
pad rs cs img@(dimensions -> (rows, cols)) = makeImage rs cs pad where
  pad r c 
    | r < rows && c < cols = ref img r c
    | otherwise = mempty
  
crop :: (Image img) => Int -> Int -> Int -> Int -> img -> img
crop r0 c0 w h img = makeImage w h crop where
  crop r c = ref img (r+r0) (c+c0)
                  
maxIntensity :: (Image img, MaxMin (Pixel img)) => img -> Pixel img
maxIntensity = maximal . pixelList

minIntensity :: (Image img, MaxMin (Pixel img)) => img -> Pixel img
minIntensity = minimal . pixelList

leftToRight :: (Image img) => img -> img -> img
leftToRight i0 i1 = makeImage (rows i0) cols' concat where
  cols' = sum . map cols $ [i0, i1]
  concat r c 
    | c < (cols i0) = ref i0 r c
    | otherwise = ref i1 r (c - (cols i0))
                  
leftToRight' :: (Image (Elem a), Listable a) => a -> Elem a
leftToRight' (toList -> imgs) = foldr1 leftToRight imgs

topToBottom :: (Image img) => img -> img -> img
topToBottom i0 i1 = makeImage rows' (cols i0) concat where
  rows' = sum . map rows $ [i0, i1]
  concat r c
    | r < (rows i0) = ref i0 r c
    | otherwise = ref i1 (r - (rows i0)) c

topToBottom' :: (Image (Elem a), Listable a) => a -> Elem a
topToBottom' (toList -> imgs) = foldr1 topToBottom imgs

normalize :: (Image img,
              Scaleable (Pixel img),
              MaxMin (Pixel img),
              Num (Pixel img)) => img -> img
normalize img@(dimensions -> (rows, cols)) = makeImage rows cols map where
  map r c = scale `mult` ((ref img r c) - min)
  (min, max) = (minimal px, maximal px)
  scale = 1 `divide` (max - min)
  px = pixelList img

imageFold :: Image img => (Pixel img -> b -> b) -> b -> img -> b
imageFold f init img = foldr f init (pixelList img)

arrayToImage :: (Image img) => Array (Int, Int) (Pixel img) -> img
arrayToImage arr = makeImage rows cols ref where
  ((rmin,cmin), (rmax, cmax)) = bounds arr
  rows = rmax - rmin + 1
  cols = cmax - cmin + 1
  ref r c = arr ! (r, c)
  
imageToArray :: (Image img) => img -> Array (Int, Int) (Pixel img)
imageToArray img@(dimensions -> (rows, cols)) = listArray bounds elems where
  bounds = ((0,0), (rows-1,cols-1))
  elems = pixelList img
    
imageMap :: (Image a, Image b) => (Pixel a -> Pixel b) -> a -> b
imageMap f img@(dimensions -> (rows, cols)) = makeImage rows cols map where
  map r c = f (ref img r c)

imageOp :: (Image img) => 
           (Pixel img -> Pixel img -> Pixel img) -> img -> img -> img
imageOp op i0 i1 = makeImage (rows i0) (cols i0) operate where
  operate r c = op (ref i0 r c) (ref i1 r c)