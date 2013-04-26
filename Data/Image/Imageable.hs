{-# LANGUAGE 
   TypeFamilies, 
   FlexibleContexts, 
   FlexibleInstances, 
   ViewPatterns #-}
module Data.Image.Imageable(Imageable(..), 
                            PixelOp,
                            Zero(..),
                            Maximal(..),
                            RGB(..),
                            imageOp, 
                            dimensions, 
                            maxIntensity,
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
                            imagePad,
                            imageCrop) where

import Data.List(intercalate)

type PixelOp px = (Int -> Int -> px)

class Imageable i where
  type Pixel i :: *
  rows :: i -> Int
  cols :: i -> Int
  ref  :: i -> Int -> Int -> (Pixel i)
  makeImage :: Int -> Int -> PixelOp (Pixel i) -> i
  pixelList :: i -> [Pixel i]
  pixelList i = [ ref i r c | r <- [0..(rows i - 1)], c <- [0..(cols i - 1)]]
  
class RGB px where
  rgb :: px -> (Double, Double, Double)

class Maximal m where
  maximal :: [m] -> m

instance Maximal Double where
  maximal = maximum
  
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
      
imagePad :: (Imageable img, Zero (Pixel img)) => Int -> Int -> img -> img    
imagePad rs cs img@(dimensions -> (rows, cols)) = makeImage rs cs pad where
  pad r c 
    | r < rows && c < cols = ref img r c
    | otherwise = zero
  
imageCrop :: (Imageable img) => Int -> Int -> Int -> Int -> img -> img
imageCrop r0 c0 w h img = makeImage w h crop where
  crop r c = ref img (r+r0) (c+c0)
                  
maxIntensity :: (Imageable img, Maximal (Pixel img)) => img -> Pixel img
maxIntensity = maximal . pixelList

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