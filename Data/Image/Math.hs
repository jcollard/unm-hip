{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Data.Image.Math(BinaryPixel(..),
                       toBinaryImage, compareImage,
                       (<.), (.<), (.<.),
                       (>.), (.>), (.>.),
                       (==.), (.==), (.==.),
                       (+.), (.+), (.+.),
                       (.-), (-.), (.-.),
                       (/.), (./), (./.),
                       (*.), (.*), (.*.))  where

import Data.Image.Internal

class BinaryPixel px where
  off :: px
  on  :: px

toBinaryImage :: (Image img,
                  BinaryPixel (Pixel img)) => (Pixel img -> Bool) -> img -> img
toBinaryImage pred img@(dimensions -> (rows, cols)) = makeImage rows cols bin where
  bin r c = if pred (ref img r c) then on else off
  
compareImage :: (Image img,
                 BinaryPixel (Pixel img),
                 Ord (Pixel img)) => ((Pixel img) -> (Pixel img) -> Bool) -> img -> img -> img
compareImage comp img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols bin where
  bin r c = if p0 `comp` p1 then on else off where
    p0 = ref img0 r c
    p1 = ref img1 r c


(.<) :: (Image img, 
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.<) img num = toBinaryImage pred img where
  pred p = p < num

(<.) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(<.) = flip (.>)

(.<.) :: (Image img,
          BinaryPixel (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.<.) = compareImage (<)

(.>) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.>) img num = toBinaryImage pred img where
  pred p = p >  num
  
(>.) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(>.) = flip (.<)

(.>.) :: (Image img,
          BinaryPixel (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.>.) = compareImage (>)

(.==) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => img -> a -> img
(.==) img num = toBinaryImage pred img where
  pred p = p == num

(==.) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => a -> img -> img
(==.) = flip (.==)
  
(.==.) :: (Image img,
           BinaryPixel (Pixel img),
           Eq (Pixel img)) => img -> img -> img
(.==.) img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols img where
  img r c = if p0 == p1 then p0 else off where
    p0 = ref img0 r c
    p1 = ref img1 r c
        
(.+) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.+) img a = imageMap (+ a) img

(+.) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => a -> img -> img
(+.) = flip (.+)

(.+.) :: (Image img,
          Num (Pixel img)) => img -> img -> img
(.+.) = imageOp (+)

(.-) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.-) img a = imageMap (flip (-) a) img

(-.) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => a -> img -> img
(-.) a img = imageMap ((-) a) img

(.-.) :: (Image img,
          Num (Pixel img)) => img -> img -> img
(.-.) = imageOp (-)

(./) :: (Image img,
         Fractional (Pixel img),
         Pixel img ~ a) => img -> a -> img
(./) img a = imageMap (/ a) img

(/.) :: (Image img,
         Fractional (Pixel img),
         Pixel img ~ a) => a -> img -> img
(/.) num img = imageMap (num /) img

(./.) :: (Image img,
          Fractional (Pixel img)) => img -> img -> img
(./.) = imageOp (/)

(.*) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.*) img a = imageMap (* a) img

(*.) :: (Image img,
         Num (Pixel img),
         Pixel img ~ a) => a -> img -> img
(*.) = flip (.*)

(.*.) :: (Image img,
          Num (Pixel img)) => img -> img -> img
(.*.) = imageOp (*)


