{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts #-}
module Data.Image.Math(Binary,
                       binary, compareImage,
                       (<.), (.<), (.<.),
                       (>.), (.>), (.>.),
                       (==.), (.==), (.==.),
                       (+.), (.+), (.+.),
                       (.-), (-.), (.-.),
                       (/.), (./), (./.),
                       (*.), (.*), (.*.))  where

import Data.Image.Internal

class Binary b where
  false :: b
  true  :: b

instance Binary Double where
  false = zero
  true = 1.0

binary :: (Image img,
             Binary (Pixel img)) => (Pixel img -> Bool) -> img -> img
binary pred img@(dimensions -> (rows, cols)) = makeImage rows cols bin where
  bin r c = if pred (ref img r c) then true else false
  
compareImage :: (Image img,
                 Binary (Pixel img),
                 Ord (Pixel img)) => ((Pixel img) -> (Pixel img) -> Bool) -> img -> img -> img
compareImage comp img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols bin where
  bin r c = if p0 `comp` p1 then true else false where
    p0 = ref img0 r c
    p1 = ref img1 r c


(.<) :: (Image img, 
         Binary (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.<) img num = binary pred img where
  pred p = p < num

(<.) :: (Image img,
         Binary (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(<.) = flip (.>)

(.<.) :: (Image img,
          Binary (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.<.) = compareImage (<)

(.>) :: (Image img,
         Binary (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.>) img num = binary pred img where
  pred p = p >  num
  
(>.) :: (Image img,
         Binary (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(>.) = flip (.<)

(.>.) :: (Image img,
          Binary (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.>.) = compareImage (>)

(.==) :: (Image img,
          Binary (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => img -> a -> img
(.==) img num = binary pred img where
  pred p = p == num

(==.) :: (Image img,
          Binary (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => a -> img -> img
(==.) = flip (.==)
  
(.==.) :: (Image img,
           Zero (Pixel img),
           Eq (Pixel img)) => img -> img -> img
(.==.) img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols img where
  img r c = if p0 == p1 then p0 else zero where
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


