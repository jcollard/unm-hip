{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Data.Image.Math((+.), (.+),
                       (.-), (-.),
                       (/.), (./),
                       (*.), (.*))  where

import Data.Image.Internal
          
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