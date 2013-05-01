{-# LANGUAGE ViewPatterns, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
{-# OPTIONS -O2 #-}
module Data.Image.Complex(ComplexPixel(..),
                          complex,
                          realPart,
                          imagPart,
                          magnitude,
                          angle,
                          polar,
                          complexImageToRectangular,
                          complexImageToPolar,
                          makeFilter,
                          fft, ifft) where

import Data.Image.Internal
import Data.Monoid
import qualified Data.Complex as C
import qualified Data.Image.FFT as FFT
import qualified Data.Vector as V

class ComplexPixel px where
  toComplex :: px -> C.Complex Double

realPart :: (Image img,
             Image img',
             Pixel img' ~ Double,
             ComplexPixel (Pixel img)) => img -> img'
realPart = imageMap (C.realPart . toComplex)

imagPart :: (Image img,
             Image img',
             Pixel img' ~ Double,
             ComplexPixel (Pixel img)) => img -> img'
imagPart = imageMap (C.imagPart . toComplex)

magnitude :: (Image img,
             Image img',
             RealFloat (Pixel img'),
             Pixel img ~ C.Complex (Pixel img')) => img -> img'
magnitude = imageMap C.magnitude

angle :: (Image img,
          Image img',
          RealFloat (Pixel img'),
          Pixel img ~ C.Complex (Pixel img')) => img -> img'
angle = imageMap C.phase

polar :: (Image img,
          Image img',
          RealFloat (Pixel img'),
          Pixel img ~ C.Complex (Pixel img')) => img -> (img', img')
polar img@(dimensions -> (rows, cols)) = (mkImg mag, mkImg phs) where
  mkImg = makeImage rows cols
  ref' r c = C.polar $ (ref img r c)
  mag r c = fst (ref' r c)
  phs r c = snd (ref' r c)

complex :: (Image img,
            Image img',
            Pixel img' ~ C.Complex (Pixel img)) => img -> img -> img'
complex real imag@(dimensions -> (rows, cols)) = makeImage rows cols ri where
  ri r c = (ref real r c) C.:+ (ref imag r c)

complexImageToRectangular :: (Image img,
                              Image img',
                              Pixel img' ~ Double,
                              ComplexPixel (Pixel img)) => img -> (img', img')
complexImageToRectangular img = (realPart img, imagPart img)

complexImageToPolar
  :: (RealFloat (Pixel img'), Image img, Image img',
      Pixel img ~ C.Complex (Pixel img')) =>
     img -> (img', img')
complexImageToPolar img = (magnitude img, angle img)

makeFilter :: (Image img) => Int -> Int -> (PixelOp (Pixel img)) -> img
makeFilter rows cols func = makeImage rows cols func' where
  func' r c = let x = if r < (rows `div` 2) then r else r-rows 
                  y = if c < (cols `div` 2) then c else c-cols
              in func x y

fft :: (Image img,
        Image img',
        ComplexPixel (Pixel img),
        Pixel img' ~ C.Complex Double) => img -> img'
fft img@(dimensions -> (rows, cols)) = makeImage rows cols fftimg where
  fftimg r c = fft' V.! (r*cols + c)
  vector = V.map toComplex . V.fromList . pixelList $ img
  fft' = FFT.fft rows cols vector

ifft :: (Image img,
        Image img',
        ComplexPixel (Pixel img),
        Pixel img' ~ C.Complex Double) => img -> img'
ifft img@(dimensions -> (rows, cols)) = makeImage rows cols fftimg where
  fftimg r c = fft' V.! (r*cols + c)
  vector = V.map toComplex . V.fromList . pixelList $ img
  fft' = FFT.ifft rows cols vector

