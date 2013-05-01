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

import Data.Image.Internal(Image(..), PixelOp, imageMap,dimensions)
import qualified Data.Complex as C
import Data.Complex(Complex((:+)))
import Data.List(transpose)
import qualified Data.Vector as V

import Data.Bits


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
  fft' = fftv rows cols vector

ifft :: (Image img,
        Image img',
        ComplexPixel (Pixel img),
        Pixel img' ~ C.Complex Double) => img -> img'
ifft img@(dimensions -> (rows, cols)) = makeImage rows cols fftimg where
  fftimg r c = fft' V.! (r*cols + c)
  vector = V.map toComplex . V.fromList . pixelList $ img
  fft' = ifftv rows cols vector

type Vector = V.Vector (Complex Double)
type FFT = [Int] -> Vector -> Int -> Int -> [Complex Double]

-- FFT support code

fftv :: Int -> Int -> Vector -> Vector
fftv = fft' fftRange

ifftv :: Int -> Int -> Vector -> Vector
ifftv rows cols vec = V.map (/fromIntegral (rows*cols)) . fft' ifftRange rows cols $ vec

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0

fft' :: FFT -> Int -> Int -> Vector -> Vector
fft' range rows cols orig = if check then fromRows rows' else err where 
  check = and . map isPowerOfTwo $ [rows, cols]
  err = error "FFT can only be applied to images with dimensions 2^k x 2^j where k and j are integers."
  (fromColumns -> cols') = map (fftc range rows cols 0 (rows-1) orig) [0..cols-1] -- FFT on each col
  rows' = map (fftr range cols 0 (cols-1) cols') [0..rows-1] -- FFT on each row

fromColumns :: [[Complex Double]] -> V.Vector (Complex Double)
fromColumns = fromRows . transpose

fromRows :: [[Complex Double]] -> V.Vector (Complex Double)
fromRows = V.fromList . concat

fftc :: FFT -> Int -> Int -> Int -> Int -> Vector -> Int -> [Complex Double]
fftc fftfunc rows cols sIx eIx orig row = fftfunc indices orig rows 1 where
  indices = map ((+row) . (*cols)) $ [sIx..eIx]

fftr :: FFT -> Int -> Int -> Int -> Vector ->  Int -> [Complex Double]
fftr fftfunc cols sIx eIx orig row = fftfunc indices orig cols 1 where
  indices = map (+ (row*cols)) $ [sIx..eIx]

fftRange :: FFT
fftRange = range (-2*pii)

ifftRange :: FFT
ifftRange = range (2*pii)

range :: Complex Double -> FFT
range e ix vec n s 
  | n == 1 = [vec V.! (head ix)]
  | otherwise = fft' where
    fft' = seperate data'
    fi = fromIntegral
    ix0 = range e ix vec (n `div` 2) (2*s)
    ix1 = range e (drop s ix) vec (n `div` 2) (2*s)
    data' = (flip map) (zip3 ix0 ix1 [0..]) (\ (ix0, ix1, k) -> do
      let e' = exp (e * ((fi k) / (fi n)))
          ix0' = ((ix0 + e' * ix1))
          ix1' = ((ix0 - e' * ix1))
        in (ix0', ix1'))
    
seperate :: [(a, a)] -> [a]
seperate = seperate' [] [] where
  seperate' acc0 acc1 [] = (reverse acc0) ++ (reverse acc1)
  seperate' acc0 acc1 ((a, b):xs) = seperate' (a:acc0) (b:acc1) xs

pii :: Complex Double
pii = 0 :+ pi

-- End FFT support code