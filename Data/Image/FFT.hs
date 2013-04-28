{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.FFT(fft, ifft,
                      Vector) where

import Data.Complex(Complex((:+)))
import Data.List(transpose)
import qualified Data.Vector as V

type Vector = V.Vector (Complex Double)
type FFT = [Int] -> Vector -> Int -> Int -> [Complex Double]

fft' :: FFT -> Int -> Int -> Vector -> Vector
fft' range rows cols orig = fromRows rows' where 
  (fromColumns -> cols') = map (fftc range rows cols 0 (rows-1) orig) [0..cols-1] -- FFT on each col
  rows' = map (fftr range cols 0 (cols-1) cols') [0..rows-1] -- FFT on each row

fft :: Int -> Int -> Vector -> Vector
fft = fft' fftRange

ifft :: Int -> Int -> Vector -> Vector
ifft rows cols vec = fmap (/fromIntegral (rows*cols)) . fft' ifftRange rows cols $ vec

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

