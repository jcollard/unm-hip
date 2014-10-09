--    The University of New Mexico's Haskell Image Processing Library
--    Copyright (C) 2013 Joseph Collard
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ViewPatterns, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
{-# OPTIONS -O2 #-}
module Data.Image.Complex(-- * Complex Images
                          ComplexPixel(..),
                          -- * Discrete Fourier Transformations
                          makeFilter,
                          fft, ifft,
                          -- * Converting Complex Images
                          complex,
                          realPart,
                          imagPart,
                          magnitude,
                          angle,
                          shrink,
                          complexImageToRectangular,
                          complexImageToPolar,
                          module Data.Complex) where

import Data.Image.Internal(Image(..), PixelOp, imageMap,dimensions)

--base>=4
import qualified Data.Complex as C
import Data.Complex(Complex((:+)))
import Data.List(transpose)
import Data.Bits

--vector>=0.10.0.1
import qualified Data.Vector as V

class RealFloat (Value px) => ComplexPixel px where
  type Value px :: *
  toComplex ::  px -> C.Complex (Value px)
  fromComplex :: C.Complex (Value px) -> px
  
{-| Given a positive integer m, a positive integer n, and a function 
    returning a pixel value, makeFilter returns an image with m rows 
    and n columns. Let x equal i if i is less than m/2 and i - m otherwise 
    and let y equal j if j is less than n/2 and j - n otherwise. To match 
    the periodicity of the 2D discrete Fourier spectrum, the value of the 
    result image at location (i, j) is computed by applying the function to x 
    and y, e.g., the value at location (0, 0) is the result of applying the 
    function to 0 and 0, the value at (m-1, n-1) is the result of applying 
    function to -1 and -1.

    >>> makeFilter 128 128 (\ r c -> fromIntegral (r + c)) :: GrayImage
    < Image 128x128 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/makefilter.jpg>

    @
    laplacianOfGaussian :: Double -> Int -> Int -> Double
    laplacianOfGaussian stddev i j = ((-pi) / (stddev*stddev)) * (1 - x) * (exp (-x)) where
      r = fromIntegral $ i*i + j*j
      x = r / (2.0*stddev)
    @

    >>>let d2g = makeFilter 128 128 (laplacianOfGaussian 8) :: GrayImage
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/d2g.jpg>

    @
    gaussian :: Double -> Int -> Int -> Double 
    gaussian variance i j = exp (-x) where
      r = fromIntegral $ i*i + j*j
      x = r / (2.0*pi*variance)
    @

    >>>let g = makeFilter 128 128 (gaussian 8) :: GrayImage
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/g.jpg>
-}
makeFilter :: (Image img) => Int -> Int -> (PixelOp (Pixel img)) -> img
makeFilter rows cols func = makeImage rows cols func' where
  func' r c = let x = if r < (rows `div` 2) then r else r-rows 
                  y = if c < (cols `div` 2) then c else c-cols
              in func x y

{-| Given an image whose pixels can be converted to a complex value, 
    fft returns an image with complex pixels representing its 2D discrete 
    Fourier transform (DFT). Because the DFT is computed using the Fast Fourier 
    Transform (FFT) algorithm, the number of rows and columns of the image 
    must both be powers of two, i.e., 2K where K is an integer.

    >>>frog <- readImage "images/frog.pgm"
    >>>let frogpart = crop 64 64 128 128 frog
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/frog.jpg>
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/frogpart.jpg>

    >>>magnitude . imageMap log . fft $ frogpart

    <https://raw.github.com/jcollard/unm-hip/master/examples/fft.jpg>  
    
    >>>fft d2g
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/fftd2g.jpg>  
    
    >>>fft g
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/fftg.jpg>      
-}
fft :: (Image img,
        ComplexPixel (Pixel img),
        Image img',
        Pixel img' ~ C.Complex (Value (Pixel img))) => img -> img'
fft img@(dimensions -> (rows, cols)) = makeImage rows cols transformed where
  vector = V.map toComplex . V.fromList . pixelList $ img
  transformed r c = helper vector V.! (r * cols + c)
  helper ss = (V.zipWith (+) ys ts) V.++ (V.zipWith (-) ys ts) where
    n = V.length ss
    ys = helper evns
    zs = helper ods
    (evns, ods) = (V.ifilter (\i e -> even i) ss, V.ifilter (\i e -> odd i) ss)
    ts = V.zipWith (\z k -> exp' k n * z) zs (V.enumFromN 0 (V.length zs))
    exp' k n = C.cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

{-| Given an image, ifft returns a complex image representing its 2D 
    inverse discrete Fourier transform (DFT). Because the inverse DFT is 
    computed using the Fast Fourier Transform (FFT) algorithm, the number 
    of rows and columns of <image> must both be powers of two, i.e., 2K 
    where K is an integer. 

    >>>ifft ((fft frogpart) * (fft d2g))

    <https://raw.github.com/jcollard/unm-hip/master/examples/ifft.jpg>
    
    >>>ifft ((fft frogpart) * (fft g))

    <https://raw.github.com/jcollard/unm-hip/master/examples/ifft2.jpg>
 -}
ifft :: (Image img,
        ComplexPixel (Pixel img),
        Image img',
        Pixel img' ~ C.Complex (Value (Pixel img))) => img -> img'
ifft img@(dimensions -> (rows, cols)) = check where
  check = if (isPowerOfTwo rows && isPowerOfTwo cols) then makeImage rows cols fftimg else error "Image is not a power of 2 in rows and cols"
  fftimg r c = fft' V.! (r*cols + c)
  vector = V.map toComplex . V.fromList . pixelList $ img
  fft' = ifftv rows cols vector

{-| Given a complex image, returns a real image representing
    the real part of the image.

    @
    harmonicSignal :: Double -> Double -> Int -> Int -> C.Complex Double
    harmonicSignal u v m n = exp (-pii*2.0 * var) where 
      pii = 0.0 C.:+ pi
      var = (u*m' + v*n') C.:+ 0.0
      [m',n'] = map fromIntegral [m, n]
    @

    >>> let signal = makeImage 128 128 (harmonicSignal (3/128) (2/128)) :: ComplexImage

    <https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg>

    >>>let cosine = realPart signal

    <https://raw.github.com/jcollard/unm-hip/master/examples/cosine.jpg>

    >>>realPart . ifft $ (fft frogpart) * (fft d2g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart.jpg>
    
    >>>realPart . ifft $ (fft frogpart) * (fft g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart2.jpg>

-}
realPart :: (Image img,
             ComplexPixel (Pixel img),
             Image img',
             Pixel img' ~ Value (Pixel img)) => img -> img'
realPart = imageMap (C.realPart . toComplex)

{-| Given a complex image, returns a real image representing
   the imaginary part of the image

   >>>let sine = imagPart signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/sine.jpg>

 -}
imagPart :: (Image img,
             ComplexPixel (Pixel img),
             Image img',
             Pixel img' ~ Value (Pixel img)) => img -> img'
imagPart = imageMap (C.imagPart . toComplex)

{-| Given a complex image, returns a real image representing
    the magnitude of the image.

    >>>magnitude signal

 -}
magnitude :: (Image img,
              ComplexPixel (Pixel img),
              Image img',
              Pixel img' ~ Value (Pixel img)) => img -> img'
magnitude = imageMap (C.magnitude . toComplex)

{-| Given a complex image, returns a real image representing
    the angle of the image 
   
    >>>angle signal

    <https://raw.github.com/jcollard/unm-hip/master/examples/angle.jpg>
-}
angle :: (Image img,
          ComplexPixel (Pixel img),
          Image img',
          Pixel img' ~ Value (Pixel img)) => img -> img'
angle = imageMap (C.phase . toComplex)

{-| Given a complex image, returns a pair of real images each
    representing the component (magnitude, phase) of the image

    >>>leftToRight' . complexImageToPolar $ signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/compleximagetopolar.jpg>
-}
complexImageToPolar :: (Image img,
                        ComplexPixel (Pixel img),
                        Image img',
                        Pixel img' ~ Value (Pixel img)) => img -> (img', img')
complexImageToPolar img@(dimensions -> (rows, cols)) = (mkImg mag, mkImg phs) where
  mkImg = makeImage rows cols
  ref' r c = C.polar . toComplex . ref img r $ c
  mag r c = fst (ref' r c)
  phs r c = snd (ref' r c)

{-| Given an image representing the real part of a complex image, and
    an image representing the imaginary part of a complex image, returns
    a complex image.

    >>>complex cosine sine

    <https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg>
 -}
complex :: (Image img,
            Image img',
            Pixel img' ~ C.Complex (Pixel img)) => img -> img -> img'
complex real imag@(dimensions -> (rows, cols)) = makeImage rows cols ri where
  ri r c = (ref real r c) C.:+ (ref imag r c)

{-| Given a complex image, return a pair of real images each representing
    a component of the complex image (real, imaginary).

    >>>leftToRight' . complexImageToRectangular $ signal

    <https://raw.github.com/jcollard/unm-hip/master/examples/complexsignaltorectangular.jpg>
 -}
complexImageToRectangular :: (Image img,
                              ComplexPixel (Pixel img),
                              Image img',
                              Pixel img' ~ Value (Pixel img)) => img -> (img', img')
complexImageToRectangular img = (realPart img, imagPart img)

{-| Given a complex image and a real positive number x, shrink returns 
    a complex image with the same dimensions. Let z be the value of the 
    image at location (i, j). The value of the complex result image at 
    location (i, j) is zero if |z| < x, otherwise the result has the 
    same phase as z but the amplitude is decreased by x.
   
 -}
shrink :: (Image img,
           ComplexPixel (Pixel img)) => (Value (Pixel img)) -> img -> img
shrink x img@(dimensions -> (rows, cols)) = 
  makeImage rows cols shrink' where
    shrink' r c = fromComplex . helper $ px where
      px = toComplex . ref img r $ c
      helper px
        | (C.magnitude px) < x = 0.0 C.:+ 0.0
        | otherwise = real C.:+ imag where
          mag = C.magnitude px
          x' = (mag - x)/mag
          real = x'*(C.realPart px)
          imag = x'*(C.imagPart px)


type Vector a = V.Vector (Complex a)
type FFT a = [Int] -> Vector a -> Int -> Int -> [Complex a] 

-- FFT support code

fftv :: (RealFloat a) => Int -> Int -> Vector a -> Vector a
fftv = fft' fftRange

ifftv :: (RealFloat a) => Int -> Int -> Vector a -> Vector a
ifftv rows cols vec = V.map (/fromIntegral (rows*cols)) . fft' ifftRange rows cols $ vec

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n /= 0 && (n .&. (n-1)) == 0

fft' :: FFT a -> Int -> Int -> Vector a -> Vector a
fft' range rows cols orig = if check then fromRows rows' else err where 
  check = and . map isPowerOfTwo $ [rows, cols]
  err = error "FFT can only be applied to images with dimensions 2^k x 2^j where k and j are integers."
  (fromColumns -> cols') = map (fftc range rows cols 0 (rows-1) orig) [0..cols-1] -- FFT on each col
  rows' = map (fftr range cols 0 (cols-1) cols') [0..rows-1] -- FFT on each row

fromColumns :: [[Complex a]] -> V.Vector (Complex a)
fromColumns = fromRows . transpose

fromRows :: [[Complex a]] -> V.Vector (Complex a)
fromRows = V.fromList . concat

fftc :: FFT a -> Int -> Int -> Int -> Int -> Vector a -> Int -> [Complex a]
fftc fftfunc rows cols sIx eIx orig row = fftfunc indices orig rows 1 where
  indices = map ((+row) . (*cols)) $ [sIx..eIx]

fftr :: FFT a -> Int -> Int -> Int -> Vector a ->  Int -> [Complex a]
fftr fftfunc cols sIx eIx orig row = fftfunc indices orig cols 1 where
  indices = map (+ (row*cols)) $ [sIx..eIx]

fftRange :: (RealFloat a) => FFT a
fftRange = range (-2*pii)

ifftRange :: (RealFloat a) => FFT a
ifftRange = range (2*pii)

range :: (RealFloat a) => Complex a -> FFT a
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

pii :: (Floating a) => Complex a
pii = 0 :+ pi

-- End FFT support code
