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

{-# LANGUAGE TypeFamilies, ViewPatterns, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Boxed(
  BoxedImage,
  -- * Gray Images
  GrayImage, Gray, readImage, 
  grayToComplex, makeHotImage,
  -- * Color Images
  ColorImage, Color(..), readColorImage,
  colorImageRed, colorImageGreen, colorImageBlue,
  colorImageToRGB, rgbToColorImage, 
  colorImageHue, colorImageSaturation, colorImageIntensity,
  colorImageToHSI, hsiToColorImage, 
  -- * Complex Images
  ComplexImage, Complex,
  CI.makeFilter,
  fft, ifft,
  realPart, imagPart,
  magnitude, angle,
  complex, complexImageToRectangular,
  complexImageToPolar,
  shrink, 
  -- * Binary Images
  distanceTransform, label,
  -- * Additional Modules
    -- | Contains functionality related to Binary Images
  module Data.Image.Binary,  
  -- | Contains functionality for convolution of images
  module Data.Image.Convolution,
  -- | Contains basic functionality for Images
  module Data.Image.Internal,
  -- | Contains functionality for writing images and displaying with an external program
  module Data.Image.IO) where
 
import Data.Image.Binary hiding (distanceTransform, label)
import qualified Data.Image.Binary as Bin
import qualified Data.Image.Complex as CI
import Data.Image.Convolution
import Data.Image.Internal
import Data.Image.IO

--base>=4
import Control.Applicative
import qualified Data.Complex as C
import Data.Maybe(fromJust)
import Data.Monoid

--bytestring-0.10.0.2
import qualified Data.ByteString.Char8 as B

--vector>=0.10.0.1
import qualified Data.Vector as V

type Vector = V.Vector

-- BoxedImage
-- | BoxedImage is a concrete implementation of Image using a boxed internal structure. This allows for it to be installed nicely in Functor and Applicative.
data BoxedImage a = Image { rs :: Int,
                        cs :: Int,
                        pixels :: Vector a} 

instance Image (BoxedImage a) where
  type Pixel (BoxedImage a) = a
  rows = rs
  cols = cs
  ref i r c = (pixels i) V.! (r * (cols i) + c)
  makeImage rows cols f = Image rows cols (V.fromList px) where
    px = [ f r c | r <- [0..rows-1], c <- [0..cols-1]]
  pixelList = V.toList . pixels
  imageOp = liftA2

instance Functor BoxedImage where
  fmap f (Image rows cols pixels) = Image rows cols (fmap f pixels)

instance Applicative BoxedImage where
  pure a = Image 1 1 (V.fromList [a])
  (<*>) (Image rows cols partial) (Image rows' cols' toApply)
    | rows /= rows' && cols /= cols' = error "Cannot apply images of unequal dimensions."
    | otherwise = Image rows cols (V.fromList applied) where
       indices = [ r*cols + c | r <- [0..rows-1], c <- [0..cols-1]]
       applied = map func indices
       func i = (partial V.! i) (toApply V.! i)

instance Show (BoxedImage a) where
  show (Image rows cols _) = "< Image " ++ (show rows) ++ "x" ++ (show cols) ++ " >"

instance Num a => Num (BoxedImage a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i

instance Fractional a => Fractional (BoxedImage a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational i = pure $ fromRational i
  
instance Eq a => Eq (BoxedImage a) where
  (==) img0 img1 
    | (rows img0) /= (rows img1) = False
    | (cols img0) /= (cols img1) = False
    | otherwise = and . zipWith (==) (pixelList img0) $ pixelList img1

-- GrayImage
{-| A concrete instance of Image representing a gray scale image.
    This instance is installed in DisplayFormat as a gray PGM.
 -}
type GrayImage = BoxedImage Gray
type Gray = Double

instance DisplayFormat GrayImage where
  format = toPGM

instance GrayPixel Gray where
  type GrayVal Gray = Gray
  toGray = id

instance RGBPixel Gray where
  type ColorVal Gray = Gray
  toRGB px = (px, px, px)

instance HSIPixel Gray where
  toHSI = toHSI . RGB . toRGB
  
instance BinaryPixel Gray where
  toBinary 0.0 = False
  toBinary _ = True
  on = 1.0
  off = 0.0
  
instance CI.ComplexPixel Gray where
  type Value Gray = Double
  toComplex i =  i C.:+ 0.0

instance Monoid Gray where
  mempty = 0.0
  mappend = (+)
  
instance MaxMin Gray where
  maximal = maximum
  minimal = minimum

-- ColorImage
{-| A concrete instance of Image that represents images with color values.
    This instance is installed in DisplayFormat and can be written to 
    a color PPM -}
type ColorImage = BoxedImage Color

class HSIPixel px where
  toHSI :: px -> (Double, Double, Double)

instance DisplayFormat ColorImage where
  format = toPPM

-- | A color encoding scheme
data Color = 
             -- | Red, Green, Blue encoding
             RGB (Double, Double, Double)
             -- | Hue, Saturation, Intensity encoding
           | HSI (Double, Double, Double) deriving (Show, Eq)

instance GrayPixel Color where
  type GrayVal Color = Double
  toGray (RGB (r, g, b)) = (r + g + b) / 3.0
  toGray (toRGB -> (r, g, b)) = (r + g + b) / 3.0

instance RGBPixel Color where
  type ColorVal Color = Double
  toRGB (RGB px) = px
  toRGB (HSI (h, s, i)) = (r, g, b) where
    r = i + v1
    g = i - (v1/2) + v2
    b = i - (v1/2) - v2
    v1 = const*s*(cos h)/3
    v2 = const*s*(sin h)/2
    const = 2.44948974278318
  
instance HSIPixel Color where
  toHSI (RGB (r, g, b)) = (h, s, i) where
    h = if (v1 /= 0.0) then atan2 v2 v1 else 0
    s = sqrt( (v1*v1) + (v2*v2) )
    i = (r+g+b)/3
    v1 = (2.0*r-g-b) / const
    v2 = (g - b) / const
    const = 2.44948974278318    
  toHSI (HSI px) = px
    
--Requires the image to be scaled 
--instance ComplexPixel Color where
--  toComplex = undefined 

instance BinaryPixel Color where
  toBinary (toRGB -> (r, g, b)) 
    | r == 0 && g == 0 && b == 0 = False
    | otherwise = True
  on = RGB (1.0, 1.0, 1.0)
  off = RGB (0.0, 0.0, 0.0)

instance Monoid Color where
  mempty = RGB (0.0, 0.0, 0.0)
  mappend (toRGB -> (a,b,c)) (toRGB -> (d,e,f)) = RGB (a+d,b+e,c+f)

instance MaxMin Color where
  maximal = helper max mempty . map toRGB
  minimal = helper min (RGB (10e10, 10e10, 10e10)) . map toRGB
  
helper :: (Double -> Double -> Double) -> Color -> [(Double, Double, Double)] -> Color
helper compare (RGB (r,g,b)) [] = let i = foldr1 compare [r, g, b] in RGB (i,i,i)
helper compare (RGB (r, g, b)) ((r', g', b'):xs) = helper compare acc' xs where
  acc' = (RGB (compare r r', compare g g', compare b b'))

instance Num Color where 
  (+) = colorOp (+)
  (-) = colorOp (-)
  (*) = colorOp (*)
  abs (toRGB -> (r, g, b)) = RGB (abs r, abs g, abs b)
  signum (toRGB -> (r, g, b)) = RGB (signum r, signum g, signum b)
  fromInteger (fromIntegral -> i) = RGB (i,i,i)

instance Fractional Color where
  (/) = colorOp (/)
  recip (toRGB -> (r,g,b)) = RGB (recip r, recip g, recip b)
  fromRational _ = error "Could not create Color from Rational."

colorOp :: (Double -> Double -> Double) -> Color -> Color -> Color
colorOp op (toRGB -> (a, b, c)) (toRGB -> (d, e, f)) = RGB (op a d, op b e, op c f)

-- ComplexImage
{-| A concrete instance of Image representing pixels as complex values. 
    This instance can be written to file as a color PPM.
 -}
type ComplexImage = BoxedImage Complex
type Complex = C.Complex Double

instance BinaryPixel Complex where
  toBinary (0.0 C.:+ 0.0) = False
  toBinary _ = True
  on = (1.0 C.:+ 0.0)
  off = (0.0 C.:+ 0.0)

instance DisplayFormat ComplexImage where
  format (complexImageToColorImage -> rgb) = toPPM rgb

instance CI.ComplexPixel Complex where
  type Value Complex = Double
  toComplex = id

complexImageToColorImage :: ComplexImage -> ColorImage
complexImageToColorImage img = fmap rgb img where
  scale = complexScale img
  rgb comp = if radius < 1 then RGB (red', grn', blu') else RGB (red, grn, blu) where
    [red, grn, blu] = map (+d') [r',g',b']
    [red', grn', blu'] = map (flip (-) d')  [r',g',b']
    [x, y] = map (*scale) [C.realPart comp, C.imagPart comp]
    radius = sqrt((x*x) + (y*y))
    a = onedivsqrtsix*x
    b = sqrttwodivtwo*y
    d = 1.0/(1.0 + (radius*radius))
    d' = 0.5 - radius*d
    r' = 0.5 + (twodivsqrtsix * x * d)
    b' = 0.5 - (d * (a - b))
    g' = 0.5 - (d * (a + b))
    
complexScale :: ComplexImage -> Double
complexScale (CI.complexImageToRectangular -> (real, imag)) = 2.0/(maxv - minv) where
    maxr = maximum . pixelList $ (real :: GrayImage)
    maxi = maximum . pixelList $ imag
    minr = minimum . pixelList $ real
    mini = minimum . pixelList $ imag
    maxv = max maxr maxi
    minv = min minr mini
    
twodivsqrtsix = 0.81649658092772603273 --2.0/sqrt(6)
onedivsqrtsix = 0.40824829046386301636 --1.0/sqrt(6)
sqrttwodivtwo = 0.70710678118654752440 --sqrt(2)/2.0
  
getComponent to component img = fmap (component . to) img
  
getRGB = getComponent toRGB
 
{-| Given a ColorImage, returns a GrayImage representing the Red color component
    
    >>>let red = colorImageRed cactii

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimagered.jpg>
 -}
colorImageRed :: ColorImage -> GrayImage
colorImageRed = getRGB (\ (r, _, _) -> r)

{-| Given a ColorImage, returns a GrayImage representing the Green color component

    >>>let green = colorImageGreen cactii

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimagegreen.jpg>
 -}
colorImageGreen :: ColorImage -> GrayImage
colorImageGreen = getRGB (\ (_,g,_) -> g)

{-| Given a ColorImage, returns a GrayImage representing the Blue color component
   
    >>>let blue = colorImageBlue cactii

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimageblue.jpg>
 -}
colorImageBlue :: ColorImage -> GrayImage
colorImageBlue = getRGB (\ (_,_,b) -> b)

{-| Given a ColorImage, returns a triple containing three GrayImages each
    containing one of the color components (red, green, blue)

    >>>leftToRight' . colorImageToRGB $ cactii

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimagetorgb.jpg>
 -}
colorImageToRGB :: ColorImage -> (GrayImage, GrayImage, GrayImage)
colorImageToRGB img = (colorImageRed img, colorImageGreen img, colorImageBlue img)

{-| Given a triple containing three GrayImages each containing one of the
    color components (red, green, blue), returns a ColorImage

    >>>rgbToColorImage (red,green,blue)

    <https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg>
 -}
rgbToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage
rgbToColorImage (red, green, blue) = createRGB <$> red <*> green <*> blue where
  createRGB r g b = RGB (r, g, b)

getHSI = getComponent toHSI

{-| Given a ColorImage, returns a GrayImage representing the Hue component

    >>>let h = colorImageHue cactii

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimagehue.jpg>
 -}
colorImageHue :: ColorImage -> GrayImage
colorImageHue = getHSI (\ (h, _, _) -> h)

{-| Given a ColorImage, returns a GrayImage representing the Saturation component

    >>>let s = colorImageSaturation cactii
  
    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimagesaturation.jpg>
 -}
colorImageSaturation :: ColorImage -> GrayImage
colorImageSaturation = getHSI (\ (_,s,_) -> s)

{-| Given a ColorImage, returns a GrayImage representing the Intensity component

    >>>let i = colorImageIntensity cactii 

    <https://raw.github.com/jcollard/unm-hip/master/examples/colorimageintensity.jpg>

 -}
colorImageIntensity :: ColorImage -> GrayImage
colorImageIntensity = getHSI (\ (_,_,i) -> i)

{-| Given a triple containing three GrayImages each containing one of the
    color components (hue, saturation, ), returns a ColorImage

    >>> hsiToColorImage (h, s, i) 

    <https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg>
 -}
hsiToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage
hsiToColorImage (h, s, i) = toHSI <$> h <*> s <*> i where
  toHSI h s i = HSI (h, s, i)

{-| Given a ColorImage, returns a triple containing three GrayImages each
    containing one of the components (hue, saturation, intensity)

    >>>let (h, s, i) = colorImageToHSI $ cactii
 -}
colorImageToHSI :: ColorImage -> (GrayImage, GrayImage, GrayImage)
colorImageToHSI img = (colorImageHue img, colorImageSaturation img, colorImageIntensity img) 


{-| Reads in an ASCI PPM file as a ColorImage
    
    >>>cactii <- readColorImage "images/cactii.ppm"

    <https://raw.github.com/jcollard/unm-hip/master/examples/cactii.jpg>
 -}
readColorImage :: FilePath -> IO ColorImage
readColorImage fileName =
  do
    y <- B.readFile fileName
    return $ parseRGBPixelImage . B.intercalate (B.pack " ") . stripComments . B.lines $ y
    
parseRGBPixelImage :: B.ByteString -> ColorImage
parseRGBPixelImage string = Image rows cols (V.fromList rgbs)
  where ws = B.words string
        getInt = fst. fromJust . B.readInt
        px = map (fromIntegral . getInt) $ drop 4 ws
        cols = getInt $ ws !! 1
        rows = getInt $ ws !! 2
        maxi = fromIntegral . getInt $ ws !! 3
        [r, g, b] = colors px
        rgbs = map rgb3 . zip3 r g $ b
        rgb3 (r, g, b) = RGB (r, g, b)

colors :: [Int] -> [[Gray]]
colors xs = helper xs [] [] []
  where helper [] red green blue = map (map fromIntegral) $ map reverse [red, green, blue]
        helper (r:g:b:cs) red green blue = helper cs (r:red) (g:green) (b:blue)

{-| Coerces a GrayImage to a ComplexImage where the imaginary 
    part for all pixels is 0.

    >>>grayToComplex frog

 -}
grayToComplex :: GrayImage -> ComplexImage
grayToComplex img = fmap (C.:+ 0.0) img

{-| Given a GrayImage, makeHotImage returns a ColorImage with the same 
    dimensions. The R, G, B values of the result image at (i, j) are 
    determined by using the value of the ColorImage at (i, j) to index 
    three lookup tables. These lookup tables implement a false coloring 
    scheme which maps small values to black, large values to white, and 
    intermediate values to shades of red, orange, and yellow (in that order).

    >>>makeHotImage frog

    <https://raw.github.com/jcollard/unm-hip/master/examples/makehotimage.jpg>
 -}
makeHotImage :: GrayImage -> ColorImage
makeHotImage img = fmap (toHot max min) img where
  max = maxIntensity img
  min = minIntensity img
  toHot max min pixel = RGB (r, g, b) where
    px = (pixel - min)/(max-min)
    r = if px < 0.333333333 then (px*3.0) else 1.0
    g = if px < 0.333333333 then 0.0 else
          if px < 0.666666667 then (px - 0.333333333)*3 else 1.0
    b = if px < 0.666666667 then 0.0 else (px - 0.666666667)*3

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

    >>>realPart realPart . ifft $ (fft frogpart) * (fft d2g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart.jpg>
    
    >>>realPart realPart . ifft $ (fft frogpart) * (fft g)
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/realpart2.jpg>

-}
realPart :: ComplexImage -> GrayImage
realPart = CI.realPart

{-| Given a complex image, returns a real image representing
   the imaginary part of the image

   >>>let sine = imagPart signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/sine.jpg>

 -}
imagPart :: ComplexImage -> GrayImage
imagPart = CI.imagPart

{-| Given a complex image, returns a real image representing
    the magnitude of the image.

    >>>magnitude signal

 -}
magnitude :: ComplexImage -> GrayImage 
magnitude = CI.magnitude

{-| Given a complex image, returns a real image representing
    the angle of the image 
   
    >>>angle signal

    <https://raw.github.com/jcollard/unm-hip/master/examples/angle.jpg>
-}
angle :: ComplexImage -> GrayImage 
angle = CI.angle         

{-| Given a complex image, returns a pair of real images each
    representing the component (magnitude, phase) of the image

    >>>leftToRight' . complexImageToPolar $ signal
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/compleximagetopolar.jpg>
-}
complexImageToPolar :: ComplexImage -> (GrayImage, GrayImage)
complexImageToPolar = CI.complexImageToPolar

{-| Given an image representing the real part of a complex image, and
    an image representing the imaginary part of a complex image, returns
    a complex image.

    >>>complex cosine sine

    <https://raw.github.com/jcollard/unm-hip/master/examples/signal.jpg>
 -}
complex :: GrayImage -> GrayImage -> ComplexImage
complex = CI.complex

{-| Given a complex image, return a pair of real images each representing
    a component of the complex image (real, imaginary).

    >>>leftToRight' . complexImageToRectangular $ signal

    <https://raw.github.com/jcollard/unm-hip/master/examples/complexsignaltorectangular.jpg>
 -}
complexImageToRectangular :: ComplexImage -> (GrayImage, GrayImage)
complexImageToRectangular = CI.complexImageToRectangular

{-| Given a complex image and a real positive number x, shrink returns 
    a complex image with the same dimensions. Let z be the value of the 
    image at location (i, j). The value of the complex result image at 
    location (i, j) is zero if |z| < x, otherwise the result has the 
    same phase as z but the amplitude is decreased by x.
   
 -}
shrink :: (Num a,
           Image img,
           CI.ComplexPixel (Pixel img),
           CI.Value (Pixel img) ~ Double) => a -> img -> ComplexImage 
shrink = CI.shrink

{-| Given an image whose pixels can be converted to a complex value, 
    fft returns an image with complex pixels representing its 2D discrete 
    Fourier transform (DFT). Because the DFT is computed using the Fast Fourier 
    Transform (FFT) algorithm, the number of rows and columns of the image 
    must both be powers of two, i.e., 2K where K is an integer.

    >>>frog <- readImage "images/frog.pgm"
    >>>let frogpart = crop 64 64 128 128 frog
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/frog.jpg>
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/frogpart.jpg>

    >>>imageMap log . fft $ frogpart :: ComplexImage

    <https://raw.github.com/jcollard/unm-hip/master/examples/fft.jpg>  
    
    >>>fft d2g
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/fftd2g.jpg>  
    
    >>>fft g
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/fftg.jpg>      
-}
fft :: (Image img,
        CI.ComplexPixel (Pixel img),
        CI.Value (Pixel img) ~ Double) => img -> ComplexImage
fft = CI.fft

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
         CI.ComplexPixel (Pixel img),
         CI.Value (Pixel img) ~ Double) => img -> ComplexImage
ifft = CI.ifft


-- Binary Images

{-| Given a binary image, distanceTransform returns an image 
    representing the 2D distance transform of the image.
    The distance transform is accurate to within a 2% error for euclidean
     distance.

    >>>distanceTransform binaryStop :: GrayImage
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/distancetransform.jpg>
 -}
distanceTransform :: (Image img, 
                      BinaryPixel (Pixel img)) => img -> GrayImage
distanceTransform = Bin.distanceTransform

{-| Given a binary image, label returns an image where pixels in 
    distinct connected components (based on 4-neighbor connectivity) 
    have distinct integer values. These values range from 1 to n where 
    n is the number of connected components in image.

    >>> label binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/label.jpg>
 -}
label :: (Image img,
          BinaryPixel (Pixel img)) => img -> GrayImage
label = Bin.label

{-| Reads in a ASCII PGM image located at fileName as a GrayImage
    
    >>>frog <- readImage "images/frog.pgm"

    <https://raw.github.com/jcollard/unm-hip/master/examples/frog.jpg>
 -}
readImage :: FilePath -> IO GrayImage
readImage fileName = 
  do
    y <- B.readFile fileName
    return $ parseImage . B.intercalate (B.pack " ") . stripComments . B.lines $ y
    
parseImage :: B.ByteString -> GrayImage
parseImage string = img
  where ws = B.words string
        getInt = fst . fromJust . B.readInt
        px = map (fromIntegral . getInt) $ drop 4 ws
        cols = getInt $ ws !! 1
        rows = getInt $ ws !! 2
        maxi = fromIntegral . getInt $ ws !! 3
        img = Image rows cols (V.fromList px)

stripComments :: [B.ByteString] -> [B.ByteString]
stripComments xs = filter pred xs
  where pred x
          | B.null x = False
          | B.head x == '#' = False
          | otherwise = True