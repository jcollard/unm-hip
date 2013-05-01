{-# LANGUAGE TypeFamilies, ViewPatterns, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Boxed(
  -- | Contains functionality related to Binary Images
  module Data.Image.Binary, 
  -- | Contains functionality related to Complex Images
  module Data.Image.Complex,
  -- | Contains functionality for convolution of images
  module Data.Image.Convolution,
  -- | Contains basic functionality for Images
  module Data.Image.Internal,
  -- | Contains functionality for writing images and displaying with an external program
  module Data.Image.IO,
  GrayImage, Gray, readImage, 
  shrink, grayToComplex, makeHotImage,
  ColorImage, Color, readColorImage,
  colorImageRed, colorImageGreen, colorImageBlue,
  colorImageToRGB, 
  colorImageToHSI,
  rgbToColorImage, 
  hsiToColorImage, 
  ComplexImage,
  complexImageToColorImage,
  realPart', imagPart', magnitude',
  angle', polar', complexImageToRectangular') where
 
import Data.Image.Binary
import Data.Image.Complex
import Data.Image.Convolution
import Data.Image.Internal
import Data.Image.IO

import Control.Applicative
import qualified Data.Complex as C
import qualified Data.ByteString.Char8 as B
import Data.Maybe(fromJust)
import Data.Monoid
import qualified Data.Vector as V

type Vector = V.Vector

-- BoxedImage
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
       partials = map (partial V.!) indices
       values = map (toApply V.!) indices
       applied = zipWith ($) partials values

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

-- GrayImage
type GrayImage = BoxedImage Gray
type Gray = Double

instance DisplayFormat GrayImage where
  format = toPGM

instance GrayPixel Gray where
  toGray = id

instance RGBPixel Gray where
  toRGB px = (px, px, px)

instance HSIPixel Gray where
  toHSI = toHSI . RGB . toRGB
  
instance BinaryPixel Gray where
  toBinary 0.0 = False
  toBinary _ = True
  on = 1.0
  off = 0.0
  
instance ComplexPixel Gray where
  toComplex i =  i C.:+ 0.0

instance Monoid Gray where
  mempty = 0.0
  mappend = (+)
  
instance MaxMin Gray where
  maximal = maximum
  minimal = minimum

-- ColorImage
class HSIPixel px where
  toHSI :: px -> (Double, Double, Double)

type ColorImage = BoxedImage Color

instance DisplayFormat ColorImage where
  format = toPPM

data Color = RGB (Double, Double, Double)
           | HSI (Double, Double, Double) deriving (Show, Eq)

instance GrayPixel Color where
  toGray (RGB (r, g, b)) = (r + g + b) / 3.0
  toGray (toRGB -> (r, g, b)) = (r + g + b) / 3.0

instance RGBPixel Color where
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
type ComplexImage = BoxedImage CPixel
type CPixel = C.Complex Double

instance DisplayFormat ComplexImage where
  format (complexImageToColorImage -> rgb) = toPPM rgb

instance ComplexPixel CPixel where
  toComplex = id

complexImageToColorImage :: ComplexImage -> ColorImage
complexImageToColorImage img@(dimensions -> (rows, cols)) = makeImage rows cols rgb where
  scale = complexScale img
  rgb row col = if radius < 1 then RGB (red', grn', blu') else RGB (red, grn, blu) where
    [red, grn, blu] = map (+d') [r',g',b']
    [red', grn', blu'] = map (flip (-) d')  [r',g',b']
    comp = ref img row col
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
complexScale (complexImageToRectangular -> (real, imag)) = 2.0/(maxv - minv) where
    maxr = maximum . pixelList $ (real :: GrayImage)
    maxi = maximum . pixelList $ imag
    minr = minimum . pixelList $ real
    mini = minimum . pixelList $ imag
    maxv = max maxr maxi
    minv = min minr mini
    
twodivsqrtsix = 0.81649658092772603273 --2.0/sqrt(6)
onedivsqrtsix = 0.40824829046386301636 --1.0/sqrt(6)
sqrttwodivtwo = 0.70710678118654752440 --sqrt(2)/2.0

hsiToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage
hsiToColorImage (h@(dimensions -> (rows, cols)), s, i) = makeImage rows cols hsi where
  hsi r c = HSI (h', s', i') where
    ref' img = ref img r c
    h' = ref h r c
    s' = ref' s
    i' = ref' i

colorImageToHSI :: ColorImage -> (GrayImage, GrayImage, GrayImage)
colorImageToHSI img = (colorImageHue img, colorImageSaturation img, colorImageIntensity img) 
  
getColor to color img@(dimensions -> (rows,cols)) = makeImage rows cols get where
  get r c = color . to . ref img r $ c
  
getRGB = getColor toRGB

colorImageRed :: ColorImage -> GrayImage
colorImageRed = getRGB (\ (r, _, _) -> r)

colorImageGreen :: ColorImage -> GrayImage
colorImageGreen = getRGB (\ (_,g,_) -> g)

colorImageBlue :: ColorImage -> GrayImage
colorImageBlue = getRGB (\ (_,_,b) -> b)

getHSI = getColor toHSI

colorImageHue :: ColorImage -> GrayImage
colorImageHue = getHSI (\ (h, _, _) -> h)

colorImageSaturation :: ColorImage -> GrayImage
colorImageSaturation = getHSI (\ (_,s,_) -> s)

colorImageIntensity :: ColorImage -> GrayImage
colorImageIntensity = getHSI (\ (_,_,i) -> i)

rgbToColorImage :: (GrayImage, GrayImage, GrayImage) -> ColorImage
rgbToColorImage (red, green, blue@(dimensions -> (rows, cols))) = makeImage rows cols colors where
  colors row col = RGB (r, g, b) where
    r = ref red row col
    g = ref green row col
    b = ref blue row col

colorImageToRGB :: ColorImage -> (GrayImage, GrayImage, GrayImage)
colorImageToRGB img = (colorImageRed img, colorImageGreen img, colorImageBlue img)

shrink :: (Integral a) => a -> GrayImage -> GrayImage
shrink (fromIntegral -> x) img@(dimensions -> (rows, cols)) = makeImage rows cols shrink' where
  shrink' r c = let z = abs (ref img r c) in helper z where
    helper z 
      | z < x = mempty
      | z > 0 = z - x 
      | otherwise = z  + x

realPart' :: ComplexImage -> GrayImage
realPart' = realPart

imagPart' :: ComplexImage -> GrayImage
imagPart' = imagPart

magnitude' :: ComplexImage -> GrayImage
magnitude' = magnitude

angle' :: ComplexImage -> GrayImage
angle' = angle

polar' :: ComplexImage -> (GrayImage, GrayImage)
polar' = polar

complexImageToRectangular' :: ComplexImage -> (GrayImage, GrayImage) 
complexImageToRectangular' = complexImageToRectangular

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

grayToComplex :: GrayImage -> ComplexImage
grayToComplex img@(dimensions -> (rows, cols)) = makeImage rows cols toComp where
  toComp r c = (ref img r c ) C.:+ 0.0

makeHotImage :: GrayImage -> ColorImage
makeHotImage img@(dimensions -> (rows, cols)) = makeImage rows cols hot where
  min = minIntensity img
  max = maxIntensity img
  hot r c = RGB (r', g', b') where
    px = ((ref img r c) - min)/(max-min)
    r' = if px < 0.333333333 then (px*3.0) else 1.0
    g' = if px < 0.333333333 then 0.0 else
          if px < 0.666666667 then (px - 0.333333333)*3 else 1.0
    b' = if px < 0.666666667 then 0.0 else (px - 0.666666667)*3
  

-- Reads in a PGM image located at fileName
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