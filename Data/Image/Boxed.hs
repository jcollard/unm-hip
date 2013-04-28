{-# LANGUAGE TypeFamilies, ViewPatterns, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Boxed(module Data.Image.Imageable,
                        GrayImage, 
                        GrayPixel,
                        ComplexImage,
                        RGBPixel,
                        HSIPixel,
                        readImage,
                        readRGBImage,
                        colorImageRed,
                        colorImageGreen,
                        colorImageBlue,
                        colorImageHue,
                        colorImageSaturation,
                        colorImageIntensity,
                        rgbToColorImage,
                        hsiToColorImage,
                        rgbList,
                        hsiList,
                        shrink,
                        hsiToRGB,
                        rgbToHSI,
                        complexToRGB,
                        realPart',
                        imagPart',
                        magnitude',
                        angle',
                        polar',
                        complexImageToRectangular',
                        fft, ifft)
                        where

import qualified Data.Complex as C
import qualified Data.Image.FFT as FFT
import Data.Image.IO
import Data.Image.DisplayFormat
import Data.Image.Imageable
import qualified Data.ByteString.Char8 as B
import Data.Maybe(fromJust)
import qualified Data.Vector as V

type Vector = V.Vector

data Image a = Image { rs :: Int,
                        cs :: Int,
                        pixels :: Vector a} 

instance Functor Image where
  fmap f i = Image (rows i) (cols i) (fmap f . pixels $ i)

type HSIImage = Image HSIPixel

type ComplexImage = Image (C.Complex Double)

type GrayImage = Image GrayPixel

type GrayPixel = Double

instance Imageable (Image a) where
  type Pixel (Image a) = a
  rows = rs
  cols = cs
  ref i r c = (pixels i) V.! (r * (cols i) + c)
  makeImage rows cols f = Image rows cols (V.fromList px) where
    px = [ f r c | r <- [0..rows-1], c <- [0..cols-1]]
  pixelList = V.toList . pixels

instance DisplayFormat (Image GrayPixel) where
  format = toPGM

instance Show (Image a) where
  show (Image rows cols _) = "< Image " ++ (show rows) ++ "x" ++ (show cols) ++ " >"

instance (Num a, 
          Ord a) => Num (Image a) where
  (+) = imageOp (+)
  (-) = imageOp (-)
  (*) = imageOp (*)
  abs i = Image (rows i) (cols i) (V.map abs . pixels $ i)
  signum i = Image (rows i) (cols i) (V.map signum . pixels $ i)
  fromInteger = undefined

type RGBImage = Image RGBPixel

data RGBPixel = RGB Double Double Double deriving (Eq, Show)

instance DisplayFormat (Image RGBPixel) where
  format = toPPM

instance Zero RGBPixel where
  zero = RGB 0.0 0.0 0.0

instance MaxMin RGBPixel where
  maximal = helper max zero
  minimal = helper min zero
  
instance Divisible RGBPixel where
  divide f (RGB r g b) = RGB r' g' b' where
    f' = fromIntegral f
    (r', g', b') = (f'/r, f'/g, f'/b)

helper :: (Double -> Double -> Double) -> RGBPixel -> [RGBPixel] -> RGBPixel
helper _ acc [] = acc
helper compare (RGB r g b) ((RGB r' g' b'):xs) = helper compare acc' xs where
  acc' = RGB (compare r r') (compare g g') (compare b b')

instance RGB RGBPixel where
  rgb (RGB r g b) = (r, g, b)
  
instance Num RGBPixel where 
  (+) = rgbOp (+)
  (-) = rgbOp (-)
  (*) = rgbOp (*)
  abs (RGB r g b) = RGB (abs r) (abs g) (abs b)
  signum (RGB r g b) = RGB (signum r) (signum g) (signum b)
  fromInteger = undefined

rgbOp :: (Double -> Double -> Double) -> RGBPixel -> RGBPixel -> RGBPixel
rgbOp op (RGB a b c) (RGB d e f) = RGB (op a d) (op b e) (op c f)

data HSIPixel = HSI Double Double Double

instance DisplayFormat ComplexImage where
  format (complexToRGB -> rgb) = toPPM rgb

complexToRGB :: ComplexImage -> RGBImage
complexToRGB img@(dimensions -> (rows, cols)) = makeImage rows cols rgb where
  scale = complexScale img
  rgb row col = if radius < 1 then RGB red' grn' blu' else RGB red grn blu where
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
complexScale (complexImageToRectangular -> [real, imag]) = 2.0/(maxv - minv) where
    maxr = maximum . pixelList $ (real :: GrayImage)
    maxi = maximum . pixelList $ imag
    minr = minimum . pixelList $ real
    mini = minimum . pixelList $ imag
    maxv = max maxr maxi
    minv = min minr mini
    
twodivsqrtsix = 0.81649658092772603273 --2.0/sqrt(6)
onedivsqrtsix = 0.40824829046386301636 --1.0/sqrt(6)
sqrttwodivtwo = 0.70710678118654752440 --sqrt(2)/2.0

instance Num HSIPixel where
  (+) = hsiOp (+)
  (-) = hsiOp (-)
  (*) = hsiOp (*)
  abs (HSI r g b) = HSI (abs r) (abs g) (abs b)
  signum (HSI r g b) = HSI (signum r) (signum g) (signum b)
  fromInteger = undefined

hsiOp :: (Double -> Double -> Double) -> HSIPixel -> HSIPixel -> HSIPixel
hsiOp op (HSI a b c) (HSI d e f) = HSI (op a d) (op b e) (op c f)

hsiToRGB :: Image HSIPixel -> Image RGBPixel
hsiToRGB img@(dimensions -> (rows, cols)) = makeImage rows cols rgb where
  rgb r c = RGB red grn blu where
    (HSI h s i) = (ref img r c)
    red = i + v1
    grn = i - (v1/2) + v2
    blu = i - (v1/2) - v2
    v1 = const*s*(cos h)/3
    v2 = const*s*(sin h)/2
    const = 2.44948974278318

rgbToHSI :: Image RGBPixel -> Image HSIPixel
rgbToHSI img@(dimensions -> (rows, cols)) = makeImage rows cols hsi where
  hsi r c = HSI h s i where
    h = if (v1 /= 0.0) then atan2 v2 v1 else 0
    s = sqrt( (v1*v1) + (v2*v2) )
    i = (r'+g'+b')/3
    (RGB r' g' b') = (ref img r c)
    v1 = (2.0*r'-g'-b') / const
    v2 = (g' - b') / const
    const = 2.44948974278318
  
getColor :: ((Double, Double, Double) -> Double) -> RGBImage -> GrayImage
getColor color img@(dimensions -> (rows,cols)) = makeImage rows cols get where
  get r c = color . rgb $ ref img r c
  
getColor' :: ((Double, Double, Double) -> Double) -> Image HSIPixel -> GrayImage
getColor' color img@(dimensions -> (rows,cols)) = makeImage rows cols get where
  get r c = color . (\ (HSI h s i) -> (h, s, i)) $ ref img r c
  
colorImageRed :: RGBImage -> GrayImage
colorImageRed = getColor (\ (r, _, _) -> r)

colorImageGreen :: RGBImage -> GrayImage
colorImageGreen = getColor (\ (_,g,_) -> g)

colorImageBlue :: RGBImage -> GrayImage
colorImageBlue = getColor (\ (_,_,b) -> b)

colorImageHue :: Image HSIPixel -> GrayImage
colorImageHue = getColor' (\ (h,_,_) -> h)

colorImageSaturation :: Image HSIPixel -> GrayImage
colorImageSaturation = getColor' (\ (_,s,_) -> s)

colorImageIntensity :: Image HSIPixel -> GrayImage
colorImageIntensity = getColor' (\ (_,_,i) -> i)

rgbToColorImage :: GrayImage -> GrayImage -> GrayImage -> RGBImage
rgbToColorImage red green blue@(dimensions -> (rows, cols)) = makeImage rows cols colors where
  colors row col = RGB r g b where
    r = ref red row col
    g = ref green row col
    b = ref blue row col

hsiToColorImage :: GrayImage -> GrayImage -> GrayImage -> Image HSIPixel
hsiToColorImage h s i@(dimensions -> (rows, cols)) = makeImage rows cols colors where
  colors r c = HSI h' s' i' where
    h' = ref h r c
    s' = ref s r c
    i' = ref i r c

hsiList :: Image HSIPixel -> [GrayImage]
hsiList img = [colorImageHue img, colorImageSaturation img, colorImageIntensity img]

rgbList :: RGBImage -> [GrayImage]
rgbList img= [colorImageRed img, colorImageGreen img, colorImageBlue img]

shrink :: (Integral a) => a -> GrayImage -> GrayImage
shrink (fromIntegral -> x) img@(dimensions -> (rows, cols)) = makeImage rows cols shrink' where
  shrink' r c = let z = abs (ref img r c) in helper z where
    helper z 
      | z < x = zero
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

polar' :: ComplexImage -> [GrayImage]
polar' = polar

complexImageToRectangular' :: ComplexImage -> [GrayImage]
complexImageToRectangular' = complexImageToRectangular

readRGBImage :: FilePath -> IO RGBImage
readRGBImage fileName =
  do
    y <- B.readFile fileName
    return $ parseRGBImage . B.intercalate (B.pack " ") . stripComments . B.lines $ y
    
parseRGBImage :: B.ByteString -> RGBImage
parseRGBImage string = Image rows cols (V.fromList rgbs)
  where ws = B.words string
        getInt = fst. fromJust . B.readInt
        px = map (fromIntegral . getInt) $ drop 4 ws
        cols = getInt $ ws !! 1
        rows = getInt $ ws !! 2
        maxi = fromIntegral . getInt $ ws !! 3
        [r, g, b] = colors px
        rgbs = map rgb3 . zip3 r g $ b
        rgb3 (r, g, b) = RGB r g b

colors :: [Int] -> [[GrayPixel]]
colors xs = helper xs [] [] []
  where helper [] red green blue = map (map fromIntegral) $ map reverse [red, green, blue]
        helper (r:g:b:cs) red green blue = helper cs (r:red) (g:green) (b:blue)

toRGB :: GrayImage -> RGBImage
toRGB img@(dimensions -> (rows, cols)) = makeImage rows cols rgb where
  rgb r c = RGB p p p where
    p = ref img r c

toComplex :: GrayImage -> ComplexImage
toComplex img@(dimensions -> (rows, cols)) = makeImage rows cols toComp where
  toComp r c = (ref img r c ) C.:+ 0.0

fft :: ComplexImage -> ComplexImage
fft (Image rows cols vec) = Image rows cols (FFT.fft rows cols vec)

ifft :: ComplexImage -> ComplexImage
ifft (Image rows cols vec) = Image rows cols (FFT.ifft rows cols vec) 

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