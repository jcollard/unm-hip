{-# LANGUAGE TypeFamilies, ViewPatterns, FlexibleContexts, FlexibleInstances #-}
module Data.Image.Boxed(module Data.Image.Imageable,
                        GrayImage, 
                        GrayPixel,
                        RGBPixel,
                        readImage,
                        readRGBImage,
                        colorImageRed,
                        colorImageGreen,
                        colorImageBlue,
                        rgbToColorImage,
                        colorImageToRGB)
                        where

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

instance Maximal RGBPixel where
  maximal = maximal' zero where
    maximal' acc [] = acc
    maximal' (RGB r g b) ((RGB r' g' b'):xs) = maximal' acc' xs where
      acc' = RGB (max r r') (max g g') (max b b')

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

getColor :: ((Double, Double, Double) -> Double) -> RGBImage -> GrayImage
getColor color img@(dimensions -> (rows,cols)) = makeImage rows cols get where
  get r c = color . rgb $ ref img r c
  
colorImageRed :: RGBImage -> GrayImage
colorImageRed = getColor (\ (r, _, _) -> r)

colorImageGreen :: RGBImage -> GrayImage
colorImageGreen = getColor (\ (_,g,_) -> g)

colorImageBlue :: RGBImage -> GrayImage
colorImageBlue = getColor (\ (_,_,b) -> b)

rgbToColorImage :: GrayImage -> GrayImage -> GrayImage -> RGBImage
rgbToColorImage red green blue@(dimensions -> (rows, cols)) = makeImage rows cols colors where
  colors row col = RGB r g b where
    r = ref red row col
    g = ref green row col
    b = ref blue row col

colorImageToRGB :: RGBImage -> [GrayImage]
colorImageToRGB img= [colorImageRed img, colorImageGreen img, colorImageBlue img]


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