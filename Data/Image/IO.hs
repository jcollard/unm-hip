{-# LANGUAGE TypeFamilies,  ViewPatterns, FlexibleContexts #-}
module Data.Image.IO(DisplayFormat(..),
                     GrayPixel(..),
                     RGBPixel(..),
                     writeImage,
                     toPGM,
                     toPPM)  where

import Data.Image.Internal

--base>=4
import Data.List(intercalate)

-- | A DisplayFormat for writing to a file
class DisplayFormat df where
  format :: df -> String
    
-- | GrayPixels will be converted using this class
class RealFrac (GrayVal px) => GrayPixel px where
  type GrayVal px :: *
  toGray :: px -> GrayVal px

-- | RGBPixels will be converted using this class
class RealFrac (ColorVal px) => RGBPixel px where
  type ColorVal px :: *
  toRGB :: px -> (ColorVal px, ColorVal px, ColorVal px)


-- Converts an image into a PGM string
-- | Converts an image an ASCII PPM scaled between pixel values of 0 and 255
toPGM :: (Image img, 
          GrayPixel (Pixel img)) => img -> [Char]
toPGM img@(dimensions -> (rows, cols)) = "P2 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " . map (show . round . (*scale) . (flip (-) min)) $ pixels
  pixels = map toGray . pixelList $ img
  min = minimum pixels
  max = maximum pixels
  scale = 255 / (max - min)

-- | Converts an image to an ASCII PPM scaled between pixel values of 0 and 255
toPPM :: (Image img,
          RGBPixel (Pixel img)) => img -> [Char]
toPPM img@(dimensions -> (rows, cols)) = "P3 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " rgbs
  rgbs = map (showRGB . scaleRGB) pixels
  pixels = map toRGB . pixelList $ img
  min = comp 10e10 min' pixels
  max = comp (-10e10) max' pixels
  scale = 255 / (max - min)
  scaleRGB (r, g, b) = (scale*(r-min), scale*(g-min), scale*(b-min))
  showRGB (r, g, b) = (show . round $ r) ++ " " ++ (show . floor $ g) ++ " " ++ (show . floor $ b)

min' :: RealFrac a => a -> a -> a
min' = comp' (<)

max' :: RealFrac a => a -> a -> a
max' = comp' (>)
  
comp' :: RealFrac a => (a -> a -> Bool) -> a -> a -> a
comp' f d0 d1
  | f d0 d1 = d0
  | otherwise = d1

comp :: RealFrac a => a -> (a -> a -> a) -> [(a, a, a)] -> a
comp seed f = compare' (seed,seed,seed) where
  compare' (r,g,b) [] =  foldr1 f [r,g,b]
  compare' (r,g,b) ((r',g',b'):xs) = compare' (f r r', f g g', f b b') xs

{-| Given a file name and a formatable image, writes the image to that file
    with the format.
 -}
writeImage :: (DisplayFormat df) => FilePath -> df -> IO ()
writeImage file = writeFile file . format 