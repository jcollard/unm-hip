{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances #-}
module Data.Image.IO(DisplayFormat(..),
                     GrayPixel(..),
                     RGBPixel(..),
                     writeImage,
                     toPGM,
                     toPPM,
                     module System.Process)  where

import Data.Image.Internal
import Data.List(intercalate)
import System.IO
import System.Process

import Data.IORef

class DisplayFormat df where
  format :: df -> String
  
class GrayPixel px where
  toGray :: px -> Double

class RGBPixel px where
  toRGB :: px -> (Double, Double, Double)


-- Converts an image into a PGM string
toPGM :: (Image img, 
          RealFrac (Pixel img),
          MaxMin (Pixel img),
          Num (Pixel img),
          Show (Pixel img)) => img -> [Char]
toPGM img@(dimensions -> (rows, cols)) = "P2 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " . map (show . round . (255 *)) . pixelList $ norm
  norm = normalize img

toPPM :: (Image img,
          MaxMin (Pixel img),
          Fractional (Pixel img),
          RGBPixel (Pixel img)) => img -> [Char]
toPPM img@(dimensions -> (rows, cols)) = "P3 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " rgbs
  norm = normalize img
  rgbs = map (showRGB . scale . toRGB) . pixelList $ norm
  scale (r, g, b) = (255*r, 255*g, 255*b)
  showRGB (r, g, b) = (show . round $ r) ++ " " ++ (show . floor $ g) ++ " " ++ (show . floor $ b)

writeImage :: (DisplayFormat df) => FilePath -> df -> IO ()
writeImage file = writeFile file . format 