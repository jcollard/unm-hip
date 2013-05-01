{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances #-}
module Data.Image.IO(display,
                     writeImage,
                     toPGM,
                     toPPM,
                     setDisplayProgram,
                     module System.Process)  where

import Data.Image.Internal
import Data.Image.DisplayFormat
import Data.List(intercalate)
import System.IO
import System.Process

import System.IO.Unsafe
import Data.IORef

displayProgram :: IORef String
displayProgram = unsafePerformIO $ do
  dp <- newIORef "display"
  return dp

useStdin :: IORef Bool
useStdin = unsafePerformIO $ do
  usestdin <- newIORef True
  return usestdin

setDisplayProgram :: String -> Bool -> IO ()
setDisplayProgram program stdin = writeIORef displayProgram program >> writeIORef useStdin stdin

-- Run a command via the shell with the input given as stdin
runCommandWithStdIn :: String -> String -> IO (Handle, Handle, Handle, ProcessHandle)
runCommandWithStdIn cmd stdin =
  do
    ioval <- runInteractiveCommand cmd
    let stdInput = (\ (x, _, _, _) -> x) ioval
    hPutStr stdInput stdin
    hFlush stdInput
    hClose stdInput
    return ioval

-- Converts an image into a PGM string
toPGM :: (Image img, 
          Scaleable (Pixel img),
          RealFrac (Pixel img),
          MaxMin (Pixel img),
          Num (Pixel img),
          Show (Pixel img)) => img -> [Char]
toPGM img@(dimensions -> (rows, cols)) = "P2 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " . map (show . round . (255 `mult`)) . pixelList $ norm
  norm = normalize img

toPPM :: (Image img,
          MaxMin (Pixel img),
          Scaleable (Pixel img),
          Num (Pixel img),
          RGBPixel (Pixel img)) => img -> [Char]
toPPM img@(dimensions -> (rows, cols)) = "P3 " ++ (show cols) ++ " " ++ (show rows) ++ " 255 " ++ px where
  px = intercalate " " rgbs
  norm = normalize img
  rgbs = map (showRGB . scale . toRGB) . pixelList $ norm
  scale (r, g, b) = (255*r, 255*g, 255*b)
  showRGB (r, g, b) = (show . round $ r) ++ " " ++ (show . floor $ g) ++ " " ++ (show . floor $ b)

-- Displays an image using ImageMagick's display command
-- System must have ImageMagick installed for this to work
display :: (DisplayFormat df) => df -> IO (Handle, Handle, Handle, ProcessHandle)
display img = do
  usestdin <- readIORef useStdin
  display <- readIORef displayProgram
  if usestdin then runCommandWithStdIn display . format $ img
              else do
    writeImage ".tmp-img" img
    runInteractiveCommand (display ++ " .tmp-img")

writeImage :: (DisplayFormat df) => FilePath -> df -> IO ()
writeImage file = writeFile file . format 