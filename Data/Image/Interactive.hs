module Data.Image.Interactive(display, 
                              setDisplayProgram,
                              plotHistograms) where

import Data.Image.IO
import Data.Image.Binary(areas)
import Data.Image.Internal

--base>=4
import Data.List(intercalate)
import Data.IORef
import System.IO.Unsafe
import System.IO

--vector>=0.10.0.2
import qualified Data.Vector.Unboxed as V

--process>=1.1.0.2
import System.Process

{-| Sets the program to use when making a call to display and specifies if
    the program can accept an image via stdin. If it cannot, then a temporary
    file will be created and passed as an argument instead. By default,
    ImageMagick ("display") is the default program to use and it is read
    using stdin.
 -}
setDisplayProgram :: String -> Bool -> IO ()
setDisplayProgram program stdin = writeIORef displayProgram program >> writeIORef useStdin stdin


{-| Makes a call to the current display program to be displayed. If the
    program cannot read from standard in, a file named ".tmp-img" is created
    and used as an argument to the program.
 -}
display :: (DisplayFormat df) => df -> IO (Handle, Handle, Handle, ProcessHandle)
display img = do
  usestdin <- readIORef useStdin
  display <- readIORef displayProgram
  if usestdin then runCommandWithStdIn display . format $ img
              else do
    writeImage ".tmp-img" img
    runInteractiveCommand (display ++ " .tmp-img")

displayProgram :: IORef String
displayProgram = unsafePerformIO $ do
  dp <- newIORef "display"
  return dp

useStdin :: IORef Bool
useStdin = unsafePerformIO $ do
  usestdin <- newIORef True
  return usestdin
  
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

{-| Takes a list, pair, or triple of images and passes them to 
    gnuplot to be displayed as histograms.
 -}
plotHistograms images = runCommandWithStdIn "gnuplot -persist"  $ input
  where input = intercalate "\n" [plotCommand datas max, histogramList, "exit"]
        datas = [ "data" ++ (show x) | x <- [0..n]]
        n = length . toList $ images
        histogramList = intercalate "\ne" . map histogramString . toList $ images
        max = floor . maximum . map maxIntensity . toList $ images

-- Creates a plot command for gnuplot containing a title for each of the titles
-- and sets the width of the plot to be from 0 to max
plotCommand :: [String] -> Int -> String
plotCommand titles max = "plot [0:" ++ (show max) ++ "]" ++ (intercalate "," $ vals)
  where vals = [ " '-' title '" ++ title ++ "' with lines" | title <- titles]

-- Takes an image and creates a gnuplot command of the points for the
-- histogram.
histogramString img = intercalate "\n" $ map show (V.toList arr)
  where arr = areas img