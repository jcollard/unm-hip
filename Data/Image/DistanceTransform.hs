{-# LANGUAGE ViewPatterns, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.DistanceTransform(distanceTransform) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

import Data.Image.Imageable


-- Performs a distance transform on the provided image. The provided
-- image is assumed to be binary such that all values greater than 0
-- are of equal foreground and all pixel values of 0 are background.
-- The distance transform is accurate to within a 2% error for euclidean
-- distance.
distanceTransform :: (Imageable img,
                      Pixel img ~ Double) => img -> img
distanceTransform img@(dimensions -> (rows, cols)) = makeImage rows cols func
  where arr  = getDistanceTransformArray img
        func r c = arr ! ((cols*r) + c)


-- Begin support code for distance transform
getDistanceTransformArray :: (Imageable img, 
                              Pixel img ~ Double) => img -> UArray Int Double
getDistanceTransformArray img@(dimensions -> (rows, cols)) = runSTUArray $ do
  let size = rows * cols
      imgdata = listArray (0,size-1) (pixelList img) :: UArray Int Double
      on   = 10000
  {- 
    Mask of distances to center pixel. The pixel being
    measured is placed beneath the 00 distance mask. The
    sum of all underlying pixels are added and the pixel being
    measured is set to be the minimum value. The areas
    marked with XX should be ignored. To accomplish this, we
    will set their values to be sufficiently high. Initially,
    all values of the binary image to be calculated are set sufficiently high.
    maskRight:          maskLeft:
    |--|--|--|--|--|    |--|--|--|--|--|
    |XX|11|XX|11|XX|    |XX|XX|00|05|XX|
    |--|--|--|--|--|    |--|--|--|--|--|
    |11|07|05|07|11|    |11|07|05|07|11|
    |--|--|--|--|--|    |--|--|--|--|--|
    |XX|05|00|XX|XX|    |XX|11|XX|11|XX|
    |--|--|--|--|--|    |--|--|--|--|--|
    
    This is the chamfer algorithm
  -}
  let maskRight = [on, 11, on, 11, on, 11, 7, 5, 7, 11, on, 5, 0, on, on]
  let maskLeft  = reverse maskRight
  dtImg <- newArray_ (0, size-1) :: ST s (STUArray s Int Double)
  forM_ [0..size-1] $ \ i -> do
    let val = if ((imgdata ! i) == 0) then 0 else on
    writeArray dtImg i val
  
  let pass rs cs tr br mask = do
        forM_ rs $ \ r -> do 
          forM_ cs $ \ c -> do
            let imgPixels = [ (i, j) | i <- [r+tr..r+br], j <- [c-2..c+2]]
            pxVals <- forM imgPixels $ \ (i, j) -> do
                        if (i < 0 || i > (rows-1) || j < 0 || j > (cols-1))
                          then return on
                          else do
                            val <- readArray dtImg ((i*cols)+j)
                            return val
            let sums = map (\ (x, y) -> x+y) $ zip pxVals mask
            let min = minimum sums
            writeArray dtImg ((r*cols) + c) min
  -- Pass from from left to right and top to bottom
  pass [0..rows-1] [0..cols-1] (-2) 0 maskRight
  -- Pass from right to left and bottom to top  
  pass [rows-1,rows-2..0] [cols-1,cols-2..0] 0 2 maskLeft
  
  return dtImg

-- End Distance Transform Support code
