{-# LANGUAGE ViewPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.Binary(-- * Binary Images
                         BinaryPixel(..),
                         toBinaryImage,
                         (<.), (.<),
                         (>.), (.>),
                         (==.), (.==),
                         (/=.), (./=),
                         compareImage,
                         (.<.), (.>.), (.==.), (./=.),
                         -- * Binary Morphology
                         erode, erode',
                         dilate, dilate',
                         open, open',
                         close, close',
                         -- * Functions on Binary Images
                         label,
                         areas, 
                         perimeters,
                         boundingBoxes,
                         centersOfMass,                             
                         distanceTransform,
                         outline, outline') where

--base>=4
import Control.Monad
import Control.Monad.ST
import Data.STRef

--containers>=0.5.0.0
import qualified Data.Map as M

--vector>=0.10.0.1
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as BVM

import Data.Image.Internal
import Data.Image.Convolution

-- | A BinaryPixel can be in one of two states, on or off.
class BinaryPixel px where
  toBinary :: px -> Bool
  off :: px
  on  :: px

{-| Given a function of a pixel to a boolean and an image, returns the Binary
    version of that image.

    >>>stop <- readColorImage "images/stop.ppm"
    >>>let binaryStop = toBinaryImage (\(RGB (r, g, b)) -> r > 196 && g > 0 && b > 0) stop

    <https://raw.github.com/jcollard/unm-hip/master/examples/stop.jpg>

    <https://raw.github.com/jcollard/unm-hip/master/examples/binarystop.jpg>
 -}
toBinaryImage :: (Image img,
                  BinaryPixel (Pixel img)) => (Pixel img -> Bool) -> img -> img
toBinaryImage pred img@(dimensions -> (rows, cols)) = makeImage rows cols bin where
  bin r c = if pred (ref img r c) then on else off

{-| Given an image, img, and a Pixel p, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is less
    than p and off otherwise.

    >>>frog <- readImage "images/frog.pgm"
    >>>frog .< 50
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/lessthanfrog.jpg>
 -}
(.<) :: (Image img, 
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.<) img num = toBinaryImage pred img where
  pred p = p < num

{-| Given a Pixel p and an image img, return a Binary image where the
    pixel at (i, j) is on if p is less than the corresponding pixel in 
    img at (i,j) and off otherwise.

    >>>frog <. 50
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/greaterthanfrog.jpg>
 -}
(<.) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(<.) = flip (.>)


{-| Given an image, img, and a Pixel p, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is greater
    than p and off otherwise.
    
    >>>50 <. frog
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/greaterthanfrog.jpg>
 -}
(.>) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => img -> a -> img
(.>) img num = toBinaryImage pred img where
  pred p = p >  num
  
{-| Given a Pixel p and an image img, return a Binary image where the
    pixel at (i, j) is on if p is greater than the corresponding pixel in 
    img at (i,j) and off otherwise.

    >>>50 >. frog
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/lessthanfrog.jpg>

 -}
(>.) :: (Image img,
         BinaryPixel (Pixel img),
         Ord (Pixel img),
         Pixel img ~ a) => a -> img -> img
(>.) = flip (.<)


{-| Given an image, img, and a Pixel p, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is equal
    to p and off otherwise.

    >>>frog .== 50
    < Image 225x242 >
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/frogequals.jpg>
-}
(.==) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => img -> a -> img
(.==) img num = toBinaryImage pred img where
  pred p = p == num

{-| Given a Pixel p and an image img, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is equal
    to p and off otherwise.

    >>>50 ==. frog
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/frogequals.jpg>
 -}
(==.) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => a -> img -> img
(==.) = flip (.==)

{-| Given an image, img, and a Pixel p, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is equal
    to p and off otherwise.

    >>>frog ./= 50
    < Image 225x242 >
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/notequals.jpg>
-}
(./=) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => img -> a -> img
(./=) img num = toBinaryImage pred img where
  pred p = p /= num

{-| Given a Pixel p and an image img, return a Binary image where the
    pixel at (i, j) is on if the corresponding pixel in img at (i,j) is equal
    to p and off otherwise.

    >>>50 /=. frog
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/notequals.jpg>
 -}
(/=.) :: (Image img,
          BinaryPixel (Pixel img),
          Eq (Pixel img),
          Pixel img ~ a) => a -> img -> img
(/=.) = flip (./=)

{-| Given a function of two pixels to a boolean pred  and two images X and Y, 
    return a binary image that for each pixel (i,j) is on if the pixel 
    pred X(i,j) Y(i,j) return True and off otherwise.

    >>>let fade = makeImage 225 242 (\ r _ -> fromIntegral r) :: GrayImage
    >>>let fademask = compareImage (>) frog fade

    <https://raw.github.com/jcollard/unm-hip/master/examples/fade.jpg>
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/fademask.jpg>
-}
compareImage :: (Image img,
                 BinaryPixel (Pixel img),
                 Ord (Pixel img)) => ((Pixel img) -> (Pixel img) -> Bool) -> img -> img -> img
compareImage comp img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols bin where
  bin r c = if p0 `comp` p1 then on else off where
    p0 = ref img0 r c
    p1 = ref img1 r c

{-| Given two images X and Y, return a binary image that for each pixel (i, j)
    is on if X(i,j) > Y(i,j) and off otherwise.

    >>>let fademask = frog .>. fade

    <https://raw.github.com/jcollard/unm-hip/master/examples/fademask.jpg>
 -}
(.>.) :: (Image img,
          BinaryPixel (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.>.) = compareImage (>)
  
{-| Given two images X and Y, return a binary image that for each pixel (i, j)
    is on if X(i,j) < Y(i,j) and off otherwise.

    >>>let fademask2 = frog .<. fade

    <https://raw.github.com/jcollard/unm-hip/master/examples/fademask2.jpg>
 -}
(.<.) :: (Image img,
          BinaryPixel (Pixel img),
          Ord (Pixel img)) => img -> img -> img
(.<.) = compareImage (<)

{-| Given two images X and Y, return a binray image that for each pixel (i, j)
    is on if X(i,j) == Y(i,j) and off otherwise.

    >>>let fademask3 = frog .==. fade

    <https://raw.github.com/jcollard/unm-hip/master/examples/fademask3.jpg>
 -}
(.==.) :: (Image img,
           BinaryPixel (Pixel img),
           Eq (Pixel img)) => img -> img -> img
(.==.) img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols img where
  img r c = if p0 == p1 then on else off where
    p0 = ref img0 r c
    p1 = ref img1 r c

{-| Given two images X and Y, return a bniry image that for each pixel (i, j)
    is on if X(i,j) == Y(i,j) and off otherwise.

    >>>let fademask4 = frog ./=. fade

    <https://raw.github.com/jcollard/unm-hip/master/examples/fademask4.jpg>
 -}
(./=.) :: (Image img,
           BinaryPixel (Pixel img),
           Eq (Pixel img)) => img -> img -> img
(./=.) img0@(dimensions -> (rows, cols)) img1 = makeImage rows cols img where
  img r c = if p0 /= p1 then on else off where
    p0 = ref img0 r c
    p1 = ref img1 r c

{-| Given a 2D list consisting solely of pixels representing a structuring 
    element, and a binary image, erode returns the morphological erosion of 
    the <image> with the structuring element. 

    >>>stop <- readColorImage "images/stop.ppm"
    >>>let binaryStop = toBinaryImage (\(RGB (r, g, b)) -> r > 196 && g > 0 && b > 0) stop
    >>>let erosion = erode [[1,1],[1,1]] binaryStop
    >>>binaryStop - erosion
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/stop.jpg>
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/binarystop.jpg>
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/erode.jpg>

    <https://raw.github.com/jcollard/unm-hip/master/examples/erosion.jpg>
-}
erode :: (Image img,
           BinaryPixel (Pixel img),
           Num (Pixel img),
           Eq (Pixel img)) => [[Pixel img]] -> img -> img
erode ls img = (convolve ls img) .== (sum . concat $ ls)

{-| For convenience erode' = erode [[1,1],[1,1]]
    
    >>>erode' binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/erode.jpg>
-}
erode' :: (Image img,
          BinaryPixel (Pixel img),
          Num (Pixel img),
          Eq (Pixel img)) => img -> img
erode' = erode [[1,1],[1,1]]

{-| Given a 2D list consisting solely of pixels representing a structuring 
    element, and a binary image, dilate returns the morphological dilation of 
    the <image> with the structuring element. 

    >>>let dilated = dilate [[1,1],[1,1]] binaryStop
    >>>dilate - binaryStop
    < Image 86x159 >
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/dilate.jpg>

    <https://raw.github.com/jcollard/unm-hip/master/examples/dilated.jpg>
 -}
dilate :: (Image img,
           BinaryPixel (Pixel img),
           Num (Pixel img),
           Eq (Pixel img)) => [[Pixel img]] -> img -> img
dilate ls img = toBinaryImage (0 /=) (convolve ls img)

{-| For convenience dilate' = dilate [[1,1],[1,1]]

    >>>dilate' binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/dilate.jpg>
 -}
dilate' :: (Image img,
           BinaryPixel (Pixel img),
           Num (Pixel img),
           Eq (Pixel img)) => img -> img
dilate' = dilate [[1,1],[1,1]]

{-| Given a 2D list consisting solely of pixels representing a structuring 
    element, and a binary image, dilate returns the morphological opening of 
    the <image> with the structuring element. 

    >>>noise <- readColorImage "images/noise.ppm"
    >>>let noisyStop = binaryStop ./=. noise
    >>>open noisyStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/noise.jpg>
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/noisyStop.jpg>

    <https://raw.github.com/jcollard/unm-hip/master/examples/open.jpg>
-}
open :: (Image img,
          BinaryPixel (Pixel img),
          Num (Pixel img),
          Eq (Pixel img)) => [[Pixel img]] -> img -> img
open ls = dilate ls . erode ls

{-| For convenience open' = open [[1,1],[1,1]]
 
    >>>open' noisyStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/open.jpg>
-}
open' :: (Image img,
         BinaryPixel (Pixel img),
         Num (Pixel img),
         Eq (Pixel img)) => img -> img
open' = dilate' . erode'


{-| Given a 2D list consisting solely of pixels representing a structuring 
    element, and a binary image, dilate returns the morphological closing of 
    the <image> with the structuring element. 

    >>>close [[1,1],[1,1]] noisyStop

    <https://raw.github.com/jcollard/unm-hip/master/examples/close.jpg>
 -}
close :: (Image img,
           BinaryPixel (Pixel img),
           Num (Pixel img),
           Eq (Pixel img)) => [[Pixel img]] -> img -> img
close ls = erode ls . dilate ls

{-| For convenience close' = close [[1,1],[1,1]]
    
    >>>close' noisyStop

    <https://raw.github.com/jcollard/unm-hip/master/examples/close.jpg>
 -}
close' :: (Image img,
          BinaryPixel (Pixel img),
          Num (Pixel img),
          Eq (Pixel img)) => img -> img
close' = erode' . dilate'

{-| Given a binary image, label returns an image where pixels in 
    distinct connected components (based on 4-neighbor connectivity) 
    have distinct integer values. These values range from 1 to n where 
    n is the number of connected components in image.

    >>> label binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/label.jpg>
 -}
label :: (Image img,
          BinaryPixel (Pixel img),
          Image img',
          Pixel img' ~ Double) => img -> img'
label img@(dimensions -> (rows, cols)) = makeImage rows cols labels where
  labels r c = arr V.! (r*cols+c)
  arr = getLabels img :: V.Vector Double
  
getLabels img@(dimensions -> (rows, cols)) = runST $ do 
  currentLabel <- newSTRef 1 :: ST s (STRef s Int)
  equivalences <- newSTRef M.empty :: ST s (STRef s (M.Map Double Double))
  labels <- VM.replicate (rows*cols) 0 :: ST s (VM.STVector s Double)
  
  --Label Groups
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1] ] (\ (r, c) -> do
    if toBinary . ref img r $ c then do
      smn <- neighbors labels rows cols r c
      if (smn == []) then newLabel labels currentLabel cols r c equivalences
        else writeLabel labels cols r c smn equivalences
      else return ())
  
  --Relabel with Lowest Equivalence
  eq <- readSTRef equivalences
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1] ] (\ (r, c) -> do
    if toBinary . ref img r $ c then do
      currLabel <- VM.read labels (r*cols + c)
      let newLabel = (eq M.! currLabel)
      VM.write labels (r*cols + c) newLabel
      else return ())
  V.freeze labels

writeLabel labels cols r c smn equiv = do
  oldMap <- readSTRef equiv
  let min = minimum smn
      insert k m = M.insert k min m
      newMap = foldr insert oldMap smn
  VM.write labels (r*cols + c) min
  writeSTRef equiv newMap

newLabel labels currentLabel cols r c equivalences = do
  l <- readSTRef currentLabel
  modifySTRef currentLabel (+1)
  VM.write labels (cols*r + c) (fromIntegral l)
  eq <- readSTRef equivalences
  let newMap = M.insert (fromIntegral l) (fromIntegral l) eq
  writeSTRef equivalences newMap

neighbors :: VM.STVector s Double -> Int -> Int -> Int -> Int -> ST s [Double]
neighbors labels rows cols r c = do
  let n' = neighbor labels rows cols
  center <- n' r c
  north <- n' (r-1) c
  ne <- n' (r-1) (c+1)
  east <- n' r (c+1)
  se <- n' (r+1) (c+1)
  south <- n' (r+1) c
  sw <- n' (r+1) (c-1)
  west <- n' r (c-1)
  nw <- n' (r-1) (c-1)
  return $ filter (> 0) [center, north, ne, east, se, south, sw, west, nw]

neighbor :: VM.STVector s Double -> Int -> Int -> Int -> Int -> ST s Double
neighbor labels rows cols r c
  | r >= 0 && r < rows && c >= 0 && c < cols = VM.read labels (r*cols + c)
  | otherwise = return 0.0

{-| Given an image, areas returns a vector where the n-th component equals 
    the number of pixels with value n. If image is the result of applying 
    label to a binary image, then the vector represents the areas of the 
    connected-components of the binary-image. If not, areas returns 
    the histogram of the image.

    >>> areas . label $ binaryStop
    fromList [9241.0,1149.0,1323.0,5.0,809.0,3.0,1144.0]
 -}
areas :: (Image img,
         MaxMin (Pixel img),
         RealFrac (Pixel img)) => img -> V.Vector Double
areas img@(dimensions -> (rows, cols)) = runST $ do 
  histogram <- VM.replicate ((floor $ maxIntensity img)+1) 0 :: ST s (VM.STVector s Double)
  forM_ (pixelList img) (\ (floor -> p) -> do
                            currVal <- VM.read histogram p
                            VM.write histogram p (currVal + 1))
  V.freeze histogram

{-| Given an image, perimeters returns a vector where the n-th component 
    equals the number of pixels with value n which are adjacent to pixels 
    of value 0 and the 0-th component equals the sum of the other components. 
    If image is the result of applying label to a binary image, then the 
    vector represents the perimeters of the connected-components of the 
    binary-image.

    >>>perimeters . label $ binaryStop
    fromList [1082.0,307.0,323.0,5.0,184.0,3.0,260.0]
 -}
perimeters :: (Image img,
               MaxMin (Pixel img),
               Pixel img ~ Double) => img -> V.Vector Double
perimeters img@(dimensions -> (rows, cols)) = runST $ do
  vector <- VM.replicate ((floor $ maxIntensity img)+1) 0 :: ST s (VM.STVector s Double)
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) -> do
   let ns = filter (== 0) . neighborList img r $ c
   if null ns then return ()
     else do
       let (floor -> group) = ref img r c
       currPerimeter <- VM.read vector group
       VM.write vector group (currPerimeter+1)
   )
  VM.write vector 0 0
  vals <- V.freeze vector
  VM.write vector 0 (V.sum vals)
  V.freeze vector
  
neighborList :: (Image img) => img -> Int -> Int -> [Pixel img]
neighborList img@(dimensions -> (rows, cols)) r c = 
  [ ref img r' c' | r' <- [r-1..r+1], c' <- [c-1..c+1], 
    r' /= r && c' /= c, r' >= 0, r' < rows, c' >= 0, c' < cols]

{-| Given an image, the result of applying label to a binary-image, 
    boundingBoxes returns a vector where the n-th component is a four 
    element tuple representing the minimum and maximum row and column 
    indices of pixels of the n-th connected-component of the image.

    >>>boundingBoxes . label $ binarySto
    [(10,8,73,41),(10,75,74,110),(12,12,16,16),(11,42,72,73),(13,80,15,82),(11,117,72,150)]
 -}
boundingBoxes :: (Image img,
                  MaxMin (Pixel img),
                  Pixel img ~ Double) => img -> [(Int, Int, Int, Int)]
boundingBoxes img@(dimensions -> (rows, cols)) = runST $ do 
  let n = floor . maxIntensity $ img
  boxes <- VM.new (n*4) :: ST s (VM.STVector s Int)
  
  -- Initialize boxes to be the entire image
  forM_ [0..n-1] (\ n -> do 
    VM.write boxes (n*4) rows
    VM.write boxes (n*4 + 1) cols
    VM.write boxes (n*4 + 2) 0
    VM.write boxes (n*4 + 3) 0)
    
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) ->
    let (floor -> px) = ref img r c in updateAt px r c boxes)
  
  boxes' <- V.freeze boxes
  
  return . toQuads . V.toList $ boxes'
  
updateAt :: Int -> Int -> Int -> VM.STVector s Int -> ST s ()
updateAt ((flip (-) 1) -> group) r c boxes 
  | group < 0 = return ()
  | otherwise = do
  let read = VM.read boxes
      write = VM.write boxes
  minR <- read (group*4)
  minC <- read (group*4 + 1)
  maxR <- read (group*4 + 2)
  maxC <- read (group*4 + 3)
  let minR' = min minR r
      minC' = min minC c
      maxR' = max maxR r
      maxC' = max maxC c
  write (group*4) minR'
  write (group*4 + 1) minC'
  write (group*4 + 2) maxR'
  write (group*4 + 3) maxC'
  
toQuads :: [a] -> [(a,a,a,a)]
toQuads = toQuads' [] where
  toQuads' acc [] = reverse acc
  toQuads' acc (a:b:c:d:xs) = toQuads' ((a,b,c,d):acc) xs
  
{-| Given an image, the result of applying label to a binary-image, 
    centersOfMass returns a vector where the n-th component is a tuple 
    representing the average row and column indices of pixels of the 
    n-th connected-component of the image.

    >>>centersOfMass . label $ binaryStop
    [(42.391644908616186,24.70409051348999),(41.80952380952381,92.23431594860166),(14.0,14.0),(35.31025957972806,57.595797280593324),(14.0,81.0),(35.59178321678322,129.90734265734267)]
 -}
centersOfMass :: (Image img,
                  MaxMin (Pixel img),
                  Pixel img ~ Double) => img -> [(Double, Double)]
centersOfMass img@(dimensions -> (rows, cols)) = runST $ do
  let n = floor . maxIntensity $ img
  centers <- VM.replicate n (0,0,0) :: ST s (VM.STVector s (Int, Int, Int))
  
  forM_ [ (r, c) | r <- [0..rows-1], c <- [0..cols-1]] (\ (r, c) ->
    let (floor -> px) = ref img r c in updateMass px r c centers)
  
  centers' <- V.freeze centers
  
  return . map averageMass . V.toList $ centers'
  
updateMass :: Int -> Int -> Int -> VM.STVector s (Int, Int, Int) -> ST s ()
updateMass ((flip (-) 1) -> group) r c centers
  | group < 0 = return ()
  | otherwise = do
    (pixels, totalRow, totalCol) <- VM.read centers group
    VM.write centers group (pixels+1, totalRow+r, totalCol+c)
    
averageMass :: (Int, Int, Int) -> (Double, Double)
averageMass ((fromIntegral -> total), (fromIntegral -> rows), (fromIntegral -> cols)) = (rows/total, cols/total)



{-| Given a binary image, distanceTransform returns an image 
    representing the 2D distance transform of the image.
    The distance transform is accurate to within a 2% error for euclidean
     distance.

    >>>distanceTransform binaryStop :: GrayImage
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/distancetransform.jpg>
 -}
distanceTransform :: (Image img,
                      BinaryPixel (Pixel img),
                      Image img',
                      Pixel img' ~ Double) => img -> img'
distanceTransform img@(dimensions -> (rows, cols)) = makeImage rows cols func
  where arr  = getDistanceTransformArray img
        func r c = arr V.! ((cols*r) + c)


-- Begin support code for distance         
getDistanceTransformArray :: (Image img, 
                              BinaryPixel (Pixel img)) => img -> V.Vector Double
getDistanceTransformArray img@(dimensions -> (rows, cols)) = runST $ do
  let size = rows * cols
      imgdata = V.fromList . map (boolToDouble . toBinary) . pixelList $ img :: V.Vector Double
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
  dtImg <- VM.replicate size 0 :: ST s (VM.STVector s Double)
  forM_ [0..size-1] $ \ i -> do
    let val = if ((imgdata V.! i) == 0) then 0 else on
    VM.write dtImg i val
  
  let pass rs cs tr br mask = do
        forM_ rs $ \ r -> do 
          forM_ cs $ \ c -> do
            let imgPixels = [ (i, j) | i <- [r+tr..r+br], j <- [c-2..c+2]]
            pxVals <- forM imgPixels $ \ (i, j) -> do
                        if (i < 0 || i > (rows-1) || j < 0 || j > (cols-1))
                          then return on
                          else do
                            val <- VM.read dtImg ((i*cols)+j)
                            return val
            let sums = map (\ (x, y) -> x+y) $ zip pxVals mask
            let min = minimum sums
            VM.write dtImg ((r*cols) + c) min
  -- Pass from from left to right and top to bottom
  pass [0..rows-1] [0..cols-1] (-2) 0 maskRight
  -- Pass from right to left and bottom to top  
  pass [rows-1,rows-2..0] [cols-1,cols-2..0] 0 2 maskLeft
  
  V.freeze dtImg

boolToDouble :: Bool -> Double        
boolToDouble True = 1.0
boolToDouble _ = 0.0

-- End Distance Transform Support code

{-| Given an image, outline returns an image where edge pixels are 
    set to the value on and non-edge pixels are set to the value off. 
    Pixel (i, j) is an edge pixel iff its value is different than the value 
    of either pixel (i, j+1) or pixel (i+1, j).

    >>>outline binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/outline.jpg>
 -}
outline :: (Image img,
            BinaryPixel (Pixel img),
            Eq (Pixel img)) => img -> img
outline img = outline' off on img

{-| Given two doubles nonEdge and edge, and an image, outline' returns 
    an image where edge pixels are 
    set to the value edge and non-edge pixels are set to the value nonEdge. 
    Pixel (i, j) is an edge pixel iff its value is different than the value 
    of either pixel (i, j+1) or pixel (i+1, j).

    >>>outline' (RGB (255, 255, 255)) (RGB (0, 0, 255)) binaryStop
    < Image 86x159 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/outline2.jpg>
 -}
outline' :: (Image img,
            BinaryPixel (Pixel img),
            Eq (Pixel img)) => Pixel img -> Pixel img -> img -> img
outline' nonEdge edge img@(dimensions -> (rows, cols)) = makeImage rows cols func
  where arr = getOutlineArray img edge nonEdge
        func r c = arr BV.! ((cols*r)+c)

-- Outline support code
getOutlineArray :: (Image img,
                    BinaryPixel (Pixel img),
                    Eq (Pixel img)) => img -> Pixel img -> Pixel img -> BV.Vector (Pixel img)
getOutlineArray img@(dimensions -> (rows, cols)) edge nonEdge = runST $ do
  let data1 = BV.fromList . map (boolToDouble . toBinary) . pixelList $ img :: BV.Vector Double
  data4 <- BVM.replicate (rows*cols) off -- :: ST s (BVM.STVector s (Pixel img))
  
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [0..cols-1] $ \ j -> do
      BVM.write data4 (index0+j) nonEdge

  forM [0..rows-2] $ \ i -> do
    let index0 = i*cols
    let index1 = (i+1)*cols
    forM [0..cols-1] $ \ j -> do
      let val = (data1 BV.! (index0+j)) + (data1 BV.! (index1+j))
      if (val == 1) 
        then 
          BVM.write data4 (index0+j) edge
        else return ()

  let index0 = (rows-1)*cols
  forM [0..cols-1] $ \ j -> do
    let val = (data1 BV.! (index0+j)) + (data1 BV.! j)
    if (val == 1)
      then BVM.write data4 (index0+j) edge
      else return ()

  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    forM [1..cols-2] $ \ j -> do
      let val = (data1 BV.! (index0+j)) + (data1 BV.! (index0 + j + 1))
      if (val == 1)
        then BVM.write data4 (index0+j) edge
        else return ()
   
  forM [0..rows-1] $ \ i -> do
    let index0 = i*cols
    let val = (data1 BV.! (index0+cols-1)) + (data1 BV.! index0)
    if (val == 1)
      then BVM.write data4 (index0+cols-1) edge
      else return ()  
   
  BV.freeze data4
-- End outline support code
