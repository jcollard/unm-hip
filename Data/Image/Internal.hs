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

{-# LANGUAGE 
   TypeFamilies, 
   FlexibleContexts, 
   FlexibleInstances, 
   ViewPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Internal(-- * Images
                           Image(..), 
                           PixelOp,
                           MaxMin(..),
                           Listable(..),
                           -- * Basic
                           dimensions, 
                           maxIntensity,
                           minIntensity,
                           transpose,
                           matrixProduct,
                           medianFilter,                           
                           normalize,
                           imageFold,
                           imageMap,
                           -- * Resizing Images
                           pad,
                           crop,
                           downsampleRows,
                           downsampleCols,
                           downsample,
                           upsampleRows,
                           upsampleCols,
                           upsample,
                           -- * Concatenation of Images
                           leftToRight,
                           leftToRight',
                           topToBottom,
                           topToBottom',
                           -- * Images as Arrays
                           imageToArray,
                           arrayToImage) where

--base>=4
import Data.Monoid
import Data.List(sort)

--array>=0.4.0.1
import Data.Array.IArray

-- | A function of a row and column that returns a pixel at that location
type PixelOp px = Int -> Int -> px

-- | An Image can be thought of as a 2 dimensional array of pixel values
class Image i where
  -- | The type of Pixel to be stored in images of type i.
  type Pixel i :: *

  {-| Given an Int m, Int n, and a PixelOp f, returns an Image with      
      dimensions m x n and the Pixel value at each (i, j) is (f i j)

      >>>let gradient = makeImage 128 128 (\r c -> fromIntegral (r*c)) :: GrayImage
      >>>gradient
      < Image 128x128 >

      <https://raw.github.com/jcollard/unm-hip/master/examples/gradient.jpg>
   -}
  makeImage :: Int -> Int -> PixelOp (Pixel i) -> i
  
  {-| Given an Image i, row i, and column j, returns the Pixel in
      i at row i and column j.

      >>>ref gradient 12 52
      624.0
   -}
  ref  :: i -> Int -> Int -> (Pixel i)
  
  {-| Given an Image i, returns the number of rows in i
      
      >>>rows gradient
      128
   -}
  rows :: i -> Int
  
  {-| Given an Image i, returns the number of columns in i
   
      >>>cols gradient
      128
   -}
  cols :: i -> Int
  
  {-| Given an Image i, returns a list containing all of the pixels in i.
      The order in which the pixels are returned is from top left to
      bottom right.

      >>> take 5 . reverse . pixelList $ gradient
      [16129.0,16002.0,15875.0,15748.0,15621.0]
   -}
  pixelList :: i -> [Pixel i]
  pixelList i = [ ref i r c | r <- [0..(rows i - 1)], c <- [0..(cols i - 1)]]
  
  {-| Given a function of two pixel values to a pixel value f, an image X, and 
      an image Y, return an Image that for each pixel value at (i,j) is the
      result of applying f to X(i,j) and Y(i,j). Note: The dimensions of X and
      Y must be equal otherwise the result of imageOp is undefined.  

      >>>let white = makeImage 128 128 (\ r c -> 8000) :: GrayImage
      >>>let diff = imageOp (-) gradient white
      >>>diff
      < Image 128x128 >
      >>>ref diff 0 0
      -8000.0

      <https://raw.github.com/jcollard/unm-hip/master/examples/whitegrad.jpg>
   -} 
  imageOp :: (Pixel i -> Pixel i -> Pixel i) -> i -> i -> i
  imageOp op i0 i1 = makeImage (rows i0) (cols i0) operate where
    operate r c = op (ref i0 r c) (ref i1 r c)
  
{-| Something is Listable if it can be converted to a list.
    This type class is mostly for convenience when using leftToRight'
    and topToBottom'.
 -}
class Listable a where
  -- | The type of the elements in the list
  type Elem a :: *
  toList :: a -> [Elem a]

instance Listable [a] where
  type Elem [a] = a
  toList = id

instance Listable (a,a) where
  type Elem (a,a) = a
  toList (a,b) = [a,b]
  
instance Listable (a,a,a) where
  type Elem (a,a,a) = a
  toList (a,b,c) = [a,b,c]
    
class MaxMin m where
  -- | Given a [m] returns the maximal m in the list
  maximal :: [m] -> m
  -- | Given a [m] returns the minimal m in the list
  minimal :: [m] -> m

{-| Given an Image i, return a pair (rows i, cols i)
    
    >>>dimensions gradient
    (128, 128)
 -}
dimensions :: Image i => i -> (Int, Int)
dimensions i = (rows i, cols i)

{-| Given an Image i, returns the value of the Pixel with the maximal intensity

    >>>maxIntensity gradient
    16129.0
    >>>maxIntensity cactii
    RGB (254.0, 254.0, 254.0)
 -}
maxIntensity :: (Image img, MaxMin (Pixel img)) => img -> Pixel img
maxIntensity = maximal . pixelList

{-| Given an Image i, returns the value of the Pixel with the minimal intensity
    
    >>>minIntensity gradient
    0.0
    >>>minIntensity cactii
    RGB (18.0, 18.0, 18.0)
 -}
minIntensity :: (Image img, MaxMin (Pixel img)) => img -> Pixel img
minIntensity = minimal . pixelList


{-| Given an Image i, returns an Image created by interchanging the rows 
    and columns of i, i.e., the pixel value at location (i, j) of the resulting 
    Image is the value of i at location (j, i).

    >>> transpose frog
    < Image 242x225 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/transposefrog.jpg>
 -}
transpose :: (Image img) => img -> img
transpose img@(dimensions -> (rows, cols)) = makeImage cols rows trans where
  trans r c = ref img c r

{-| Given m, n, and img, pad returns an Image with m rows and n columns 
    where the value at location (i, j) of the result image is the value 
    of img at location (i, j) if i is less than m and j is less than n 
    and mempty otherwise.

    >>> pad 256 256 frog
    < Image 256x256 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/padfrog.jpg>
 -}
pad :: (Image img, Monoid (Pixel img)) => Int -> Int -> img -> img    
pad rs cs img@(dimensions -> (rows, cols)) = makeImage rs cs pad where
  pad r c 
    | r < rows && c < cols = ref img r c
    | otherwise = mempty
  
{-| Given a i0, j0, m, n, and img, crop returns an image with m rows 
    and n columns where the value at location (i, j) of the result 
    image is the value of img at location (i0 + i, j0 + j).  

    >>> crop 64 64 128 128 frog
    < Image 128x128 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/cropfrog.jpg>
 -}
crop :: (Image img) => Int -> Int -> Int -> Int -> img -> img
crop r0 c0 w h img = makeImage w h crop where
  crop r c = ref img (r+r0) (c+c0)
                  
{-| Given img, downsampleCols returns the image created by discarding 
    the odd numbered rows, i.e., the value at location (i, j) of the 
    result image is the value of img at location (2i, j).  

    >>> downsampleCols frog
    < Image 112x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/downsamplecolsfrog.jpg>
 -}
downsampleCols :: (Image img) => img -> img
downsampleCols img@(dimensions -> (rows, cols)) = makeImage (rows `div` 2) cols downsample where
  downsample r c = ref img (r*2) c
  
{-| Given img, downsampleRows returns the image created by discarding the odd 
    numbered columns, i.e., the value at location (i, j) is the value of img 
    at location (i, 2j).  

    >>> downsampleRows frog
    < Image 225x121 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/downsamplerowsfrog.jpg>
 -}
downsampleRows :: (Image img) => img -> img
downsampleRows img@(dimensions -> (rows, cols)) = makeImage rows (cols `div` 2) downsample where
  downsample r c = ref img r (c*2)
  
{-| Given img, downsample returns the image created by discarding the odd
    numbered rows and columns, i.e., the value at location (i, j) is the
    value of img at location (2i, 2j)

    >>>let smallfrog = downsample frog
    >>>smallfrog
    < Image 112x121 >
    
    <https://raw.github.com/jcollard/unm-hip/master/examples/downsamplefrog.jpg>
 -}
downsample :: (Image img) => img -> img
downsample = downsampleRows . downsampleCols

{-| Given img, upsampleCols returns an image with twice the number of 
    rows where the value at location (i, j) of the result image is the 
    value of img at location (i/2, j) if i is even and mempty otherwise.

    >>>upsampleCols smallfrog
    < Image 224x121 >
 
    <https://raw.github.com/jcollard/unm-hip/master/examples/upsamplecols.jpg>
 -}
upsampleCols :: (Image img, Monoid (Pixel img)) => img -> img
upsampleCols img@(dimensions -> (rows, cols)) = makeImage (rows*2) cols upsample where
  upsample r c 
    | even r = ref img (r `div` 2) c
    | otherwise = mempty

{-| Given img, upsampleRows returns an image with twice the number of 
    columns where the value at location (i, j) of the result image is 
    the value of img at location (i, j/2) if j is even and 
    mempty otherwise.

    >>>upsampleRows smallfrog
    < Image 112x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/upsamplerows.jpg>
 -}
upsampleRows :: (Image img, Monoid (Pixel img)) => img -> img
upsampleRows img@(dimensions -> (rows, cols)) = makeImage rows (cols*2) upsample where
  upsample r c 
    | even c = ref img r (c `div` 2)
    | otherwise = mempty

{-| Given img, upsample returns an image with twice the number of
    rows and columns where the value at location (i, j) of the resulting
    image is the value of img at location (i/2, j/2) if i and jare are even
    and mempty otherwise.

    >>>upsample smallfrog
    < Image 224x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/upsample.jpg>
 -}
upsample :: (Image img, Monoid (Pixel img)) => img -> img
upsample = upsampleRows . upsampleCols

{-| Given an image X1 and an image X2, where the number of columns of X1 
    equals the number of rows of X2, matrixProduct returns an image 
    representing the matrix product of X1 and X2. 

    >>>let cropped = crop 64 64 128 128 frog
    >>>matrixProduct cropped cropped
    < Image 128x128 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/matrixproduct.jpg>
-}
matrixProduct :: (Image img,
                  Num (Pixel img)) => img -> img -> img
matrixProduct 
  a@(dimensions -> (arows, acols)) 
  b@(dimensions -> (brows, bcols)) = if check then makeImage arows bcols product else err where
    check = acols == brows
    err = error "Matrix Product requires images with matching inner dimensions AxN and NxB and produces a new image with dimensions AxB."
    product r c = sum . zipWith (*) arow $ bcol where
      arow = map (ref a r) [0..acols-1]
      bcol = map (flip (ref b) c) [0..brows-1]

{-| Given two positive integers, m and n and a an image, 
    medianFilter returns an image with the same dimensions where each 
    pixel (i, j) in <image> is replaced by the pixel with median value 
    in the neighborhood of size m times n centered on (i, j).

    >>>medianFilter 5 5 frog
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/medianfilter.jpg>
 -}
medianFilter :: (Image img,
                 Ord (Pixel img)) => Int -> Int -> img -> img
medianFilter m n img@(dimensions -> (rows, cols)) = makeImage rows cols avg where
  [moff, noff] = map (`div` 2) [m,n]
  avg r c = px !! ((fromIntegral . length $ px) `div` 2) where
    px = sort [ ref img i j |  
                   i <- [rm..rm+m], 
                   j <- [cm..cm+n], 
                   i >= 0, i < rows, j >= 0, j < cols]
    rm = r - moff
    cm = c - noff

{-| Given img, normalize returns an image with the same dimensions 
    where the values have been normalized to lie in the interval [0, 1].

    >>>let normalfrog = normalize frog
    >>>ref frog 0 0
    151.0
    >>>ref normalfrog 0 0
    0.592156862745098
 -}
normalize :: (Image img,
              MaxMin (Pixel img),
              RealFloat (Pixel img)) => img -> img
normalize img@(dimensions -> (rows, cols)) = check  where
  check = if isNaN scale then blank else makeImage rows cols map where
    blank = makeImage rows cols (\ _ _ -> 0)
    map r c = scale * ((ref img r c) - min)
    (min, max) = (minimal px, maximal px)
    scale = 1 / (max - min)
    px = pixelList img

{-| Folds over the pixels of the provided image 
 
    >>>imageFold (+) 0 frog
    6948219.0
-}
imageFold :: Image img => (Pixel img -> b -> b) -> b -> img -> b
imageFold f init img = foldr f init (pixelList img)

{-| Maps a function over each pixel in the provided image. When using
    Boxed images, you should use fmap instead.

    >>>imageMap ((-1) *) frog :: GrayImage
    < Image 225x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/invertfrog.jpg>
 -}
imageMap :: (Image img, Image img') => (Pixel img -> Pixel img') -> img -> img'
imageMap f img@(dimensions -> (rows, cols)) = makeImage rows cols map where
  map r c = f (ref img r c) 

{-| Given two images with the same number of rows X and Y,  returns an
    image that is the concatenation of the two images from left to right.

    >>>leftToRight frog frog
    < Image 225x484 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/lefttoright.jpg>
 -}
leftToRight :: (Image img) => img -> img -> img
leftToRight i0@(dimensions -> (rows, cols)) i1@(dimensions -> (rows', cols'))
  | rows /= rows' = error "leftToRight: Images must have matching row length."
  | otherwise = makeImage rows (cols + cols') concat where
                  concat r c 
                    | c < cols = ref i0 r c 
                    | otherwise = ref i1 r (c - cols)
                  
{-| Given a Listable of images each of which have the same number of rows,
    returns an image that is the concatenation of all of the images from 
    left to Right.

    >>>leftToRight' . replicate 3 $ frog
    < Image 225x726 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/lefttoright3.jpg>
 -}
leftToRight' :: (Listable a,
                 Image img,
                 Image (Elem a), 
                 Elem a ~ img) => a -> img
leftToRight' (toList -> imgs) = foldr1 leftToRight imgs

{-| Given two images with the same number of columns X and Y, returns an
    image that is the concatenation of the two images from top to bottom.

    >>>topToBottom frog frog
    < Image 450x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/toptobottom.jpg>
 -}
topToBottom :: (Image img) => img -> img -> img
topToBottom i0@(dimensions -> (rows, cols)) i1@(dimensions -> (rows',cols')) 
  | cols /= cols' = error "topToBottom: Images must have matching column length."
  | otherwise = makeImage (rows + rows') cols concat where
                  concat r c
                    | r < rows = ref i0 r c
                    | otherwise = ref i1 (r - rows) c

{-| Given a Listable of images all of which have the same number of columns,
    returns an image that is the concatenation of all of theimages from top
    to bottom.

    >>>topToBottom' . replicate 3 $ frog
    < Image 675x242 >

    <https://raw.github.com/jcollard/unm-hip/master/examples/toptobottom3.jpg>
 -}
topToBottom' :: (Listable a,
                 Image img,
                 Image (Elem a),
                 Elem a ~ img) => a -> img
topToBottom' (toList -> imgs) = foldr1 topToBottom imgs

  
{-| Given img, returns an two dimensional array of Pixel values 
    indexed by pairs of Ints where the fst is the row and snd is the column.

    >>>let frogArr = imageToArray frog
    >>>frogArr ! (0, 0)
    151.0
 -}
imageToArray :: (Image img) => img -> Array (Int, Int) (Pixel img)
imageToArray img@(dimensions -> (rows, cols)) = listArray bounds elems where
  bounds = ((0,0), (rows-1,cols-1))
  elems = pixelList img

{-| Given a two dimensional array of Pixel values indexed by
    pairs of Ints where the fst is the row and snd is the column, returns
    an Image. 

    >>>let img = arrayToImage (listArray ((0,0) (127,127)) [0..]) :: GrayImage
    >>>img
    < Image 128x128 >
    >>>ref img 0 0
    0.0
    >>>ref img 0 10
    10.0
    >>>ref img 10 0
    1280.0
    >>>ref img 10 10
    1290.0
 -}
arrayToImage :: (Image img) => Array (Int, Int) (Pixel img) -> img
arrayToImage arr = makeImage rows cols ref where
  ((rmin,cmin), (rmax, cmax)) = bounds arr
  rows = rmax - rmin + 1
  cols = cmax - cmin + 1
  ref r c = arr ! (r, c)