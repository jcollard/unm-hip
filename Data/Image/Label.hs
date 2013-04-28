{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Data.Image.Label(label) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Unboxed
import Data.Array.ST

import Data.Image.Imageable

import Data.STRef

label :: (Imageable img,
          Pixel img ~ Double) => img -> img
label img@(dimensions -> (rows,cols)) = makeImage rows cols func
  where arr  = getLabelArray img
        func r c = arr ! ((cols*r)+c)

-- Disgusting support code for label
mod' x n
 | x >= n = x - n
 | x < 0  = n + x
 | otherwise = x

--getLabelArray :: Image -> UArray Int RealPixel
getLabelArray :: (Imageable img,
                  Pixel img ~ Double) => img -> UArray Int Double
getLabelArray img@(dimensions -> (rows, cols)) = runSTUArray $ do
  let data1 = listArray (0,(rows*cols)-1) (pixelList img) :: Array Int Double
  acc <- newSTRef 1
  data2 <- newArray (0,rows*cols-1) 0 :: ST s (STUArray s Int Double)
  alias <- newAlias rows cols
  let rcs = [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
  let loop = nestedLoop acc rows cols data1 data2 alias
  forM rcs $ \ rc -> do loop rc
  let indices = [0..rows*cols-1]
  forM indices $ \ index -> do (findLowest data2 alias) index
  j <- newSTRef 1.0
  accVal <- readSTRef acc
  forM [1..(floor accVal)] $ \ index -> do (renumberLabels j alias) index
  forM [1..rows*cols-1] $ \ index -> do (relabelImage data2 alias) index
  return data2

relabelImage data2 alias i =
  do 
   dataVal <- readArray data2 i
   aliasVal <- readArray alias (floor dataVal)
   writeArray data2 i aliasVal

renumberLabels j alias i = 
  do
   aVal <- readArray alias i
   if ( (floor aVal) == i )
     then  
       do
         jVal <- readSTRef j
         writeSTRef j (jVal+1.0)
         writeArray alias i jVal
     else return ()

findLowest data2 alias i =
  do
   val1 <- readArray data2 i
   index0 <- newSTRef (floor val1)
   if (val1 > 0)
     then do 
       i <- readSTRef index0
       val2 <- readArray alias i
       index1 <- newSTRef (floor val2)
       whileLoop index0 index1 alias
       val2 <- readSTRef index1
       writeArray data2 i (fromIntegral val2)
       return ()
     else do return ()
  
whileLoop index0 index1 alias = 
  do 
    val0 <- readSTRef index0
    val1 <- readSTRef index1
    if (val0 /= val1)
       then do 
        writeSTRef index0 val1
        aVal <- readArray alias val1
        writeSTRef index1 (floor aVal)
        whileLoop index0 index1 alias
       else do return ()

nestedLoop acc rows cols data1 data2 alias (r, c) =
 do
  let index = r*cols + c
  let val = data1 ! index
  if (val > 0) 
   then
                 do
                   let aIndex = cols*(mod' (r-1) rows) + c
                   let bIndex = cols*r + (mod' (c-1) cols)
                   a <- readArray data2 aIndex
                   b <- readArray data2 bIndex
                   aVal <- readArray alias (floor a)
                   bVal <- readArray alias (floor b)
                   if (aVal == 0 && bVal == 0)
                      then do 
                             accVal <- readSTRef acc
                             writeSTRef acc (accVal+1)
                             writeArray data2 index accVal
                      else do
                             if (aVal /= 0 && bVal /= 0)
                                then 
                                  if (aVal == bVal)
                                     then do writeArray data2 index aVal
                                     else
                                       if (aVal < bVal)
                                         then do
                                           writeArray data2 index aVal
                                           writeArray alias (floor b) aVal
                                         else do
                                           writeArray data2 index bVal
                                           writeArray alias (floor a) bVal
                                else
                                  if (aVal /= 0)
                                    then do writeArray data2 index aVal
                                    else do writeArray data2 index bVal
  else return ()


newAlias :: Int -> Int -> ST s (STUArray s Int Double)
newAlias rows cols = do let n = rows*cols-1
                        alias <- newArray (0,n) 0 :: ST s (STUArray s Int Double)
                        mapM (initialize alias) [0..n]
                        return alias
  where initialize arr i = do
          writeArray arr i (fromIntegral i)
          
-- End support code for label        
