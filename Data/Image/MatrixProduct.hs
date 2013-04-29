{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
{-# OPTIONS -O2 #-}
module Data.Image.MatrixProduct(matrixProduct) where

import Data.Image.Imageable

{-matrixProduct :: (Imageable img,
                  Pixel img ~ Double)-}

matrixProduct :: (Imageable img,
                  Num (Pixel img)) => img -> img -> img
matrixProduct 
  a@(dimensions -> (arows, acols)) 
  b@(dimensions -> (brows, bcols)) = if check then makeImage arows bcols product else err where
    check = acols == brows
    err = error "Matrix Product requires images with matching inner dimensions AxN and NxB and produces a new image with dimensions AxB."
    product r c = sum . zipWith (*) arow $ bcol where
      arow = map (ref a r) [0..acols-1]
      bcol = map (flip (ref b) c) [0..brows-1]
