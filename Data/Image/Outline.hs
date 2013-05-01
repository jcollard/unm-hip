{-# LANGUAGE ViewPatterns, TypeFamilies #-}
{-# OPTIONS -O2 #-}
module Data.Image.Outline(outline) where

import Control.Monad
import Control.Monad.ST

import Data.Image.Internal

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

