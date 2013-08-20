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

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Image.Arithmetic ((+.), (.+), (*.), (.*), (-.), (.-), (/.), (./)) where

import Data.Image.Internal

makeImage' im = makeImage (rows im) (cols im)

{-| Performs scalar addition to each pixel in an image -}
(+.) :: (Num (Pixel i), Image i) => Pixel i -> i -> i
k +. im = makeImage' im $ \i j -> k + (ref im i j)

{-| Performs scalar addition to each pixel in an image -}
im .+ k = k +. im

{-| Performs scalar multiplication to each pixel in an image -}
(*.) :: (Num (Pixel i), Image i) => Pixel i -> i -> i
k *. im = makeImage' im $ \i j -> k * (ref im i j)

{-| Performs scalar multiplication to each pixel in an image -}
im .* k = k *. im

{-| Performs scalar subtraction to each pixel in an image -}
(-.) :: (Num (Pixel i), Image i) => Pixel i -> i -> i
k -. im = makeImage' im $ \i j -> k - (ref im i j)

{-| Performs scalar subtraction to each pixel in an image -}
im .- k = im .+ (- k)

{-| Performs scalar division to each pixel in an image -}
(/.) :: (Fractional (Pixel i), Image i) => Pixel i -> i -> i
k /. im = makeImage' im $ \i j -> k / (ref im i j)

{-| Performs scalar division to each pixel in an image -}
im ./ k = im .* (1 / k)
