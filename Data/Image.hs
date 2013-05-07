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

{-| The Univeristy of New Mexico's Haskell Image Processing library contains
    functionality for performing manipulation gray, color, and complex images.

    If you want to jump right in, simply import this module and view the examples at the following URL:

    <https://github.com/jcollard/unm-hip>
-}
module Data.Image(
  -- | By importing Data.Image you will get functionality for working with Boxed images
  module Data.Image.Boxed) where
import Data.Image.Boxed