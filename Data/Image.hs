module Data.Image(module Data.Image.Boxed) where
import Data.Image.Boxed 

eroden 0 = erode
eroden n = erode . (eroden (n-1))
