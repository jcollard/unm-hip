module Data.Image.DisplayFormat(DisplayFormat(..)) where

class DisplayFormat df where
  format :: df -> String