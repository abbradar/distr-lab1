module Misc where

import Data.Char (isUpper, toLower)

underscore :: String -> String
underscore "" = ""
underscore (x:xs)
  | isUpper x = '_':toLower x:underscore xs
  | otherwise = x:underscore xs

