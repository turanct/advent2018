module Day05 where

import Data.List
import Data.Char

collapsePolymers :: String -> String
collapsePolymers [] = []
collapsePolymers [x] = [x]
collapsePolymers (x:y:zs)
  | x == toLower y && toUpper x == y = zs
  | x == toUpper y && toLower x == y = zs
  | otherwise = x:collapsePolymers (y:zs)

collapseAll :: String -> String
collapseAll [] = []
collapseAll [x] = [x]
collapseAll s = if length collapsed == length s then s else collapseAll collapsed
  where collapsed = collapsePolymers s
