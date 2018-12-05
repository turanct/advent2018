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

allPossiblePolymers :: String -> [String]
allPossiblePolymers s = zipWith f letters (repeat s)
  where letters = [ letter  | letter <- sort $ unique $ map toLower s ]
        f letter string = filter (\x -> x /= letter && x /= toUpper letter) string

unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:xs) = x : unique (filter (/= x) xs)

shortestReduction :: [String] -> Int
shortestReduction = head . sort . lenghts
  where lenghts = map (length . collapseAll)
