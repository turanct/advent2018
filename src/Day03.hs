module Day03 where

import Prelude hiding (id)
import Data.List

data Claim = Claim
  { id :: Int
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  } deriving (Eq, Show)

-- input of form #123 @ 3,2: 5x4
claimFromString :: String -> Claim
claimFromString s = Claim { id = id
                          , left = left
                          , top = top
                          , width = width
                          , height = height
                          }
  where id = read $ takeWhile (/= ' ') $ dropWhile (== '#') s
        left = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= '@') s
        top = read $ takeWhile (/= ':') $ drop 1 $ dropWhile (/= ',') s
        width = read $ takeWhile (/= 'x') $ drop 2 $ dropWhile (/= ':') s
        height = read $ drop 1 $ dropWhile (/= 'x') s

data Coord = Coord
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

type Area = [Coord]

toArea :: Claim -> Area
toArea c = [ Coord { x = x, y = y } | x <- [l + 1..l + w], y <- [t + 1..t + h]]
  where l = left c
        t = top c
        w = width c
        h = height c

overlappingSquares :: [String] -> Int
overlappingSquares input = length $ filter (\x -> length x > 1) $ group $ sort $ concatMap (toArea . claimFromString) input

overlapsWith :: Claim -> Claim -> Bool
overlapsWith c1 c2 = any (\x -> length x > 1) $ group $ sort $ areas
  where areas = toArea c1 ++ toArea c2

nonOverlappingClaims :: [String] -> [Int]
nonOverlappingClaims input = map id $ filter (\c -> not $ any (\x -> x `overlapsWith` c) (claims \\ [c])) claims
  where claims = map claimFromString input
