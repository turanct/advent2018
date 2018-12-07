module Day06 where

import Data.List
import Data.Maybe

data Coord = Coord { x :: Int
                   , y :: Int
                   } deriving (Show, Eq, Ord)

coordFromString :: String -> Coord
coordFromString s = Coord { x = x, y = y }
  where x = read $ takeWhile (/= ',') s
        y = read $ drop 2 $ dropWhile (/= ',') s

manhattan :: Coord -> Coord -> Int
manhattan a b = abs (x b - x a) + abs (y b - y a)

boundaries :: [Coord] -> (Coord, Coord)
boundaries cs = (Coord { x = minX, y = minY }, Coord { x = maxX, y = maxY })
  where minX = minimum $ map x cs
        minY = minimum $ map y cs
        maxX = maximum $ map x cs
        maxY = maximum $ map y cs

edges :: Coord -> Coord -> [Coord]
edges topLeft bottomRight =
  [ Coord {x=x topLeft,y=y} | y <- [y topLeft..y bottomRight - 1]]
  ++ [ Coord {x=x,y=y bottomRight} | x <- [x topLeft..x bottomRight - 1]]
  ++ reverse [ Coord {x=x bottomRight,y=y} | y <- [y topLeft + 1..y bottomRight]]
  ++ reverse [ Coord {x=x,y=y topLeft} | x <- [x topLeft + 1..x bottomRight]]

fillPlane :: Coord -> Coord -> [Coord]
fillPlane topLeft bottomRight =
  [ Coord {x=x,y=y} | x <- [x topLeft..x bottomRight], y <- [y topLeft..y bottomRight] ]

closest :: [Coord] -> Coord -> Maybe Coord
closest cs c = if manhattan c first == manhattan c second then Nothing else Just first
  where first = head sorted
        second = head $ drop 1 sorted
        sorted = sortBy dstToC cs
        dstToC a b = compare (manhattan c a) (manhattan c b)

biggestPlane :: [Coord] -> (Coord, Int)
biggestPlane cs = head $ reverse $ sorted
  where boundCoords = boundaries cs
        plane = fillPlane (fst boundCoords) (snd boundCoords)
        edgeCoords = edges (fst boundCoords) (snd boundCoords)
        -- [(Coord, match)]
        matches = map (\(x,y) -> (x, fromJust y))
                $ filter (isJust . snd)
                $ zip plane
                $ map (closest cs) plane
        -- [Coord]
        touchEdge = map snd $ filter (\x -> elem (fst x) edgeCoords) matches
        -- [(Coord, match)]
        nonEndless = filter (\x -> not $ elem (snd x) touchEdge) matches
        -- [[(Coord, match)]]
        grouped = groupBy (\x y -> snd x == snd y)
                $ sortBy (\x y -> compare (snd x) (snd y)) nonEndless
        -- [(match, Int)]
        counted = map (\group -> (snd $ head group, length group)) grouped
        sorted = sortBy (\(_, l1) (_, l2) -> compare l1 l2) counted

planeDistance :: Int -> [Coord] -> [Coord]
planeDistance i cs = filter (\c -> dstToAll cs c < i) plane
  where boundCoords = boundaries cs
        plane = fillPlane (fst boundCoords) (snd boundCoords)

dstToAll :: [Coord] -> Coord -> Int
dstToAll cs c = sum $ map (\coord -> manhattan coord c) cs
