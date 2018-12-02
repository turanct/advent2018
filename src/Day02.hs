module Day02 where

import Data.List

type BoxId = String

xOfTheSame :: Int -> [BoxId] -> [BoxId]
xOfTheSame x = filter hasXChars
  where hasXChars boxId = elem x $ map length $ group $ sort boxId

checksum :: [BoxId] -> Int
checksum boxIds = twoOfTheSame * threeOfTheSame
  where twoOfTheSame = length $ xOfTheSame 2 boxIds
        threeOfTheSame = length $ xOfTheSame 3 boxIds

similarPart :: [BoxId] -> String
similarPart boxIds = withoutDifference
  where withoutDifference = concat $ zipWith (\c b -> if b then [c] else [])
                                             (fst similarCombinations)
                                             (uncurry similarity similarCombinations)
        similarCombinations = head $ filter (oneDifference . (uncurry similarity))
                                            (combine boxIds)

oneDifference :: [Bool] -> Bool
oneDifference = (== 1) . length . filter (== False)

combine :: [BoxId] -> [(BoxId, BoxId)]
combine boxIds = [ (x, y) | x <- boxIds, y <- boxIds ]

similarity :: BoxId -> BoxId -> [Bool]
similarity = zipWith (==)
