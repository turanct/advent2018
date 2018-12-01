module Day01 where

type Frequency = Int
type FrequencyChange = Int

calibrateWith :: [FrequencyChange] -> Frequency
calibrateWith = sum

findDuplicateFrequency :: [FrequencyChange] -> [Frequency] -> Frequency
findDuplicateFrequency [] _ = 0
findDuplicateFrequency (fc:fcs) fs
  | newFrequency `elem` fs = newFrequency
  | otherwise = findDuplicateFrequency fcs (newFrequency:fs)
  where newFrequency = fc + head fs
