module Day01Spec where

import Test.Hspec
import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "calibrates frequency example 1" $ do
      let frequencyChanges1 = [1, 1, 1]
      calibrateWith frequencyChanges1 `shouldBe` 3

    it "calibrates frequency example 2" $ do
      let frequencyChanges2 = [1, 1, -2]
      calibrateWith frequencyChanges2 `shouldBe` 0

    it "calibrates frequency example 3" $ do
      let frequencyChanges2 = [-1, -2, -3]
      calibrateWith frequencyChanges2 `shouldBe` -6

  describe "Part 1 with input from file" $ do
    file <- runIO $ readFile "test/day01.txt"

    it "solves the puzzle" $ do
      let frequencyChanges = map read $ map (dropWhile (== '+')) $ lines file
      calibrateWith frequencyChanges `shouldBe` 477

  describe "Part 2" $ do
    it "finds duplicate frequencies in calibrations 1" $ do
      let frequencyChanges1 = cycle [1, -1]
      findDuplicateFrequency frequencyChanges1 [0] `shouldBe` 0

    it "finds duplicate frequencies in calibrations 2" $ do
      let frequencyChanges2 = cycle [3, 3, 4, -2, -4]
      findDuplicateFrequency frequencyChanges2 [0] `shouldBe` 10

    it "finds duplicate frequencies in calibrations 3" $ do
      let frequencyChanges3 = cycle [-6, 3, 8, 5, -6]
      findDuplicateFrequency frequencyChanges3 [0] `shouldBe` 5

    it "finds duplicate frequencies in calibrations 4" $ do
      let frequencyChanges4 = cycle [7, 7, -2, -7, -4]
      findDuplicateFrequency frequencyChanges4 [0] `shouldBe` 14

  -- describe "Part 2 with input from file" $ do
  --   file <- runIO $ readFile "test/day01.txt"

  --   it "solves the puzzle" $ do
  --     let frequencyChanges = cycle $ map read $ map (dropWhile (== '+')) $ lines file
  --     findDuplicateFrequency frequencyChanges [0] `shouldBe` 390
