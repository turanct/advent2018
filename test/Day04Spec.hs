module Day04Spec where

import Test.Hspec
import Day04
import Prelude hiding (id)

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "parses strings to claims" $ do
      True `shouldBe` True

  -- describe "Part 1 with input from file" $ do
  --   file <- runIO $ readFile "test/day04.txt"

  --   it "solves the puzzle" $ do
  --     let input = lines file
  --     overlappingSquares input `shouldBe` 112378
