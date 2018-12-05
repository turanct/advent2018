module Day05Spec where

import Test.Hspec
import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "collapses Polymers 1" $ do
      collapsePolymers "dabAcCaCBAcCcaDA" `shouldBe` "dabAaCBAcCcaDA"

    it "collapses Polymers 2" $ do
      collapsePolymers "dabAaCBAcCcaDA" `shouldBe` "dabCBAcCcaDA"

    it "collapses Polymers 3" $ do
      collapsePolymers "dabCBAcCcaDA" `shouldBe` "dabCBAcaDA"

    it "collapses all Polymers" $ do
      collapseAll "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"

  -- describe "Part 1 with input from file" $ do
  --   file <- runIO $ readFile "test/day05.txt"
  --   let input = head $ lines file

  --   it "solves the puzzle" $ do
  --     (length . collapseAll) input `shouldBe` 10

  describe "Part 2" $ do
    it "creates all possible Polymers without one letter" $ do
      let polymer = "dabAcCaCBAcCcaDA"

      allPossiblePolymers polymer `shouldBe` ["dbcCCBcCcD", "daAcCaCAcCcaDA", "dabAaBAaDA", "abAcCaCBAcCcaA"]

    it "finds the shortest polymer reduction of a list of polymers" $ do
      let polymers = ["dbcCCBcCcD", "daAcCaCAcCcaDA", "dabAaBAaDA", "abAcCaCBAcCcaA"]

      shortestReduction polymers `shouldBe` 4

  -- describe "Part 2 with input from file" $ do
  --   file <- runIO $ readFile "test/day05.txt"
  --   let input = head $ lines file

  --   it "solves the puzzle" $ do
  --     shortestReduction (allPossiblePolymers input) `shouldBe` 5148
