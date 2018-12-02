module Day02Spec where

import Test.Hspec
import Day02

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "finds ids with two of the same chars" $ do
      let boxIds = [ "abcdef"
                   , "bababc"
                   , "abbcde"
                   , "abcccd"
                   , "aabcdd"
                   , "abcdee"
                   , "ababab"
                   ]
      let idsWithTwo = [ "bababc"
                       , "abbcde"
                       , "aabcdd"
                       , "abcdee"
                       ]
      xOfTheSame 2 boxIds `shouldBe` idsWithTwo

    it "finds ids with three of the same chars" $ do
      let boxIds = [ "abcdef"
                   , "bababc"
                   , "abbcde"
                   , "abcccd"
                   , "aabcdd"
                   , "abcdee"
                   , "ababab"
                   ]
      let idsWithThree = [ "bababc"
                         , "abcccd"
                         , "ababab"
                         ]
      xOfTheSame 3 boxIds `shouldBe` idsWithThree

    it "calculates a checksum" $ do
      let boxIds = [ "abcdef"
                   , "bababc"
                   , "abbcde"
                   , "abcccd"
                   , "aabcdd"
                   , "abcdee"
                   , "ababab"
                   ]
      checksum boxIds `shouldBe` 12

  describe "Part 1 with input from file" $ do
    file <- runIO $ readFile "test/day02.txt"

    it "solves the puzzle" $ do
      let boxIds = lines file
      checksum boxIds `shouldBe` 6642

  describe "Part 2" $ do
    it "finds close boxIds" $ do
      let boxIds = [ "abcde"
                   , "fghij"
                   , "klmno"
                   , "pqrst"
                   , "fguij"
                   , "axcye"
                   , "wvxyz"
                   ]
      similarPart boxIds `shouldBe` "fgij"

  describe "Part 2 with input from file" $ do
    file <- runIO $ readFile "test/day02.txt"

    it "solves the puzzle" $ do
      let boxIds = lines file
      similarPart boxIds `shouldBe` "cvqlbidheyujgtrswxmckqnap"
