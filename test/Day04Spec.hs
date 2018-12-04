module Day04Spec where

import Test.Hspec
import Day04
import Prelude hiding (id)

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "parses strings to logs" $ do
      let strings = [ "[1518-11-21 00:01] Guard #641 begins shift"
                    , "[1518-05-29 00:31] falls asleep"
                    , "[1518-02-20 00:40] wakes up"
                    ]
      let logs = [ Log { time = Time { y = 1518, m = 11, d = 21, h = 0, i = 1 }
                       , event = BeganShift 641
                       }
                 , Log { time = Time { y = 1518, m = 5, d = 29, h = 0, i = 31 }
                       , event = FellAsleep
                       }
                 , Log { time = Time { y = 1518, m = 2, d = 20, h = 0, i = 40 }
                       , event = WokeUp
                       }
                 ]
      logsFromStrings strings `shouldBe` logs

  -- describe "Part 1 with input from file" $ do
  --   file <- runIO $ readFile "test/day04.txt"

  --   it "solves the puzzle" $ do
  --     let input = lines file
  --     overlappingSquares input `shouldBe` 112378
