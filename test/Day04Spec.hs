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

    it "finds minutes between times" $ do
      let time1 = Time { y = 1518, m = 1, d = 1, h = 0, i = 2 }
      let time2 = Time { y = 1518, m = 1, d = 1, h = 0, i = 5 }
      let expectedMinutes = [ Time { y = 1518, m = 1, d = 1, h = 0, i = 2 }
                            , Time { y = 1518, m = 1, d = 1, h = 0, i = 3 }
                            , Time { y = 1518, m = 1, d = 1, h = 0, i = 4 }
                            ]
      minsBetween time1 time2 `shouldBe` expectedMinutes

    it "reads states from logs" $ do
      let logs = [ Log { time = Time { y = 1518, m = 1, d = 1, h = 0, i = 1 }
                       , event = BeganShift 1
                       }
                 , Log { time = Time { y = 1518, m = 1, d = 1, h = 0, i = 31 }
                       , event = FellAsleep
                       }
                 , Log { time = Time { y = 1518, m = 1, d = 1, h = 0, i = 35 }
                       , event = WokeUp
                       }
                 , Log { time = Time { y = 1518, m = 1, d = 1, h = 0, i = 31 }
                       , event = BeganShift 2
                       }
                 ]
      let states = [ Awake 1 [ Time { y = 1518, m = 1, d = 1, h = 0, i = 31 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 32 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 33 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 34 }
                             ]
                   , Awake 2 []
                   ]
      statesFromLogs logs `shouldBe` states

    it "reads shifts from states" $ do
      let states = [ Awake 1 [ Time { y = 1518, m = 1, d = 1, h = 0, i = 31 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 32 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 33 }
                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 34 }
                             ]
                   , Awake 2 []
                   ]
      let shifts = [ Shift { guard = 1
                           , minutesAsleep = [ Time { y = 1518, m = 1, d = 1, h = 0, i = 31 }
                                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 32 }
                                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 33 }
                                             , Time { y = 1518, m = 1, d = 1, h = 0, i = 34 }
                                             ]
                           }
                   , Shift { guard = 2, minutesAsleep = [] }
                   ]
      shiftsFromStates states `shouldBe` shifts

  describe "Part 1 with input from file" $ do
    file <- runIO $ readFile "test/day04.txt"

    it "solves the puzzle" $ do
      let input = lines file
      solvePart1 input `shouldBe` 4716

  describe "Part 2 with input from file" $ do
    file <- runIO $ readFile "test/day04.txt"

    it "solves the puzzle" $ do
      let input = lines file
      solvePart2 input `shouldBe` 117061
