module Day06Spec where

import Test.Hspec
import Day06

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "calculates Manhattan distance between two Coords 1" $ do
      manhattan Coord {x=0,y=0} Coord {x=0,y=0} `shouldBe` 0

    it "calculates Manhattan distance between two Coords 2" $ do
      manhattan Coord {x=0,y=0} Coord {x=0,y=1} `shouldBe` 1

    it "calculates Manhattan distance between two Coords 3" $ do
      manhattan Coord {x=0,y=0} Coord {x=1,y=1} `shouldBe` 2

    it "calculates Manhattan distance between two Coords 4" $ do
      manhattan Coord {x=1,y=1} Coord {x=2,y=2} `shouldBe` 2

    it "finds boundaries for the plane in which coords live" $ do
      let coords = [ Coord {x=1,y=1}
                   , Coord {x=1,y=6}
                   , Coord {x=8,y=3}
                   , Coord {x=3,y=4}
                   , Coord {x=5,y=5}
                   , Coord {x=8,y=9}
                   ]
      boundaries coords `shouldBe` (Coord {x=1, y=1}, Coord {x=8,y=9})

    it "finds edges for the plane in which coords live" $ do
      let topLeft = Coord {x=0,y=0}
      let bottomRight = Coord {x=3,y=2}
      let expectedEdges = [ Coord {x=0,y=0}
                  , Coord {x=0,y=1}
                  , Coord {x=0,y=2}
                  , Coord {x=1,y=2}
                  , Coord {x=2,y=2}
                  , Coord {x=3,y=2}
                  , Coord {x=3,y=1}
                  , Coord {x=3,y=0}
                  , Coord {x=2,y=0}
                  , Coord {x=1,y=0}
                  ]
      edges topLeft bottomRight `shouldBe` expectedEdges

    it "fills a plane with all coords" $ do
      let topLeft = Coord {x=0,y=0}
      let bottomRight = Coord {x=3,y=2}
      let plane = [ Coord {x=0,y=0}
                  , Coord {x=0,y=1}
                  , Coord {x=0,y=2}
                  , Coord {x=1,y=0}
                  , Coord {x=1,y=1}
                  , Coord {x=1,y=2}
                  , Coord {x=2,y=0}
                  , Coord {x=2,y=1}
                  , Coord {x=2,y=2}
                  , Coord {x=3,y=0}
                  , Coord {x=3,y=1}
                  , Coord {x=3,y=2}
                  ]
      fillPlane topLeft bottomRight `shouldBe` plane

    it "finds the closest coord from a list" $ do
      let coords = [ Coord {x=1,y=1}
                   , Coord {x=1,y=6}
                   , Coord {x=8,y=3}
                   , Coord {x=3,y=4}
                   , Coord {x=5,y=5}
                   , Coord {x=8,y=9}
                   ]
      closest coords Coord {x=0,y=0} `shouldBe` Just Coord {x=1,y=1}
      closest coords Coord {x=8,y=8} `shouldBe` Just Coord {x=8,y=9}
      closest coords Coord {x=5,y=1} `shouldBe` Nothing

    it "finds biggest non-endless plane" $ do
      let coords = [ Coord {x=1,y=1}
                   , Coord {x=1,y=6}
                   , Coord {x=8,y=3}
                   , Coord {x=3,y=4}
                   , Coord {x=5,y=5}
                   , Coord {x=8,y=9}
                   ]
      biggestPlane coords `shouldBe` (Coord {x = 5, y = 5}, 17)

  -- describe "Part 1 with input from file" $ do
  --   file <- runIO $ readFile "test/day06.txt"
  --   let input = map coordFromString $ lines file

  --   it "solves the puzzle" $ do
  --     biggestPlane input `shouldBe` (Coord {x = 248, y = 265},3276)

  describe "Part 2" $ do
    it "finds plane with total distance less than 32 from all coords" $ do
      let coords = [ Coord {x=1,y=1}
                   , Coord {x=1,y=6}
                   , Coord {x=8,y=3}
                   , Coord {x=3,y=4}
                   , Coord {x=5,y=5}
                   , Coord {x=8,y=9}
                   ]
      length (planeDistance 32 coords) `shouldBe` 16

  describe "Part 2 with input from file" $ do
    file <- runIO $ readFile "test/day06.txt"
    let input = map coordFromString $ lines file

    it "solves the puzzle" $ do
      length (planeDistance 10000 input) `shouldBe` 38380
