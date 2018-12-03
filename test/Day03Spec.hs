module Day03Spec where

import Test.Hspec
import Day03
import Prelude hiding (id)

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "parses strings to claims" $ do
      let strings = [ "#123 @ 3,2: 5x4"
                   , "#1 @ 1,3: 4x4"
                   , "#2 @ 3,1: 4x4"
                   , "#3 @ 5,5: 2x2"
                   ]
      let claims = [ Claim { id = 123, left = 3, top = 2, width = 5, height = 4 }
                   , Claim { id = 1, left = 1, top = 3, width = 4, height = 4 }
                   , Claim { id = 2, left = 3, top = 1, width = 4, height = 4 }
                   , Claim { id = 3, left = 5, top = 5, width = 2, height = 2 }
                   ]
      map claimFromString strings `shouldBe` claims

    it "converts claims to areas" $ do
      let claims = [ Claim { id = 123, left = 3, top = 2, width = 5, height = 4 }
                   , Claim { id = 1, left = 1, top = 3, width = 4, height = 4 }
                   , Claim { id = 2, left = 3, top = 1, width = 4, height = 4 }
                   , Claim { id = 3, left = 5, top = 5, width = 2, height = 2 }
                   ]
      let areas = [ [Coord{x=4,y=3},Coord{x=4,y=4},Coord{x=4,y=5},Coord{x=4,y=6}
                    ,Coord{x=5,y=3},Coord{x=5,y=4},Coord{x=5,y=5},Coord{x=5,y=6}
                    ,Coord{x=6,y=3},Coord{x=6,y=4},Coord{x=6,y=5},Coord{x=6,y=6}
                    ,Coord{x=7,y=3},Coord{x=7,y=4},Coord{x=7,y=5},Coord{x=7,y=6}
                    ,Coord{x=8,y=3},Coord{x=8,y=4},Coord{x=8,y=5},Coord{x=8,y=6}
                    ]
                  , [Coord{x=2,y=4},Coord{x=2,y=5},Coord{x=2,y=6},Coord{x=2,y=7}
                    ,Coord{x=3,y=4},Coord{x=3,y=5},Coord{x=3,y=6},Coord{x=3,y=7}
                    ,Coord{x=4,y=4},Coord{x=4,y=5},Coord{x=4,y=6},Coord{x=4,y=7}
                    ,Coord{x=5,y=4},Coord{x=5,y=5},Coord{x=5,y=6},Coord{x=5,y=7}
                    ]
                  , [Coord{x=4,y=2},Coord{x=4,y=3},Coord{x=4,y=4},Coord{x=4,y=5}
                    ,Coord{x=5,y=2},Coord{x=5,y=3},Coord{x=5,y=4},Coord{x=5,y=5}
                    ,Coord{x=6,y=2},Coord{x=6,y=3},Coord{x=6,y=4},Coord{x=6,y=5}
                    ,Coord{x=7,y=2},Coord{x=7,y=3},Coord{x=7,y=4},Coord{x=7,y=5}
                    ]
                  , [Coord{x=6,y=6},Coord{x=6,y=7}
                    ,Coord{x=7,y=6},Coord{x=7,y=7}
                    ]
                  ]
      map toArea claims `shouldBe` areas

    it "counts overlappings for areas" $ do
      let input = [ "#1 @ 1,3: 4x4"
                  , "#2 @ 3,1: 4x4"
                  , "#3 @ 5,5: 2x2"
                  ]
      overlappingSquares input `shouldBe` 4

  describe "Part 1 with input from file" $ do
    file <- runIO $ readFile "test/day03.txt"

    it "solves the puzzle" $ do
      let input = lines file
      overlappingSquares input `shouldBe` 112378

  describe "Part 2" $ do
    it "checks if two claims overlap 1" $ do
      let claim1 = Claim { id = 1, left = 1, top = 3, width = 4, height = 4 }
      let claim2 = Claim { id = 2, left = 3, top = 1, width = 4, height = 4 }
      overlapsWith claim1 claim2 `shouldBe` True

    it "checks if two claims overlap 2" $ do
      let claim2 = Claim { id = 2, left = 3, top = 1, width = 4, height = 4 }
      let claim3 = Claim { id = 3, left = 5, top = 5, width = 2, height = 2 }
      overlapsWith claim2 claim3 `shouldBe` False

    it "finds claims without overlaps" $ do
      let input = [ "#1 @ 1,3: 4x4"
                  , "#2 @ 3,1: 4x4"
                  , "#3 @ 5,5: 2x2"
                  ]
      nonOverlappingClaims input `shouldBe` [3]

  -- describe "Part 2 with input from file" $ do
  --   file <- runIO $ readFile "test/day03.txt"

  --   it "solves the puzzle" $ do
  --     let input = lines file
  --     nonOverlappingClaims input `shouldBe` [603]
