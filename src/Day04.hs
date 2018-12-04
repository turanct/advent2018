module Day04 where

type GuardId = Int

data Event = BeganShift GuardId
           | FellAsleep
           | WokeUp
           deriving (Show, Eq)

data Time = Time { y :: Int
                 , m :: Int
                 , d :: Int
                 , h :: Int
                 , i :: Int
                 } deriving (Show, Eq, Ord)

data Log = Log { time :: Time
               , event :: Event
               } deriving (Show, Eq)
