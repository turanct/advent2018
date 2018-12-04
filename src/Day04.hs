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

-- [1518-11-21 00:01] Guard #641 begins shift
-- [1518-05-29 00:31] falls asleep
-- [1518-02-20 00:40] wakes up
logFromString :: String -> Log
logFromString s = Log { time = Time { y = y, m = m, d = d, h = h, i = i }, event = event }
   where y = read $ takeWhile (/= '-') $ drop 1 s
         m = read $ takeWhile (/= '-') $ dropWhile (== '0') $ drop 1 $ dropWhile (/= '-') s
         d = read $ takeWhile (/= ' ') $ dropWhile (== '0') $ drop 1 $ dropWhile (/= '-') $ drop 1 $ dropWhile (/= '-') s
         h = read $ takeWhile (/= ':') $ drop 1 $ dropWhile (/= ' ') s
         i = read $ takeWhile (/= ']') $ drop 1 $ dropWhile (/= ':') s
         event = case rest of "falls asleep" -> FellAsleep
                              "wakes up" -> WokeUp
                              x -> BeganShift (read $ takeWhile (/= ' ') $ drop 1 $ dropWhile (/= '#') x)
         rest = drop 2 $ dropWhile (/= ']') s

logsFromStrings :: [String] -> [Log]
logsFromStrings = map logFromString
