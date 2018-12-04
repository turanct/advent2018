module Day04 where

import Data.List

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

instance Ord Log where
  log1 `compare` log2 = time log1 `compare` time log2

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

data GuardState = Awake GuardId [Time]
                | Sleeping GuardId Time [Time]
                deriving (Show, Eq)

changeGuardState :: GuardState -> Log -> GuardState
changeGuardState (Awake gid mins) Log {time=start, event=FellAsleep} =
  Sleeping gid start mins
changeGuardState (Awake gid mins) Log {time=t, event=_} =
  Awake gid mins
changeGuardState (Sleeping gid start mins) Log {time=end, event=WokeUp} =
  Awake gid (minsBetween start end ++ mins)
changeGuardState (Sleeping gid start mins) Log {time=t, event=_} =
  Sleeping gid start mins

minsBetween :: Time -> Time -> [Time]
minsBetween a b = [ Time { y=y a, m=m a, d=d a, h=h a, i=i } | i <- [i a..i b - 1] ]

statesFromLogs :: [Log] -> [GuardState]
statesFromLogs = reverse . foldl f []
  where f shifts Log{time=t, event=(BeganShift gid)} = Awake gid [] : shifts
        f shifts l = changeGuardState (head shifts) l : tail shifts

data Shift = Shift { guard :: GuardId
                   , minutesAsleep :: [Time]
                   } deriving (Show, Eq, Ord)

shiftsFromStates :: [GuardState] -> [Shift]
shiftsFromStates = map f
  where f (Awake gid mins) = Shift { guard = gid, minutesAsleep = mins }
        f (Sleeping gid _ mins) = Shift { guard = gid, minutesAsleep = mins }

sameGuard :: Shift -> Shift -> Bool
sameGuard a b = guard a == guard b

joinShifts :: Shift -> Shift -> Shift
joinShifts a b = Shift { guard = guard a, minutesAsleep = minutesAsleep a ++ minutesAsleep b }

shiftWithMostSleepingMinutes :: [Shift] -> Shift
shiftWithMostSleepingMinutes = foldl1 most
  where most a b = if (length . minutesAsleep) a > (length . minutesAsleep) b then a else b

mostCommonMinute :: Shift -> Int
mostCommonMinute = head . longestList . group . sort . map i . minutesAsleep

longestList :: [[a]] -> [a]
longestList [] = []
longestList [x] = x
longestList (a:b:cs)
  | length a > length b = longestList (a:cs)
  | otherwise = longestList (b:cs)

solvePart1 :: [String] -> Int
solvePart1 ss = sleepingGuard * minute
  where sleepingGuard = guard shift
        minute = mostCommonMinute shift
        shift = shiftWithMostSleepingMinutes
              $ map (foldl1 joinShifts)
              $ groupBy sameGuard
              $ sort
              $ shiftsFromStates
              $ statesFromLogs
              $ sort
              $ logsFromStrings ss

shiftWithMostCommonMinutes :: [Shift] -> Shift
shiftWithMostCommonMinutes = foldl1 most
  where most a b = if (length . cm) a > (length . cm) b then a else b
        cm = longestList . group . sort . map i . minutesAsleep

solvePart2 :: [String] -> Int
solvePart2 ss = sleepingGuard * minute
  where sleepingGuard = guard shift
        minute = mostCommonMinute shift
        shift = shiftWithMostCommonMinutes
              $ map (foldl1 joinShifts)
              $ groupBy sameGuard
              $ sort
              $ shiftsFromStates
              $ statesFromLogs
              $ sort
              $ logsFromStrings ss
