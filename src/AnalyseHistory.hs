-- Copyright 2017 5-o

-- This file is part of Apia.

-- Apia is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- Apia is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with Apia.  If not, see <http://www.gnu.org/licenses/>.

-- DESCRIPTION

-- This module provides functions for extracting useful data from
-- the history of clocked sessions.

module AnalyseHistory 
    ( dailyDurations
    , newtags
    , getTagList
    , getTagsForPeriod
    , summary
    , daymean
    ) where

import qualified ParseClockFile as P
import qualified Data.List as L 

-- It calculates the work done each day for the work that matches the
-- input tags.  The tuples in the output contain the day number in
-- the first element and the work done on that day in the second
-- element.
dailyDurations ::
    P.Clocks -> Int -> Int -> [String] -> Float ->
    [(Int, Float)]
dailyDurations f start stop tags now =
    map oneday [start..stop]
  where
    oneday :: Int -> (Int, Float)
    oneday x = (\a -> (x, a)) $ daySum x
    -- It finds all the sessions whose tags include all 
    -- those given by the user.
    filtered :: [P.Session]
    filtered = filter (all1in2 tags . P.taglist) $ P.sessions f
    -- It works out how much work done on a given day.
    daySum :: Int -> Float
    daySum day = sum $ map (eachDay day now) filtered

-- It checks that all the elements in the first list are also in the
-- second list.
all1in2 :: [String] -> [String] -> Bool
all1in2 one two = L.all (`elem` two) one

total :: [P.Session] -> Float -> Int -> Int -> Float
total s now start stop =
    sum $ map sesstot s 
  where  
    sesstot :: P.Session -> Float
    sesstot sess = sum $ map (\d -> eachDay d now sess) [start..stop]
    
-- It takes in a day number and a session and gives the amount of
-- the session's time that was on that day.
eachDay :: Int -> Float -> P.Session -> Float
eachDay dayInt now (P.Session _ begin end) =
    rangeOverlap (daystart, dayend) (begin, endFloat)
  where
    daystart = i2f dayInt
    dayend = daystart + 1
    endFloat = sessEnd end now

sessEnd :: P.ClockEnd -> Float -> Float
sessEnd P.Open now = now
sessEnd (P.Closed end) _ = end

rangeOverlap :: (Float, Float) -> (Float, Float) -> Float
rangeOverlap (a, b) (c, d) = max 0 overlap
  where
    overlap = (b - a) + (d - c) - max b d + min a c

i2f :: Int -> Float
i2f = fromIntegral

-- It takes in the contents of the clock file after parsing, the
-- current time, and a start and stop day.  The result is a list of
-- tuples, each tuple containing a unique tag in its first element,
-- and the total work done on it in milliDays in its second element.
summary :: P.Clocks -> Float -> Int -> Int -> [(String,Int)]
summary f now start stop = breakdown ++ totaltime
  where
    breakdown = totalOnEachTag f now start stop
    totaltime :: [(String, Int)]
    totaltime = (\a -> [("total", a)]) tot
    tot :: Int
    tot = (truncate . (1000*)) $ total (P.sessions f) now start stop

-- It works out the total time spent on each tag in the given time
-- period.
totalOnEachTag :: P.Clocks -> Float -> Int -> Int -> [(String,Int)]
totalOnEachTag f now start stop =
    map onetag tags 
  where
    -- The total spent on one tag.
    onetag :: String -> (String, Int)
    onetag tag = (\a -> (tag, a)) $ tagsum tag
    tags :: [String]
    tags = getTagsForPeriod now start stop (P.sessions f)
    tagsum :: String -> Int
    tagsum tag = 
        truncate . (1000*) . sum . map snd $
            dailyDurations f start stop [tag] now

-- It finds all the tags in the clock file that are attached
-- to sessions that have at least part of their time in the given
-- time range.
--
-- This diagram shows the various alternatives.  The time range end 
-- points are marked with 'X's:
-- 
--                 |<----Session---->|
-- 1)   X     X    |                 |
-- 2)              |                 |   X        X
-- 3)              |   X    X        |
-- 4)        X     |                 |    X
-- 5)    X         |    X            |
-- 6)              |         X       |         X
-- 
-- Only (1) and (2) should be excluded, that is, tasks that have both
-- start and stop times outside the session.
getTagsForPeriod :: Float -> Int -> Int -> [P.Session] -> [String]
getTagsForPeriod now start stop = 
    L.sort . L.nub . concatMap P.taglist . filter test
  where
    test :: P.Session -> Bool
    test s = not (
        let 
            sessionEnd = 
                case P.end s of
                    P.Open -> now
                    P.Closed t -> t
        in
            i2f stop + 1 <= P.begin s || 
              i2f start >= sessionEnd
        )
  
-- It finds the mean daily time for the tags in the arguments.
daymean :: P.Clocks -> Int -> Int -> [String] -> Float -> Float
daymean f start stop tags now = 
    sum byday / period
  where
    -- It is a list of the daily totals of work matching the tags.
    byday :: [Float]
    byday = map snd $ dailyDurations f start stop tags now
    -- It is the period of time the mean is taken over.  The +1 is
    -- so that it counts both ends of the period.
    period :: Float
    period = fromIntegral $ stop - start + 1

-- It makes a list of the tags in the clock file.
getTagList :: [P.Session] -> [String]
getTagList = L.sort . L.nub . concatMap P.taglist

-- It makes a list of the tags in the argument that are not in the
-- clock file.
newtags :: [P.Session] -> [String] -> [String]
newtags s tags = L.sort . filter (`notElem` oldtags) $ L.nub tags
    where oldtags = getTagList s
