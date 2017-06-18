-- Copyright 2017 5-o

-- This file is part of Apia.

-- Apia is free software: you can redistribute it
-- and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at
-- your option) any later version.

-- Apia is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public
-- License for more details.

-- You should have received a copy of the GNU General
-- Public License along with Apia.  If not, see
-- <http://www.gnu.org/licenses/>.

-- DESCRIPTION

-- This module provides functions for extracting useful
-- data from the history of clocked sessions.

module AnalyseHistory where

import qualified ParseClockFile as P
import qualified Data.List as L 

truncFloat :: Float -> Int
truncFloat = truncate 

-- It calculates the work done each day for the work that
-- matches the input tags.  The tuples in the output contain
-- the day number in the first element and the work done
-- on that day in the second element.
dailyDurations :: P.Clocks 
               -> Int 
               -> Int 
               -> [String] 
               -> Float
               -> Either InternalError [(Int, Float)]
dailyDurations f start stop tags now = 
    mapM oneday [start..stop]
    where
        oneday :: Int -> Either InternalError (Int, Float)
        oneday x = (\a -> (x, a)) <$> daySum x
        -- It finds all the sessions whose tags include all 
        -- those given by the user.
        filtered :: [P.Session]
        filtered = filter (all1in2 tags . P.taglist) $ 
            P.sessions f
        -- It works out how much work done on a given day.
        daySum :: Int -> Either InternalError Float
        daySum day = sum <$> mapM (eachDay day now) filtered

-- It checks that all the elements in the first list
-- are also in the second list.
all1in2 :: [String] -> [String] -> Bool
all1in2 one two = L.all (`elem` two) one

data InternalError = EachDayBadPatternMatchOpen
                   | EachDayBadPatternMatchClosed
                   deriving (Eq, Show)

-- It takes in a day number and a session and gives the
-- amount of the session's time that was on that day.
eachDay :: Int -> Float -> P.Session 
        -> Either InternalError Float
eachDay day now (P.Session _ begin P.Open)
    | begin `before` day = Right (now - i2f day)
    | begin `during` day = Right (now - begin)
    | begin `after` day = Right 0
    | otherwise = Left EachDayBadPatternMatchOpen
eachDay day _ (P.Session _ begin (P.Closed end))
    | begin `after` day = Right 0
    | end `before` day = Right 0
    | begin `before` day, end `during` day = 
          Right (end - i2f day)
    | begin `before` day, end `after` day = Right 1
    | begin `during` day, end `during` day = 
          Right (end - begin)
    | begin `during` day, end `after` day =
          Right (i2f day + 1 - begin)
    | otherwise = Left EachDayBadPatternMatchClosed
    
before, during, after :: Float -> Int -> Bool
before x day = x < i2f day
during x day = x >= i2f day && x <= i2f day + 1
after x day = x > i2f day + 1
    
i2f :: Int -> Float
i2f = fromIntegral

-- It uses the dailyDurations function to calculate the
-- amount of work done today for each tag used today.
today :: P.Clocks -> Float 
      -> Either InternalError [(String,Int)]
today f now = (++) <$> breakdown <*> totaltime
    where
        totaltime :: Either InternalError [(String, Int)]
        totaltime = (\a -> [("total", a)]) <$> tagsum []
        breakdown :: Either InternalError [(String,Int)]
        breakdown = mapM onetag tags 
        onetag :: String -> 
                  Either InternalError (String, Int)
        onetag tag = (\a -> (tag, a)) <$> tagsum [tag]
        tags :: [String]
        tags = getTodaysTags now (P.sessions f)
        -- The number of millidays spent today on the given 
        -- tag.
        tagsum :: [String] -> Either InternalError Int
        tagsum tags' = 
            (truncate . (1000*) . snd . head) <$>  
                dailyDurations f dayNum dayNum tags' now
        dayNum :: Int
        dayNum = truncate now

-- It finds all the tags in the clock file that are attached
-- to sessions that have a beginning, end or both today.  
-- All sessions with open clocks are included.
getTodaysTags :: Float -> [P.Session] -> [String]
getTodaysTags now = L.nub . concatMap P.taglist . filter test
    where 
        test s = (truncFloat . P.begin) s == truncate now ||
                 P.end s == P.Open
  
-- It finds the mean daily time for the tags in
-- the arguments.
daymean :: P.Clocks -> Int -> Int -> [String] -> Float
        -> Either InternalError Float
daymean f start stop tags now = 
    (/ period) <$> (sum <$> byday) 
    where
        -- It is a list of the daily totals of work matching
        -- the tags.
        byday :: Either InternalError [Float]
        byday = 
            map snd <$> dailyDurations f start stop tags now
        -- It is the period of time the mean is taken over.
        -- The +1 is so that it counts both ends of the 
        -- period.
        period :: Float
        period = fromIntegral $ stop - start + 1

-- It makes a list of the tags in the clock file.
getTagList :: [P.Session] -> [String]
getTagList = L.nub . concatMap P.taglist

-- It makes a list of the tags in the argument that are
-- not in the clock file.
newtags :: [P.Session] -> [String] -> [String]
newtags s tags = filter (`notElem` oldtags) $ L.nub tags
    where oldtags = getTagList s
