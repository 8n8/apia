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

-- This module parses the command-line arguments.

module ArgParse 
    ( GoodCommand (..)
    , BadCommand (..)
    , argParse
    ) where

import qualified Data.List as Dl
import qualified Text.Read as Tr

data GoodCommand = 
    ClockedIn |
    ClockIn [String] |
    ClockOut |
    Daily Int Int [String] |
    DailyMean Int Int [String] |
    Now |
    Switch [String] |
    Summary Int Int |
    TagList |
    Today |
    Total Int Int [String] deriving (Eq, Show)

data BadCommand = 
    BothStartAndStopNotInt |
    NumericTags [String] |
    StartNotInt |
    StopNotInt |
    UnhelpfulFail  |
    YouNeedAtLeastOneTag deriving Eq

argParse :: [String] -> Either BadCommand GoodCommand 
argParse ["now"] = Right Now
argParse ["clockedin"] = Right ClockedIn
argParse ["clockin"] = Left YouNeedAtLeastOneTag
argParse ("clockin":tags) 
    | null badTags = Right (ClockIn tags)
    | otherwise = Left (NumericTags badTags)
    where badTags = Dl.filter isNum tags
argParse ["clockout"] = Right ClockOut
argParse ("daily":start:stop:tags) = 
    uncurry3 Daily <$> toCommand start stop tags
argParse ("dailymean":start:stop:tags) =
    uncurry3 DailyMean <$> toCommand start stop tags
argParse ["summary", start, stop] =
    uncurry Summary <$> lookForBadStartStop start stop
argParse ("switch":tags) 
    | null badTags = Right (Switch tags)
    | otherwise = Left (NumericTags badTags)
    where badTags = Dl.filter isNum tags
argParse ["today"] = Right Today
argParse ["taglist"] = Right TagList
argParse ("total":start:stop:tags) =
    uncurry3 Total <$> toCommand start stop tags
argParse _ = Left UnhelpfulFail 

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

-- Like ordinary 'uncurry', but for functions with three 
-- inputs instead of two.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f p = f (fst3 p) (snd3 p) (thd3 p)

-- Some of the commands end with <start time>, <stop time>,
-- <list of tags>.  This function is used for checking
-- that part and turning it into the right form.
toCommand :: String -> String -> [String] 
          -> Either BadCommand (Int,Int,[String])
toCommand start stop tags
    | not . null $ badTags = Left (NumericTags badTags)
    | otherwise = 
      (\(a, o) -> (a, o, tags)) <$> lookForBadStartStop start stop
    where badTags = Dl.filter isNum tags

lookForBadStartStop 
    :: String -> String -> Either BadCommand (Int,Int)
lookForBadStartStop start stop =
    case (toInt start, toInt stop) of
        (Nothing, Just _) -> Left StartNotInt
        (Just _, Nothing) -> Left StopNotInt
        (Nothing, Nothing) -> Left BothStartAndStopNotInt
        (Just a, Just o) -> Right (a, o) 

toInt :: String -> Maybe Int
toInt x = Tr.readMaybe x :: Maybe Int

instance Show BadCommand where 
    show (NumericTags xs) = "Tags should have at least one \
        \non-numeric character in them.  There was a \
        \problem with the following:\n" ++ 
        Dl.intercalate "\n" xs
    show StartNotInt = "The start time you gave was not in \
        \the right form.  It should be a whole number."
    show StopNotInt = "The stop time you gave was not in \
        \the right form.  It should be a whole number."
    show BothStartAndStopNotInt = "Both the start and stop \
        \you gave were not in the right form.  They should \
        \be whole numbers."
    show YouNeedAtLeastOneTag = "You need to provide at \
        \least one tag."
    show UnhelpfulFail = "Apia 1.0.0 (2017-04-16) \n\n\
        \Copyright (C) 5-o 2017.  Licensed under \
        \the GNU General Public License Version 3.\n\
        \\n\
        \USAGE\n=====\n\n\
        \Find out if clocked in:\n\
        \apia clockedin\n\
        \\n\
        \Clock in:\n\
        \apia clockin <tag list>\n\
        \\n\
        \Clock out:\n\
        \apia clockout\n\
        \\n\
        \Make a day-by-day work bar chart:\n\
        \apia daily <start day> <end day> <tag list>\n\
        \\n\
        \Find the average daily mean work:\n\
        \apia dailymean <start day> <end day> <tag list>\n\
        \\n\
        \Make a summary chart for a time period:\n\
        \apia summary <start day> <end day>\n\
        \\n\
        \Make a chart showing the work done today:\n\
        \apia today\n\
        \\n\
        \Make a list of all the current tags:\n\
        \apia taglist\n\
        \\n\
        \Work out the total work done during a period:\n\
        \apia total <start day> <end day> <tag list> \n\n\
        \Note that the time used is in days since 00:00 on \
        \1 October 2016, so there are no years, months, \
        \weeks hours, minutes or seconds.  This is a lot \
        \more convenient when you get used to it.  The \
        \tags lists should be separated by spaces.  Each \
        \tag excludes all sessions that do not have the \
        \tag, so no tags means that everything is included."

isNum :: String -> Bool
isNum = Dl.all (`elem` "1234567890.")
