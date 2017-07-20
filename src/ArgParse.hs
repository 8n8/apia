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
    StartIsInTheFuture |
    StopIsInTheFuture |
    UnhelpfulFail String |
    YouNeedAtLeastOneTag deriving Eq

i2f :: Int -> Float
i2f = fromIntegral

argParse :: Float -> [String] -> String -> Either BadCommand GoodCommand 
argParse _ ["now"] _ = Right Now
argParse _ ["clockedin"] _ = Right ClockedIn
argParse _ ["clockin"] _ = Left YouNeedAtLeastOneTag
argParse _ ("clockin":tags) _
    | null badTags = Right (ClockIn tags)
    | otherwise = Left (NumericTags badTags)
    where badTags = Dl.filter isNum tags
argParse _ ["clockout"] _ = Right ClockOut
argParse now ("daily":start:stop:tags) _ = 
    uncurry3 Daily <$> toCommand now start stop tags
argParse now ("dailymean":start:stop:tags) _ =
    uncurry3 DailyMean <$> toCommand now start stop tags
argParse now ["summary", start, stop] _ =
    uncurry Summary <$> lookForBadStartStop now start stop
argParse _ ("switch":tags) _ 
    | null badTags = Right (Switch tags)
    | otherwise = Left (NumericTags badTags)
    where badTags = Dl.filter isNum tags
argParse _ ["today"] _ = Right Today
argParse _ ["taglist"] _ = Right TagList
argParse now ("total":start:stop:tags) _ =
    uncurry3 Total <$> toCommand now start stop tags
argParse _ _ usage = Left (UnhelpfulFail usage)

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
toCommand :: Float -> String -> String -> [String] 
          -> Either BadCommand (Int,Int,[String])
toCommand now start stop tags
    | not . null $ badTags = Left (NumericTags badTags)
    | otherwise = 
      (\(a, o) -> (a, o, tags)) <$> lookForBadStartStop now start stop
    where badTags = Dl.filter isNum tags

lookForBadStartStop 
    :: Float -> String -> String -> Either BadCommand (Int,Int)
lookForBadStartStop now start stop =
    case (now, toInt start, toInt stop) of
        (_, Nothing, Just _) -> Left StartNotInt
        (_, Just _, Nothing) -> Left StopNotInt
        (_, Nothing, Nothing) -> Left BothStartAndStopNotInt
        (now', Just a, Just o) -> checkStartStopInPast now' a o

checkStartStopInPast :: Float -> Int -> Int -> Either BadCommand (Int, Int)
checkStartStopInPast now start stop 
    | now < i2f start = Left StartIsInTheFuture
    | now < i2f stop = Left StopIsInTheFuture
    | otherwise = Right (start, stop)

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
    show StartIsInTheFuture = "The start time you gave is \
        \in the future."
    show StopIsInTheFuture = "The stop time you gave is in \
        \the future."
    show BothStartAndStopNotInt = "Both the start and stop \
        \you gave were not in the right form.  They should \
        \be whole numbers."
    show YouNeedAtLeastOneTag = "You need to provide at \
        \least one tag."
    show (UnhelpfulFail usage) = "Apia 1.0.0 (2017-04-16) \n\
        \Copyright (C) 5-o 2017.  Licensed under \
        \the GNU General Public License Version 3.\n\n\
        \Usage examples:\n\n" ++ usage

isNum :: String -> Bool
isNum = Dl.all (`elem` "1234567890.")
