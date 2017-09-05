-- Copyright 2017 5-o

-- This file is part of Apia.

-- Apia is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Apia is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Apia.  If not, see <http://www.gnu.org/licenses/>.

-- DESCRIPTION

-- This module provides a type that represents messages
-- to the user.

module TellUser
    ( TellUser (..)
    ) where

import qualified ArgParse as A
import qualified Data.List as L
import qualified ParseClockFile as P

data TellUser =
    HereIsTheClockState P.ClockFileState |
    HereIsYourDailyChart [(Int,Float)] |
    HereIsYourDailyMean Int |
    HereIsYourSummary [(String,Int)] P.ClockFileState |
    HereIsYourTagList [String] |
    HereIsYourTodayChart [(String,Int)] P.ClockFileState |
    HereIsYourTotal Float |
    TheClockFileIsBad P.BadLines |
    TheClockFileIsEmpty |
    YouAreAlreadyClockedIn [String] |
    YouAreAlreadyClockedOut |
    YouCantSwitchWhenYoureClockedOut |
    YouCantSwitchToYourCurrentTask |
    YouGaveBadArgs A.BadCommand |
    YouHaveMadeNewTags [String] |
    TheTimeIs Float
        deriving Eq

i2f :: Int -> Float
i2f = fromIntegral

instance Show TellUser where
    show (HereIsTheClockState x) = show x
    show (HereIsYourDailyChart table) =
        L.intercalate "\n" $ map makeBarForGraphic table
    show (HereIsYourDailyMean mean) = show mean
    show (HereIsYourTagList tags) = L.intercalate "\n" tags
    show (HereIsYourTodayChart table cfs) =
        L.intercalate "\n" $ printSummaryChart cfs table
    show (HereIsYourSummary table cfs) =
        L.intercalate "\n" $ printSummaryChart cfs table
    show (HereIsYourTotal total) = 
        take 7 (show total) ++ " days"
    show TheClockFileIsEmpty = "The clock file is empty."
    show (TheClockFileIsBad msg) = show msg
    show (YouAreAlreadyClockedIn tags) = "You are already clocked \
        \in.  The tag" ++
        (if length tags == 1 then " is" else "s are") ++
        ":\n" ++ unwords tags
    show YouAreAlreadyClockedOut = "You are already clocked out."
    show YouCantSwitchWhenYoureClockedOut = "You are already clocked \
        \out, so you can't use the 'switch' option.  Perhaps you want \
        \to use 'clockin' instead."
    show YouCantSwitchToYourCurrentTask = "You are already clocked \
        \in and working on that task."
    show (YouGaveBadArgs msg) = show msg
    show (YouHaveMadeNewTags tags) = newTagMsg tags
    show (TheTimeIs t) = show t

-- It makes the chart that shows how the time has been spent today.  
-- The inputs are the clock file state and a list of tag-time pairs.
printSummaryChart :: P.ClockFileState -> [(String,Int)] -> [String]
printSummaryChart c xs =
    zipWith makeString sortxs percents ++ [show c]
  where
    sortxs = (L.sortBy sorter . init) xs ++ [last xs]
    longestTag = maximum $ map (length . fst) sortxs
    longestNum = maximum $ map (length . show . snd) sortxs
    total = snd . last $ sortxs
    percents = map (makePercent total . snd) sortxs
    maxPercents = maximum $ map length percents 
    makeString :: (String, Int) -> String -> String
    makeString (tag, dur) percent =
        toNleft longestTag tag ++ "    " ++ 
        formatNum (i2f dur) longestNum ++ "    " ++
        toNright maxPercents percent

sorter :: (String, Int) -> (String, Int) -> Ordering
sorter (_, first) (_, second) 
    | first > second = LT
    | first < second = GT
    | otherwise = EQ

makePercent :: Int -> Int -> String
makePercent 0 _ = "0%"
makePercent total num =
    show percent ++ "%"
  where 
    percent :: Int
    percent = truncate $ int2float num * 100 / int2float total

int2float :: Int -> Float
int2float = fromIntegral

-- It takes a number, chops off everything after the decimal point,
-- and changes it to a string.
num2str :: Float -> String
num2str x = show (truncFloat x)

truncFloat :: Float -> Int
truncFloat = truncate
  
-- It takes in the data for one day, which is the day number and the
-- time period, and turns it into a bar of a bar chart, like: 
-- "174   34 ###" (without the quotes).
makeBarForGraphic :: (Int,Float) -> String
makeBarForGraphic (day,duration) =
    show day ++ formatNum (duration * 1000) 5 ++
    ' ':replicate barLen '#'
  where
    -- The multiplier of 75 is about right so that the bars are a
    -- reasonable length but not too long.
    barLen :: Int
    barLen = truncate (duration * 75)

-- It makes a float into a nice shortened string padded with spaces.
formatNum :: Float -> Int -> String
formatNum num n = toNright n (num2str num)

-- It increases the length of a string to a given length by adding
-- spaces before it.
toNright :: Int -> String -> String
toNright k x = if length x < k then toNright k (' ':x) else x

-- It increases the length of a string to a given length by adding
-- spaces at the end of it.
toNleft :: Int -> String -> String
toNleft k x = if length x < k then toNleft k (x ++ " ") else x

-- It makes an message to show the user a list of the new tags.
newTagMsg :: [String] -> String
newTagMsg tags = "Note that you have made " ++ plural ++
    ":\n" ++ unwords tags
  where
    len = length tags
    plural :: String
    plural = 
        if len == 1
        then "a new tag"
        else niceNum len ++ " new tags"

niceNum :: Int -> String
niceNum x = case x of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    n -> show n
