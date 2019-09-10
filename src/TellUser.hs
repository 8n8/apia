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
import Text.Printf (printf)

data TellUser =
    HereIsTheClockState P.ClockFileState |
    HereIsYourDailyChart [(Int,Float)] |
    HereIsYourDailyMean Float |
    HereIsYourSummary [(String,Float)] P.ClockFileState |
    HereIsYourTagList [String] |
    HereIsYourTodayChart [(String,Float)] P.ClockFileState |
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

instance Show TellUser where
    show (HereIsTheClockState x) = show x
    show (HereIsYourDailyChart table) =
        L.intercalate "\n" $ map makeBarForGraphic table
    show (HereIsYourDailyMean mean) = printf "%.5f" mean
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
printSummaryChart :: P.ClockFileState -> [(String,Float)] -> [String]
printSummaryChart c xs =
    zipWith makeString sortxs percents ++ [show c]
  where
    sortxs :: [(String, Float)]
    sortxs = (L.sortBy sorter . init) xs ++ [last xs]
    longestTag :: Int
    longestTag = maximum $ map (length . fst) sortxs
    longestNum :: Int
    longestNum = maximum $ map (length . show . snd) sortxs
    total :: Float
    total = snd . last $ sortxs
    percents :: [String]
    percents = map (makePercent total . snd) sortxs
    maxPercents = maximum $ map length percents 
    makeString :: (String, Float) -> String -> String
    makeString (tag, dur) percent =
        toNleft longestTag tag ++ "    " ++ 
        (toNright longestNum $ printf "%.5f" dur) ++ "    " ++
        toNright maxPercents percent

sorter :: (String, Float) -> (String, Float) -> Ordering
sorter (_, first) (_, second) 
    | first > second = LT
    | first < second = GT
    | otherwise = EQ

makePercent :: Float -> Float -> String
makePercent 0 _ = "0%"
makePercent total num =
    percent ++ "%"
  where 
    percent :: String
    percent = toNright 6 $ printf "%.2f" $ num * 100 / total
  
-- It takes in the data for one day, which is the day number and the
-- time period, and turns it into a bar of a bar chart, like: 
-- "174   34 ###" (without the quotes).
makeBarForGraphic :: (Int,Float) -> String
makeBarForGraphic (day,duration) =
    show day ++ (prettyDuration duration) ++
    ' ':replicate barLen '#'
  where
    -- The multiplier of 75 is about right so that the bars are a
    -- reasonable length but not too long.
    barLen :: Int
    barLen = truncate (duration * 75)

prettyDuration :: Float -> String
prettyDuration d =
    toNright 10 $ printf "%.5f" d

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
