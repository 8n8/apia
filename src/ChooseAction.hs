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

-- It converts the raw input into a record containing the message
-- for the user and the string to write in the clock file.

module ChooseAction 
    ( chooseActions
    , Actions (..)
    ) where 

import qualified AnalyseHistory as A
import qualified ArgParse as G
import qualified Data.List as L
import qualified ParseClockFile as P
import qualified TellUser as T

data Actions = Actions
    { msg :: Maybe T.TellUser
    , toFile :: Maybe Do2File } deriving (Eq, Show)

data Do2File =
    WriteClockOut Float |
    WriteClockIn Float [String] |
    WriteSwitch Float [String] deriving Eq

instance Show Do2File where
    show (WriteClockOut now) = ' ':show now ++ "\n"
    show (WriteClockIn now tags) = clockLn tags now
    show (WriteSwitch now tags) = 
        ' ':show now ++ "\n" ++ clockLn tags now

-- This does everything.  It takes in the current time, the list of
-- input arguments, the data file contents and the usage file contents,
-- and gives a record containing a message to print out and a string
-- to append to the clock file.
chooseActions :: Float -> [String] -> String -> Actions
chooseActions now args fileContents = 
    -- The fileData variable is a nested Either expression.  The
    -- outer layer contains internal errors, such as unlikely pattern
    -- matches, and the inner layer contains errors in the clock file.
    case (fileData, parsedArgs) of
        (_, Left a) -> Actions 
            { msg = Just (T.YouGaveBadArgs a)
            , toFile = Nothing } 
        (Left f, _) -> Actions 
            { msg = Just (T.TheClockFileIsBad f)
            , toFile = Nothing }
        (Right f, Right a) -> switcher now f a
  where 
    fileData :: Either P.BadLines P.Clocks
    fileData = P.parseClockFile now fileContents
    parsedArgs = G.argParse now args

-- It makes the line to write to the clock file when clocking in.
clockLn :: [String] -> Float -> String
clockLn tags now = unwords tags ++ ' ':show now

-- It chooses what action to take depending on the input
-- arguments.  It does not deal with errors.  These should have been
-- sorted beforehand.
switcher ::
    Float ->  -- The current time.
    P.Clocks ->  -- The parsed data file.
    G.GoodCommand ->  -- The parsed command-line arguments.
    Actions
switcher _ (P.Clocks _ state) G.ClockedIn = Actions 
    { msg = Just (T.HereIsTheClockState state)
    , toFile = Nothing }
switcher t (P.Clocks s P.AllClocksClosed) (G.ClockIn tags) =
    Actions
    { msg = if not (null new)
            then Just (T.YouHaveMadeNewTags new)
            else Nothing
    , toFile = Just (WriteClockIn t tags) }
    where new = A.newtags s tags
switcher t (P.Clocks s P.Empty) (G.ClockIn tags) =
    Actions
    { msg = if not (null new)
            then Just (T.YouHaveMadeNewTags new)
            else Nothing
    , toFile = Just (WriteClockIn t tags) }
    where new = A.newtags s tags
switcher _ (P.Clocks s (P.LastClockOpen _)) (G.ClockIn _) =
    Actions
    { msg = Just (T.YouAreAlreadyClockedIn tags)
    , toFile = Nothing }
    where tags = P.taglist . last $ s
switcher _ (P.Clocks _ P.AllClocksClosed) G.ClockOut = 
    Actions
    { msg = Just T.YouAreAlreadyClockedOut
    , toFile = Nothing }
switcher _ (P.Clocks _ P.Empty) G.ClockOut = 
    Actions
    { msg = Just T.TheClockFileIsEmpty
    , toFile = Nothing }
switcher t (P.Clocks _ (P.LastClockOpen _)) G.ClockOut = 
    Actions
    { msg = Nothing
    , toFile = Just (WriteClockOut t) }
switcher _ (P.Clocks _ P.Empty) G.Daily{} = Actions
    { msg = Just T.TheClockFileIsEmpty
    , toFile = Nothing }
switcher t f (G.Daily start stop tags) = Actions 
    { msg = Just (T.HereIsYourDailyChart 
            (A.dailyDurations f start stop tags t))
    , toFile = Nothing }
switcher _ (P.Clocks _ P.Empty) G.DailyMean{} = Actions
    { msg = Just T.TheClockFileIsEmpty
    , toFile = Nothing }
switcher t f (G.DailyMean start stop tags) = Actions 
    { msg = Just (T.HereIsYourDailyMean (
        (truncate . (1000 *)) $ 
            A.daymean f start stop tags t))
    , toFile = Nothing }
switcher _ (P.Clocks _ P.Empty) (G.Switch _) = Actions
    { msg = Just T.TheClockFileIsEmpty
    , toFile = Nothing }
switcher _ (P.Clocks _ P.AllClocksClosed) (G.Switch _) =
    Actions
    { msg = Just T.YouCantSwitchWhenYoureClockedOut
    , toFile = Nothing }
switcher t (P.Clocks s (P.LastClockOpen oldtags)) (G.Switch tags) =
    if (L.nub . L.sort) tags == (L.nub . L.sort) oldtags
    then 
        Actions
        { msg = Just T.YouCantSwitchToYourCurrentTask
        , toFile = Nothing}
    else
        Actions
        { msg = if not (null new) 
                then Just (T.YouHaveMadeNewTags new)
                else Nothing
        , toFile = Just (WriteSwitch t tags) }
    where new = A.newtags s tags
switcher t f@(P.Clocks _ state) G.Today = Actions  
    { msg = Just (T.HereIsYourTodayChart (A.summary f t d d) state)
    , toFile = Nothing } 
    where d = truncate t
switcher t f@(P.Clocks _ state) (G.Summary a o) = Actions
    { msg = Just (T.HereIsYourSummary (A.summary f t a o) state)
    , toFile = Nothing }
switcher _ (P.Clocks s _) G.TagList = Actions 
    { msg = Just (T.HereIsYourTagList (A.getTagList s))
    , toFile = Nothing }
switcher t f (G.Total start stop tags) = Actions 
    { msg = Just (T.HereIsYourTotal ((sum . map snd) $ 
         A.dailyDurations f start stop tags t))
    , toFile = Nothing }
switcher t _ G.Now = Actions
    { msg = Just (T.TheTimeIs t)
    , toFile = Nothing }
