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

-- This module parses the file that contains the clock
-- records.

module ParseClockFile
    (
      BadLines (..)
    , ClockEnd (..)
    , ClockFileState (..)
    , Clocks (..)
    , Session (..)
    , parseClockFile
    ) where

import qualified Data.Either as E 
import qualified Data.List as L

data Session = Session 
    { taglist :: [String]
    , begin :: Float
    , end :: ClockEnd
    } deriving (Eq, Show)

data ClockEnd = Open | Closed Float deriving (Eq, Show)

data Clocks = Clocks 
    { sessions :: [Session]
    , clockstate :: ClockFileState } 
    deriving (Eq, Show)

data ClockFileState = 
    LastClockOpen [String] |
    AllClocksClosed |
    Empty deriving Eq

instance Show ClockFileState where
    show (LastClockOpen []) = "Internal error:  Can't show \
        \LastClockOpen tags because there aren't any."
    show (LastClockOpen tags@[_]) = 
        startString ++ " is:\n" ++ unwords tags
    show (LastClockOpen tags) = 
        startString ++ "s are:\n" ++ unwords tags
    show AllClocksClosed = "All the clocks are closed."
    show Empty = "The clock file is empty."

startString :: String
startString = 
    "The clock at the end of the file is open. Its tag"

data BadLineType = 
    BeginTimeIsInTheFuture  |
    EmptyLine |
    EndTimeIsInTheFuture |
    EndTimeIsBeforeBeginTime |
    LastWordNotNumeric |
    OnlyOneWordOnLine |
    OpenClockNotOnLastLine |
    AtLeastOneNumericTag deriving Eq

data BadLine = BadLine 
    { errType :: BadLineType
    , lineNum :: Int } deriving Eq

newtype BadLines = BadLines [BadLine] deriving Eq

instance Show BadLines where
    show (BadLines xs) = L.intercalate "\n" (map show xs)

instance Show BadLine where
    show (BadLine err linenum) =
        "* Error in clock file on line " ++ show linenum ++
        ":\n" ++ show err

instance Show BadLineType where
    show BeginTimeIsInTheFuture = 
        "The start time is in the future."
    show EmptyLine = "The line is empty."
    show EndTimeIsBeforeBeginTime = "The closing clock is earlier \
        \than the opening clock."
    show EndTimeIsInTheFuture = "The closing clock is in the future."
    show LastWordNotNumeric = "The last word is not a clock, that \
        \is, it contains a character that is not a number or a \
        \decimal point."
    show OnlyOneWordOnLine = "There is only one word."
    show OpenClockNotOnLastLine = "The clock has not been closed and \
        \it is not on the last line of the file.  Only the last line \
        \in the file is allowed to have an open clock."
    show AtLeastOneNumericTag = "One of the tags only contains \
        \numbers.  It should contain at least one character that is \
        \not a number or a decimal point."

parseClockFile :: Float -> String -> Either BadLines Clocks
parseClockFile _ "" = Right (Clocks [] Empty)
parseClockFile now f =
    case sequence eitherClocks of
        Left _ -> Left (BadLines (E.lefts eitherClocks))
        Right s -> Right Clocks 
            { sessions = s
            , clockstate = 
                   if isClosed (end $ last s)
                   then AllClocksClosed
                   else LastClockOpen (taglist $ last s) }
  where
    eitherClocks = file2sess now f 

isClosed :: ClockEnd -> Bool
isClosed (Closed _) = True
isClosed _ = False

file2sess ::
    Float ->
    String ->
    [Either BadLine Session]
file2sess now f = 
    [ line2sess n ofN now line | (n,line) <- zip [1..] lns ]
  where 
    lns = map words . lines $ f
    ofN = length lns

-- It converts a line in the clock file to a Session, or a BadLine if
-- it is wrong.  The first argument is the line number and the second
-- is the total number of lines in the file.
line2sess ::
    Int ->
    Int ->
    Float ->
    [String] ->
    Either BadLine Session
line2sess n _ _ [] =
    Left BadLine { errType = EmptyLine, lineNum = n }
line2sess n _ _ [_] = 
    Left BadLine { errType = OnlyOneWordOnLine, lineNum = n }
line2sess n ofN now line =
    case lookForBadWords line of
        Just err -> Left (BadLine err n)
        Nothing ->
            case lookForBadClock start stop n ofN now of
                Just err -> Left (BadLine err n)
                Nothing -> Right Session
                    { taglist = 
                        if clockClosed
                        then L.sort . L.nub . init . init $ line
                        else L.sort . L.nub . init $ line
                    , begin = start
                    , end = stop }
  where 
    clockClosed = isNum . last . init $ line
    (start, stop) =
        if clockClosed 
        then ( s2f . last . init $ line
            , Closed (s2f $ last line) )
        else ( s2f $ last line, Open )
            
data WordType = Tag | Clock deriving Eq

-- It takes in a line and works out what is wrong with it, if
-- anything.
lookForBadWords ::
    [String] -> 
    Maybe BadLineType
lookForBadWords s = parseBackwards . reverse $ wordTypes
  where 
    wordTypes = map (\x -> if isNum x then Clock else Tag) s

parseBackwards :: 
    [WordType] -> 
    Maybe BadLineType
parseBackwards [] = Just EmptyLine
parseBackwards [Clock] = Just OnlyOneWordOnLine
parseBackwards (Tag:_) = Just LastWordNotNumeric
parseBackwards (Clock:Tag:xs)  
    | all (Tag ==) xs = Nothing
    | otherwise = Just AtLeastOneNumericTag
parseBackwards [Clock,Clock] = 
    Just AtLeastOneNumericTag
parseBackwards (Clock:Clock:xs)
    | all (Tag ==) xs = Nothing
    | otherwise = Just AtLeastOneNumericTag

-- It does some checks on the clocks in a session.
lookForBadClock ::
    Float -> ClockEnd -> Int -> Int -> Float -> Maybe BadLineType
lookForBadClock start Open n ofN now 
    | start > now = Just BeginTimeIsInTheFuture
    | n /= ofN = Just OpenClockNotOnLastLine
    | otherwise = Nothing
lookForBadClock start (Closed stop) _ _ now
    | start > stop = Just EndTimeIsBeforeBeginTime
    | now < stop = Just EndTimeIsInTheFuture
    | otherwise = Nothing

s2f :: String -> Float
s2f x = read x :: Float

isNum :: String -> Bool
isNum = L.all (`elem` "1234567890.")
