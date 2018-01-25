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

-- Read the README.

module Main where 

import qualified ChooseAction as C
import qualified Control.Monad as Cm
import qualified Data.Time as Dt
import qualified DecimalTime as T
import qualified System.Directory as Sd
import qualified System.Environment as SEnv

-- It makes the clock file if it is not there, reads the current
-- time, reads the input arguments, reads the clock file, and does one
-- or both of writing to the clock file and printing a message.
main :: IO ()
main = do
    clockfile <- (++ "/.apia") <$> Sd.getHomeDirectory
    fileExists <- Sd.doesFileExist clockfile
    Cm.unless fileExists $ writeFile clockfile ""
    now <- T.utc2dt <$> Dt.getCurrentTime
    args <- SEnv.getArgs
    filecontents <- readFile clockfile
    case C.chooseActions now args filecontents of
        C.Actions Nothing Nothing ->
            print internalErrorMsg
        C.Actions (Just msg) Nothing ->
            print msg
        C.Actions Nothing (Just toFile) ->
            (appendFile clockfile . show) toFile
        C.Actions (Just msg) (Just toFile) ->
            print msg >> (appendFile clockfile . show) toFile
        
internalErrorMsg :: String
internalErrorMsg = "Internal error: the program resulted in nothing at all \
    \begin done.  This is a bug."
