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

-- Read the README.

module Main where 

import qualified ChooseAction as C
import qualified Control.Monad as Cm
import qualified Data.Time as Dt
import qualified DecimalTime as T
import qualified System.Directory as Sd
import qualified System.Environment as SEnv

-- It makes the clock file if it is not there, reads
-- the current time, reads the input arguments, reads
-- the clock file, works out what do to, and does it.
main :: IO ()
main =
    (++ "/.apia") <$> Sd.getHomeDirectory >>= \clockfile ->
    (Sd.doesFileExist clockfile >>= \x ->
     Cm.unless x $ writeFile clockfile "") >>
    T.utc2dt <$> Dt.getCurrentTime >>= \now ->
    SEnv.getArgs >>= \args ->
    readFile clockfile >>= \f ->
    case C.chooseActions now args f of
        C.Actions Nothing Nothing ->
            print pathetic
        C.Actions (Just msg) Nothing ->
            print msg
        C.Actions Nothing (Just toFile) ->
            (appendFile clockfile . show) toFile
        C.Actions (Just msg) (Just toFile) ->
            print msg >> (appendFile clockfile . show) toFile
        
pathetic :: String
pathetic =
    "Internal error:  The program resulted in nothing at \
    \all being done, which makes you wonder what the point \
    \of everything is.  It just makes you ask why you were \
    \even born, and why, given that you were, the world \
    \you were born into is such a futile waste."
