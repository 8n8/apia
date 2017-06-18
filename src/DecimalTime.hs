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

-- This module provides functions for converting UTCTime
-- to my own decimal time, which is just days since 
-- 00:00 on 1 October 2016 in Coordinated Universal Time 
-- (UTC) see 
-- https://en.wikipedia.org/wiki/Coordinated_Universal_Time.

module DecimalTime where

import qualified Data.Time as Dt
import qualified Data.Time.Clock as Dtc

-- It converts UTCTime to days since 00:00 on 1 Oct 2016,
-- which is an arbitrary epoch.
utc2dt :: Dt.UTCTime -> Float
utc2dt utc = time2float (Dtc.diffUTCTime utc epoch) / 86400

-- It converts the epoch (1 Oct 2016 at 00:00) into 
-- UTCTime.  This epoch is the beginning of time from the
-- perspective of the program.  All dates and times are 
-- the number of days away from the epoch.  A day is 86400 
-- seconds.  There are no leap seconds in this system, and no
-- years, months, weeks, hours, minutes or seconds.
epoch :: Dt.UTCTime
epoch = Dt.UTCTime (Dt.fromGregorian 2016 10 1) 0

-- It converts NominalDiffTime into a number of seconds.
time2float :: Dt.NominalDiffTime -> Float
time2float = realToFrac
