module TestParseClockFile (tests) where

import qualified Test.Tasty as TT
import qualified ParseClockFile as P
import qualified Test.Tasty.HUnit as HU

tests :: [TT.TestTree]
tests = 
    [ parseClockFile
    ]

parseClockFile :: TT.TestTree
parseClockFile = TT.testGroup "parseClockFile"
    [ HU.testCase "empty" $
        P.parseClockFile 0 "" HU.@?= 
            Right (Right (P.Clocks [] P.Empty))
    , HU.testCase "one line" $
        P.parseClockFile 4 file1 HU.@?=
            Right (Right (P.Clocks sess1 P.AllClocksClosed))
    ]
        
sess1 :: [P.Session]
sess1 =
    [ P.Session ["a"] 1 (P.Closed 1.2)
    , P.Session ["oo", "ooo"] 3 (P.Closed 3.4)
    ]

file1 :: String
file1 = "\
    \a 1 1.2\n\
    \ooo ooo ooo oo 3 3.4"

