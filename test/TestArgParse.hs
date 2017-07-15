module TestArgParse (tests) where

import qualified ArgParse as A
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as HU

tests :: [TT.TestTree]
tests = 
    [ argParse
    ]

argParse :: TT.TestTree
argParse = TT.testGroup "argParse"
    [ HU.testCase "empty" $
        A.argParse [] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "simple" $
        A.argParse [" ", "     ."] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "bad total" $
        A.argParse ["123.2", "total"] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "summary" $
        A.argParse ["daily", "123.2", "333", "aa"] HU.@?=
            Left A.StartNotInt
    , HU.testCase "summary" $
        A.argParse ["daily", "123", "333", "aa"] HU.@?=
            Right (A.Daily 123 333 ["aa"])
    , HU.testCase "switch" $
        A.argParse ["switch", "oy", "you"] HU.@?=
            Right (A.Switch ["oy", "you"])
    , HU.testCase "clockin" $
        A.argParse ["clockin", "a"] HU.@?=
            Right (A.ClockIn ["a"])
    ] 
