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
        A.argParse 0 [] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "simple" $
        A.argParse 0 [" ", "     ."] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "bad total" $
        A.argParse 0 ["123.2", "total"] HU.@?= Left A.UnhelpfulFail
    , HU.testCase "summary" $
        A.argParse 500 ["daily", "123.2", "333", "aa"] HU.@?=
            Left A.StartNotInt
    , HU.testCase "summary" $
        A.argParse 400 ["daily", "123", "333", "aa"] HU.@?=
            Right (A.Daily 123 333 ["aa"])
    , HU.testCase "switch" $
        A.argParse 0 ["switch", "oy", "you"] HU.@?=
            Right (A.Switch ["oy", "you"])
    , HU.testCase "clockin" $
        A.argParse 0 ["clockin", "a"] HU.@?=
            Right (A.ClockIn ["a"])
    ] 
