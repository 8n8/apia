module TestAnalyseHistory (tests) where

import qualified AnalyseHistory as A
import qualified Test.Tasty as TT
import qualified Data.List as L
import qualified ParseClockFile as P
import qualified Test.Tasty.HUnit as HU

tests :: [TT.TestTree]
tests = 
    [ dailyDurations
    , newtags
    , getTagList
    , getTagsForPeriod
    , daymean
    , summary ]

daymean :: TT.TestTree
daymean = TT.testGroup "daymean"
    [ HU.testCase "empty" $
        A.daymean (P.Clocks [] P.Empty) 0 0 [] 0 HU.@?= Right 0
    , HU.testCase "simple" $
        A.daymean clcks 5 10 tags 11.2 HU.@?= Right 0.9666667 ]
  where
    tags = ["a", "b"]
    clcks = P.Clocks
        [ P.Session ["b", "o", "a"] 7 P.Open
        , P.Session ["d", "zz", "b", "a"] 6.5 (P.Closed 8.3) ]
        (P.LastClockOpen ["b", "o", "a"])

summary :: TT.TestTree
summary = TT.testGroup "summary"
    [ HU.testCase "empty" $
        summary1 HU.@?= Right [("total", 0)]
    , HU.testCase "simple" $
        summary2 HU.@?= Right sums ]
  where
    summary1 = 
        A.summary (P.Clocks [] P.Empty) 0 0 0
    summary2 = 
        A.summary (P.Clocks sesses (P.LastClockOpen ["b"])) 8.888 3 6
    sums = [("^", 99), ("b", 99), ("total", 99)]
    sesses = 
        [ P.Session [ "^", "b" ] 3 (P.Closed 3.1) ]
    
getTagsForPeriod :: TT.TestTree
getTagsForPeriod = TT.testGroup "getTagsForPeriod"
    [ HU.testCase "empty" $
        A.getTagsForPeriod 0 0 0 [] HU.@?= []
    , HU.testCase "simple" $
        A.getTagsForPeriod 5.44 2 3 sesses HU.@?= tags
    , HU.testCase "edge" $
        A.getTagsForPeriod 8.888 3 6 extra HU.@?= ["^", "b"]]
  where
    tags = [ "z", "£", "££" ]
    extra =
        [ P.Session [ "^", "b" ] 3 (P.Closed 3.1) ]
    sesses =
        [ P.Session [ "z", "z" ] 0 P.Open
        , P.Session [ "aa" ] 1.5 (P.Closed 2)
        , P.Session [ "££", "£" ] 2.6 (P.Closed 4)
        , P.Session [ "a" ] 1.1 (P.Closed 1.3) ]

getTagList :: TT.TestTree
getTagList = TT.testGroup "getTagList"
    [ HU.testCase "empty" $
        A.getTagList [] HU.@?= []
    , HU.testCase "simple" $
        A.getTagList sessions HU.@?= [ "a" ]
    , HU.testCase "bigger" $
        A.getTagList bigsess HU.@?= tags ]
  where
    tags = [ "$b", "a", "aa", "bb", "zto" ]
    bigsess = 
        [ P.Session [ "aa", "$b" ] 1 P.Open
        , P.Session [ "zto", "bb" ] 0 (P.Closed 3)
        , P.Session [ "a", "zto", "aa" ] 111 (P.Closed 2) ]
    sessions =
        [ P.Session [ "a", "a" ] 1 P.Open ]

newtags :: TT.TestTree
newtags = TT.testGroup "newtags"
    [ HU.testCase "simple" $
        A.newtags sessions tags HU.@?= ["d", "ee"]
    , HU.testCase "empty" $
        A.newtags emptysess [] HU.@?= [] ]
  where
    emptysess = 
        [ P.Session [] 0 (P.Closed 0) ]
    tags = [ "b", "ee", "a", "d", "ee" ]
    sessions =
        [ P.Session ["a"] 1 (P.Closed 2)
        , P.Session ["a", "b", "c"] 3 (P.Closed 4) ]

dailyDurations :: TT.TestTree
dailyDurations = TT.testGroup "Daily durations"
    [ HU.testCase "empty" $ 
        A.dailyDurations minhist 0 0 [] 0 HU.@?= output
    , HU.testCase "four sessions" $
        eqTupLists 
            actual moreoutput HU.@?= True ]
  where
    output = Right [(0,0)] 
    minhist = 
        P.Clocks [ P.Session ["a"] 0 (P.Closed 0) ] 
            P.AllClocksClosed
    moreoutput = Right 
        [ (276, 0), (277, 0.1), (278, 0), (279, 0.3) ]
    actual = A.dailyDurations morehist 276 279 moretags 279.9
    moretags = ["asdf"]
    morehist = 
        P.Clocks
            [ P.Session ["asdf", "a"] 277.8 (P.Closed 277.9)
            , P.Session ["onion"] 277.8776 (P.Closed 277.87772)
            , P.Session ["a","b"] 277.87772 (P.Closed 277.9108)
            , P.Session ["asdf"] 279.5 (P.Closed 279.8)]
                P.AllClocksClosed

eqTupLists 
    :: Either A.InternalError [(Int, Float)] 
    -> Either A.InternalError [(Int, Float)] 
    -> Bool
eqTupLists (Left a) (Left b)
    | a /= b = False
    | otherwise = True
eqTupLists (Left _) (Right _) = False
eqTupLists (Right _) (Left _) = False
eqTupLists (Right a) (Right b)
    | length a /= length b = False
    | null a, null b = True
    | L.all comparetups (zip a b) = True
    | otherwise = False

comparetups :: ((Int, Float), (Int, Float)) -> Bool
comparetups ((d1, t1), (d2, t2))
    | d1 /= d2 = False
    | abs (t1 - t2) > 0.0001 = False
    | otherwise = True 
