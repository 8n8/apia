import qualified Test.Tasty as TT
import qualified TestAnalyseHistory as Ta
import qualified TestArgParse as Tp
import qualified TestParseClockFile as Parse

main :: IO ()
main = TT.defaultMain tests

tests :: TT.TestTree
tests = TT.testGroup "Tests" (
    Ta.tests ++ 
    Tp.tests ++
    Parse.tests
    )
