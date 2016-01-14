import Prelude hiding (until)

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen

-- TODO: better messages on TreeProperty predicates
-- TODO: TestableArrow instance for Yampa
-- TODO: create some examples

main :: IO ()
main = runTest testValueGen id testTreeProp

testValueGen :: Gen g Int ()
testValueGen = do
    branch (putValues [1..10])
    branch (putValues [10,9..0])

testTreeProp :: TreeProperty Int Bool
testTreeProp = inevitably allPaths testPathProp

testPathProp :: PathProperty Int Bool
testPathProp = always increasing `implies` (==1)
