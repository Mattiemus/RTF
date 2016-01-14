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
testValueGen = putValues [1,2,3,4,5]

testTreeProp :: TreeProperty Int Bool
testTreeProp = inevitably allPaths testPathProp

testPathProp :: PathProperty Int Bool
testPathProp = always (==5) \/ eventually (>4)
