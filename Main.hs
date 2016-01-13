import Prelude hiding (until)

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen

-- TODO: better messages on TreeProperty predicates
-- TODO: nice input generation system - branches needed then done!
-- TODO: TestableArrow instance for Yampa
-- TODO: create some examples

main :: IO ()
main = runTest testValueGen (const 5) testTreeProp

testValueGen :: Gen g Bool ()
testValueGen = putValues [True, False, True]

testTreeProp :: TreeProperty Int Bool
testTreeProp = inevitably allPaths testPathProp

testPathProp :: PathProperty Int Bool
testPathProp = always (==5)
