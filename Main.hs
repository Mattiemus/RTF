import Prelude hiding (id, until)

import Control.Wire hiding (testWire)

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen
import Test.FRP.Netwire.Run

-- TODO: better messages on TreeProperty predicates
-- TODO: create some examples

main :: IO ()
main = runTest testValueGen testWire testTreeProp

testValueGen :: Gen g Int ()
testValueGen = do
    branch (putValues [1..10])
    branch (putValues [10,9..0])

testTreeProp :: TreeProperty Int Bool
testTreeProp = inevitably allPaths testPathProp

testPathProp :: PathProperty Int Bool
testPathProp = always increasing `implies` (==2)

testWire :: Wire (TestableWireSession IO) () IO Int Int
testWire = arr (+1)
