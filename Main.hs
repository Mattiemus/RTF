import Prelude hiding ((.), id, until)

import Control.Wire hiding ((-->), testWire)
import Control.Wire.Unsafe.Event hiding ((-->), testWire)

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
testValueGen = putValues [1..10]

testTreeProp :: TreeProperty (Int, Event Int) Bool
testTreeProp = inevitably allPaths testPathProp

testPathProp :: PathProperty (Int, Event Int) Bool
testPathProp = always (do
    (x, e) <- getCurrentValue
    (x == 5) --> occurred e)

testWire :: Wire TestableWireSession e IO Int (Int, Event Int)
testWire = id &&& edge (==5)
