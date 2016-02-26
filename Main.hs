import Prelude hiding ((.), id, until)

import Control.Wire hiding ((-->), testWire, once)
import Control.Wire.Unsafe.Event hiding (testWire, once)

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen
import Test.FRP.Netwire.Run

-- TODO: the output should be made easier to debug!
-- TODO: create some examples

main :: IO ()
main = runTest testValueGen (testWire 10) (testTreeProp 10)

testValueGen :: Gen g (Event Int) ()
testValueGen = putValues [NoEvent, NoEvent, Event 10, NoEvent, Event 15]

testTreeProp :: Int -> TreeProperty (Int, Event Int) Bool
testTreeProp x = inevitably allPaths (testPathProp x)

testPathProp :: Int -> PathProperty (Int, Event Int) Bool
testPathProp x = do
    (currVal, _) <- getCurrentValue
    ((\(r, _) -> r == x) `weakUntil` (\(_, e) -> occurred e)) /\
        ((tryNextFrame True (testPathProp currVal)) `once` (\(_, e) -> occurred e))

testWire :: Int -> Wire TestableWireSession () IO (Event Int) (Int, Event Int)
testWire x = hold <+> pure x &&& id
