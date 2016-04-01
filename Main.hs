import Prelude hiding ((.), id, until)

import Control.Wire hiding ((-->), testWire, once)
import Control.Wire.Unsafe.Event hiding (testWire, once)

import System.Random

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen
import Test.FRP.Netwire.Run
import Test.FRP.Netwire.TreeGen

-- TODO: the output should be made easier to debug!
-- TODO: create some examples

main :: IO ()
main = runBasicTest (testWire 10) (testPathProp 15)

testPathProp :: Int -> PathProperty (Int, Event Int) Bool
testPathProp x = (((==x) . fst) `weakUntil` (occurred . snd)) /\ (testPathPropCont `once` (occurred . snd))

testPathPropCont :: PathProperty (Int, Event Int) Bool
testPathPropCont = do
    (x, _) <- getCurrentValue
    tryNextFrame True (testPathProp x)

testWire :: Int -> Wire TestableWireSession () IO (Event Int) (Int, Event Int)
testWire x = hold <+> pure x &&& id
