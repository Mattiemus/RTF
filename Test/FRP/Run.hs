module Test.FRP.Run where

import System.Random

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Tree
import Test.FRP.TreeGen

{--------------------------------------------------------------------
    Run a test
--------------------------------------------------------------------}

runTest :: TestableArrow arr => Gen StdGen a () -> arr a b -> TreeProperty b Bool -> IO ()
runTest gen framework prop = do
    tree <- runDefaultGen_ gen
    case tree of
        Nothing -> error "runTest: Value generator did not produce anything"
        Just inputTree -> do
            testInput <- createOutput framework inputTree
            printResult (runProperty testInput prop)

{--------------------------------------------------------------------
    Testable arrow class
--------------------------------------------------------------------}

class TestableArrow arr where
    createOutput :: arr a b -> ProgTree (Value a) -> IO (ProgTree (Value b))

instance TestableArrow (->) where
    createOutput f input = return (fmap (fmap f) input)
