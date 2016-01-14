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
    trees <- runDefaultGen_ gen
    case trees of
        [] -> error "runTest: Value generator did not produce anything"
        inputTrees -> runTrees trees framework prop
    where
        runTrees :: TestableArrow arr => [ProgTree (Value a)] -> arr a b -> TreeProperty b Bool -> IO ()
        runTrees [] _ _ = putStrLn "All tests completed successfully"
        runTrees (tree:trees) framework prop = do
            testInput <- createOutput framework tree
            let testResult = runProperty testInput prop
            printResult testResult
            case resultValue testResult of
                Right True -> runTrees trees framework prop
                _ -> putStrLn "Test failed. Stopping."

{--------------------------------------------------------------------
    Testable arrow class
--------------------------------------------------------------------}

class TestableArrow arr where
    createOutput :: arr a b -> ProgTree (Value a) -> IO (ProgTree (Value b))

instance TestableArrow (->) where
    createOutput f input = return (fmap (fmap f) input)
