module Test.FRP.Run where

import System.Random

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Tree
import Test.FRP.TreeGen

{--------------------------------------------------------------------
    Run a test
--------------------------------------------------------------------}

-- |Runs a testable arrow on some generated input trees
runTest :: TestableArrow arr
        => Gen StdGen a () -- ^Generator used to create inputs
        -> arr a b -- ^TestableArrow to run the generated inputs
        -> TreeProperty b Bool -- ^A property that must hold over all program outputs
        -> IO ()
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

runBasicTest :: (Random a, TestableArrow arr)
             => arr a b -- ^TestableArrow to run the generated inputs
             -> PathProperty b Bool -- ^A property that must hold over all program outputs
             -> IO ()
runBasicTest framework prop = runTest (makeRandomTree 10 0) framework (inevitably allPaths prop)

{--------------------------------------------------------------------
    Testable arrow class
--------------------------------------------------------------------}

-- |Class of arrows that can be tested
class TestableArrow arr where
    -- |Runs all inputs through the given arrow, returning a new tree of the outputs
    createOutput :: arr a b -- ^The arrow used to generate outputs
                 -> ProgTree (Value a) -- ^Program tree containing inputs
                 -> IO (ProgTree (Value b)) -- ^An IO action that returns a program tree containing outputs from the given arrow

instance TestableArrow (->) where
    createOutput f input = return (fmap (fmap f) input)
