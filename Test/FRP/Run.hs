module Test.FRP.Run where

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Tree
import Test.FRP.TreeGen

{--------------------------------------------------------------------
    Run a test
--------------------------------------------------------------------}

runTest :: TreeGen a -> TreeProperty a Bool -> IO ()
runTest gen prop = do
    let trees = gen
        results = fmap (`runProperty` prop) trees
        result = fmap and (sequence results)
    printResult result

{--------------------------------------------------------------------
    Test bootstraps
--------------------------------------------------------------------}

runTestAllPaths :: TreeGen a -> PathProperty a Bool -> IO ()
runTestAllPaths gen prop = runTest gen (inevitably allPaths prop)
