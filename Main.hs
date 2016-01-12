import Prelude hiding (until)

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen

-- TODO: better messages on TreeProperty predicates

main :: IO ()
main = runTestAllPaths (singlePath (Path [Value (1, 0.0), Value (2, 0.1), Value (3, 0.2), Value (3, 0.3)]))
                       (increasing `until` (==5))

increasing :: Ord a => PathProperty a Bool
increasing = do
    valA <- getValue
    valB <- next
    return (maybe True (>valA) valB)
