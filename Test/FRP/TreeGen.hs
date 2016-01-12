module Test.FRP.TreeGen where

import Data.Tree

import Test.FRP.General
import Test.FRP.Tree
import Test.FRP.Path

{--------------------------------------------------------------------
    Definitions
--------------------------------------------------------------------}

type TreeGen a = [ProgTree (Value a)]

{--------------------------------------------------------------------
    Basic generators
--------------------------------------------------------------------}

fixedInput :: [ProgTree (Value a)] -> TreeGen a
fixedInput = id

singlePath :: Path (Value a) -> TreeGen a
singlePath (Path (x:xs)) = [ProgTree $ foldr (\val node -> Node val [node]) (Node x []) xs]
singlePath (Path []) = []
