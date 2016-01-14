{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Test.FRP.Tree where

import Prelude hiding ((.), id, until)

import Data.Tree

import Control.Arrow
import Control.Category
import Control.Applicative

import Test.FRP.General
import Test.FRP.Path

{--------------------------------------------------------------------
    Tree type
--------------------------------------------------------------------}

newtype ProgTree a = ProgTree { unProgTree :: Tree a }
    deriving (Show, Foldable, Functor, Applicative, Monad)

{--------------------------------------------------------------------
    Path selector
--------------------------------------------------------------------}

type PathSelector a = ProgTree (Value a) -> [Path (Value a)]

allPaths :: PathSelector a
allPaths (ProgTree (Node x [])) = [Path [x]]
allPaths (ProgTree (Node x ys)) = fmap (Path . (x :)) (concatMap (fmap unPath . allPaths) (fmap ProgTree ys))

{---------------------------w-----------------------------------------
    Definitions
--------------------------------------------------------------------}

type TreeProperty a b = Property ProgTree a b

{--------------------------------------------------------------------
    Helpers
--------------------------------------------------------------------}

pathResults :: PathProperty a b -> PathSelector a -> TreeProperty a [Result b]
pathResults prop selector = do
    tree <- getPropInput
    let paths = selector tree
        results = fmap (`runProperty` prop) paths
    return results

{--------------------------------------------------------------------
    Predefined properties
--------------------------------------------------------------------}

inevitably :: PathSelector a -> PathProperty a Bool -> TreeProperty a Bool
inevitably selector prop = do
    results <- pathResults prop selector
    returnResult $ fmap and (sequence results)

possibly :: PathSelector a -> PathProperty a Bool -> TreeProperty a Bool
possibly selector prop = do
    results <- pathResults prop selector
    returnResult $ fmap or (sequence results)
