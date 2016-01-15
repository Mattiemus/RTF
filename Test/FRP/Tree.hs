{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Test.FRP.Tree where

import Prelude hiding ((.), id, until)

import Data.Tree
import Data.Either

import Control.Arrow
import Control.Category
import Control.Applicative

import Test.FRP.General
import Test.FRP.Path

{--------------------------------------------------------------------
    Tree type
--------------------------------------------------------------------}

-- |A program tree is simply a tree of values
newtype ProgTree a = ProgTree { unProgTree :: Tree a }
    deriving (Show, Foldable, Functor, Applicative, Monad)

{--------------------------------------------------------------------
    Path selector
--------------------------------------------------------------------}

-- |A path selector is a function that takes a program tree and returns a list of paths through it
type PathSelector a = ProgTree (Value a) -> [Path (Value a)]

-- |Generates all possible paths through the input program tree
allPaths :: PathSelector a
allPaths (ProgTree (Node x [])) = [Path [x]]
allPaths (ProgTree (Node x ys)) = fmap (Path . (x :)) (concatMap (fmap unPath . allPaths) (fmap ProgTree ys))

-- |Generates all paths through the input program tree that satisfy some path property
pathsWithProperty :: PathProperty a Bool -- ^The property that the generated paths should satisfy
                  -> PathSelector a
pathsWithProperty prop tree = filter (\path -> isLeft (resultValue (runProperty path prop))) (allPaths tree)

{---------------------------w-----------------------------------------
    Definitions
--------------------------------------------------------------------}

-- |A property over a program tree
type TreeProperty a b = Property ProgTree a b

{--------------------------------------------------------------------
    Predefined properties
--------------------------------------------------------------------}

-- |Specifies that the given paths should all satisfy some path property.
inevitably :: PathSelector a -- ^A selector to choose which paths to test
           -> PathProperty a Bool -- ^The property all paths should have
           -> TreeProperty a Bool
inevitably selector prop = do
    paths <- fmap selector getPropInput
    if null paths
        then fatal "Path selector did not select any paths through the program tree"
        else runPaths paths prop
    where
        runPaths :: [Path (Value a)] -> PathProperty a Bool -> TreeProperty a Bool
        runPaths [] pathProp = return True
        runPaths (path:paths) pathProp = do
            pathResult <- returnResult (runProperty path pathProp)
            if pathResult
                then runPaths paths pathProp
                else do
                    warn "Path property was not satisfied along one of the selected paths"
                    return False

-- |Specifies that one or more of the given paths should satisfy some path property.
possibly :: PathSelector a -- ^A selector to choose which paths to test
         -> PathProperty a Bool -- ^The property one or more of the paths should have
         -> TreeProperty a Bool
possibly selector prop = do
    paths <- fmap selector getPropInput
    if null paths
        then fatal "Path selector did not select any paths through the program tree"
        else runPaths paths prop
    where
        runPaths :: [Path (Value a)] -> PathProperty a Bool -> TreeProperty a Bool
        runPaths [] pathProp = do
            warn "Path property was not satisfied along any of the selected paths"
            return False
        runPaths (path:paths) pathProp = do
            pathResult <- returnResult (runProperty path pathProp)
            if pathResult
                then return True
                else runPaths paths pathProp
