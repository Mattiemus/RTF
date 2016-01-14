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

newtype ProgTree a = ProgTree { unProgTree :: Tree a }
    deriving (Show, Foldable, Functor, Applicative, Monad)

{--------------------------------------------------------------------
    Path selector
--------------------------------------------------------------------}

type PathSelector a = ProgTree (Value a) -> [Path (Value a)]

allPaths :: PathSelector a
allPaths (ProgTree (Node x [])) = [Path [x]]
allPaths (ProgTree (Node x ys)) = fmap (Path . (x :)) (concatMap (fmap unPath . allPaths) (fmap ProgTree ys))

pathsWithProperty :: PathProperty a Bool -> PathSelector a
pathsWithProperty prop tree = filter (\path -> isLeft (resultValue (runProperty path prop))) (allPaths tree)

{---------------------------w-----------------------------------------
    Definitions
--------------------------------------------------------------------}

type TreeProperty a b = Property ProgTree a b

{--------------------------------------------------------------------
    Predefined properties
--------------------------------------------------------------------}

inevitably :: PathSelector a -> PathProperty a Bool -> TreeProperty a Bool
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

possibly :: PathSelector a -> PathProperty a Bool -> TreeProperty a Bool
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
