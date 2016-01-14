{-# LANGUAGE FlexibleInstances #-}

module Test.FRP.Netwire.Run where

import Data.Tree

import Control.Wire

import Test.FRP.General
import Test.FRP.Tree
import Test.FRP.Run

{--------------------------------------------------------------------
    Session generator
--------------------------------------------------------------------}

type TestableWireSession m = Session m (Timed NominalDiffTime ())

makeSession :: Monad m => TimeSpan -> TestableWireSession m
makeSession ts = Session $ return (Timed (fromRational (toRational ts)) (), error "makeSession: Session cannot be stepped")

{--------------------------------------------------------------------
    TestableArrow instance for netwire
--------------------------------------------------------------------}

instance TestableArrow (Wire (TestableWireSession IO) e IO) where
    createOutput w tree@(ProgTree (Node (Value (_, tp)) _)) = runTestableWire tp w tree
        where
            runTestableWire :: TimePoint -> Wire (TestableWireSession IO) e IO a b -> ProgTree (Value a) -> IO (ProgTree (Value b))
            runTestableWire prevTp wire (ProgTree (Node (Value (x, currTp)) xs)) = do
                let deltaTime = currTp - prevTp
                (result, nextWire) <- stepWire wire (makeSession deltaTime) (pure x)
                case result of
                    Left _ -> error "createOutput: Wire inhibited during program tree output generation"
                    Right x -> do
                        nextNodes <- mapM (runTestableWire currTp nextWire) (fmap ProgTree xs)
                        return (ProgTree (Node (Value (x, currTp)) (fmap unProgTree nextNodes)))
