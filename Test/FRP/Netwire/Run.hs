{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Test.FRP.Netwire.Run where

import Data.Tree
import Data.Monoid

import Control.Wire

import Test.FRP.General
import Test.FRP.Tree
import Test.FRP.Run

{--------------------------------------------------------------------
    TestableArrow instance for netwire
--------------------------------------------------------------------}

-- |Session type for a testable wire
type TestableWireSession = Timed TimeSpan ()

-- |Instance for testing netwire wires.
instance Monoid e => TestableArrow (Wire TestableWireSession e IO) where
    createOutput w tree@(ProgTree (Node (Value (_, tp)) _)) = runTestableWire tp w tree
        where
            runTestableWire :: TimePoint -> Wire TestableWireSession e IO a b -> ProgTree (Value a) -> IO (ProgTree (Value b))
            runTestableWire prevTp wire (ProgTree (Node (Value (x, currTp)) xs)) = do
                let currTime = fromRational (toRational (currTp - prevTp))
                (result, nextWire) <- stepWire wire (Timed currTime ()) (pure x)
                case result of
                    Left _ -> error "createOutput: Wire inhibited during program tree output generation"
                    Right x -> do
                        nextNodes <- mapM (runTestableWire currTp nextWire) (fmap ProgTree xs)
                        return (ProgTree (Node (Value (x, currTp)) (fmap unProgTree nextNodes)))
