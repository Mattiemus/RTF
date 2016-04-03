{-# LANGUAGE Arrows, FlexibleInstances #-}

module Test.FRP.Netwire.Samples.Projectile where

import Prelude hiding ((.), id, until)

import Control.Monad (replicateM_)
import Control.Wire hiding ((-->), testWire, once, after)
import Control.Wire.Unsafe.Event hiding ((-->), testWire, once, after)
import FRP.Netwire hiding ((-->), testWire, once, after)

import System.Random

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen
import Test.FRP.Netwire.Run
import Test.FRP.Netwire.TreeGen
import Test.FRP.Netwire.Samples.YampaRedefs

data ProjectileOutput = ProjectileOutput {
        pjoMyPos :: Vector2D,
        pjoTargetDistance :: Float,
        pjoDestroy :: Event ()
    }

projectileTest :: IO ()
projectileTest = runTest projGen (projWire zero 1.0) projMoveTreeProp

projGen :: Gen StdGen Vector2D ()
projGen = do
    setClockDelta 0.1
    replicateM_ 10 (putValue (V2 0.5 0.5))

projMoveTreeProp :: TreeProperty ProjectileOutput Bool
projMoveTreeProp = inevitably allPaths projMoveProp

projMoveProp :: PathProperty ProjectileOutput Bool
projMoveProp = always (decreasing (fmap pjoTargetDistance getCurrentValue))

projTreeLifeProp :: TimeSpan -> TreeProperty ProjectileOutput Bool
projTreeLifeProp lifetime = inevitably allPaths (projLifeProp lifetime)

projLifeProp :: TimeSpan -> PathProperty ProjectileOutput Bool
projLifeProp lifetime = before lifetime (eventually (fmap (occurred . pjoDestroy) getCurrentValue))

projTreeHitProp :: TimeSpan -> TreeProperty ProjectileOutput Bool
projTreeHitProp lifetime = inevitably allPaths (projHitProp lifetime)

projHitProp :: TimeSpan -> PathProperty ProjectileOutput Bool
projHitProp lifetime = (\po -> lifetime >= pjoTargetDistance po) --> before lifetime (eventually (fmap (occurred . pjoDestroy) getCurrentValue))

projWire :: Vector2D -> TimeSpan -> Wire TestableWireSession () IO Vector2D ProjectileOutput
projWire startPos lifetime = proc targetPos -> do
    rec pos <- integral startPos -< normalize (targetPos ^-^ pos)
    currTime <- time -< ()
    returnA -< ProjectileOutput {
            pjoMyPos = pos,
            pjoTargetDistance = distance targetPos pos,
            pjoDestroy = if pos == targetPos || currTime >= lifetime then Event () else NoEvent
        }
