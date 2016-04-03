{-# LANGUAGE Arrows #-}

module Test.FRP.Netwire.Samples.UserInput where

import Prelude hiding ((.), id, until)

import Control.Monad (replicateM_)
import Control.Wire hiding ((-->), testWire, once, after)
import Control.Wire.Unsafe.Event hiding ((-->), testWire, once, after)
import FRP.Netwire hiding ((-->), testWire, once, after)

import System.Random hiding (next)
import System.IO.Unsafe

import Test.FRP.General
import Test.FRP.Path
import Test.FRP.Run
import Test.FRP.Tree
import Test.FRP.TreeGen
import Test.FRP.Netwire.Run
import Test.FRP.Netwire.TreeGen
import Test.FRP.Netwire.Samples.YampaRedefs

userTest :: IO ()
userTest = runBasicTest (projWire zero) userMoveProp

data Key
    = KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    deriving (Show, Eq, Enum)

data PlayerInput = PlayerInput {
        pliKeys :: [Key]
    }

data PlayerOutput = PlayerOutput {
        ploMyPos :: Vector2D
    }

instance Random Key where
    random gen = let (num, gen2) = randomR (0, 3) gen
                 in (toEnum num, gen2)
    randomR = error "Random Key::randomR - Not Implemented"

instance Random PlayerInput where
    random gen = let (num, gen2) = randomR (0, 10) gen
                     out = PlayerInput {
                             pliKeys = take num (randoms gen2)
                         }
                 in (out, gen2)
    randomR = error "Random PlayerInput::randomR - Not Implemented"

userMoveProp :: PathProperty (PlayerInput, PlayerOutput) Bool
userMoveProp = do
    currPos <- fmap (ploMyPos . snd) getCurrentValue
    deltaT <- getDeltaTime
    tryNextFrame True $ do
        movement <- fmap ((^* deltaT) . keysToMovement . pliKeys . fst) getCurrentValue
        tryNextFrame True $ do
            nextPos <- fmap (ploMyPos . snd) getCurrentValue
            return (currPos ^+^ movement == nextPos)

keysToMovement :: [Key] -> Vector2D
keysToMovement [] = zero
keysToMovement (x:xs) =
    case x of
        KeyUp -> V2 0 (-1) ^+^ keysToMovement xs
        KeyDown -> V2 0 1 ^+^ keysToMovement xs
        KeyLeft -> V2 (-1) 0 ^+^ keysToMovement xs
        KeyRight -> V2 1 0 ^+^ keysToMovement xs

projWire :: Vector2D -> Wire TestableWireSession () IO PlayerInput (PlayerInput, PlayerOutput)
projWire startPos = proc input -> do
    rec pos <- integral startPos -< keysToMovement (pliKeys input)
    returnA -< (input, PlayerOutput {
            ploMyPos = pos
        })
