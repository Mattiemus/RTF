module Test.FRP.Netwire.TreeGen where

import Control.Wire.Unsafe.Event

import System.Random

import Test.FRP.TreeGen

instance Show a => Show (Event a) where
    show NoEvent = "NoEvent"
    show (Event x) = "Event " ++ show x

instance Random a => Random (Event a) where
    random gen = let (fire, gen2) = random gen
                 in if fire
                    then let (val, gen3) = random gen
                         in (Event val, gen3)
                    else (NoEvent, gen2)
    randomR = error "Random (Event a)::randomR - Not Implemented"
