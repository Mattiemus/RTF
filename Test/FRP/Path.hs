{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Test.FRP.Path where

import Prelude hiding (until)

import Data.List
import Data.Monoid

import Control.Monad
import Control.Applicative

import Numeric.Limits

import Test.FRP.General

{--------------------------------------------------------------------
    Path type
--------------------------------------------------------------------}

-- |A path is simply a list of values
newtype Path a = Path { unPath :: [a] }
    deriving (Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

{--------------------------------------------------------------------
    Definitions
--------------------------------------------------------------------}

-- |A property over a given path
type PathProperty a b = Property Path a b

{--------------------------------------------------------------------
    Basic properties
--------------------------------------------------------------------}

-- |Gets the current point in time
getTime :: PathProperty a TimePoint
getTime = do
    Path xs <- getPropInput
    case xs of
        [] -> fatal "Could not get current time because the frame has no time point attatched to it"
        (Value (_, tp) : _) -> return tp

-- |Gets the current value
getCurrentValue :: PathProperty a a
getCurrentValue = do
    Value (x, _) <- getValue
    pure x

-- |Tests if the next frame exists
hasNext :: PathProperty a Bool
hasNext = do
    Path xs <- getPropInput
    case xs of
        (_ : _ : _) -> pure True
        _ -> pure False

{--------------------------------------------------------------------
    Time jumping properties
--------------------------------------------------------------------}

-- |Specifies that some property must be true between two points in time
betweenTimes :: IsProperty prop Path a
             => TimePoint -- ^Start time
             -> TimePoint -- ^End time
             -> prop -- ^Property that must hold
             -> PathProperty a Bool
betweenTimes tpstart tpend prop = do
    Path filtered <- getFilteredInput (\(Value (_, t)) -> t > tpstart && t < tpend)
    if null filtered
        then fatal "Operator `betweenTimes` could not find any values between the given time points"
        else runSubProperty (Path filtered) (toProperty prop) `ifFalseWarn` "Condition did not hold between the specified times"

-- |Specifies that some property must be true prior to a given time point
beforeTime :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
beforeTime tp prop = betweenTimes minValue tp prop `ifFalseWarn` "Condition did not hold before the specified time"

-- |Specifies that some property must be true after a given time point
afterTime :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
afterTime tp prop = betweenTimes tp maxValue prop `ifFalseWarn` "Condition did not hold after the specified time"

-- |Returns the value produced by a property in the next frame. Note this can fail if the next frame does not exist, use `tryNextFrame` as a safer alternative.
nextFrame :: IsProperty prop Path a => prop -> PathProperty a Bool
nextFrame prop = do
    Path xs <- getPropInput
    case xs of
        [] -> fatal "Cannot perform operation in next frame as there is no frame after the current one"
        (_:ys) -> runSubProperty (Path ys) (toProperty prop) `ifFalseWarn` "Condition did not hold in the next frame"

-- |Returns the value produced by a property in the next frame. Performs a fallback property in the current frame if the next frame does not exist.
tryNextFrame :: (IsProperty failProp Path a, IsProperty successProp Path a)
             => failProp -- ^Fallback property executed in the current frame if the next frame does not exist
             -> successProp -- ^Property performed in the next frame if it exists
             -> PathProperty a Bool
tryNextFrame failProp successProp = do
    canCont <- hasNext
    if canCont
        then nextFrame successProp
        else toProperty failProp

{--------------------------------------------------------------------
    Time constraining properties
--------------------------------------------------------------------}

-- |Specifies that some property must be true over some period of time
between :: IsProperty prop Path a => TimeSpan -> TimeSpan -> prop -> PathProperty a Bool
between tsStart tsEnd prop = do
    timeNow <- getTime
    betweenTimes (tsStart + timeNow) (tsEnd + timeNow) prop

-- |Specifies that some property must be true prior to a given amount of time
before :: IsProperty prop Path a => TimeSpan -> prop -> PathProperty a Bool
before ts prop = do
    timeNow <- getTime
    beforeTime (ts + timeNow) prop

-- |Specifies that some property must be true after a given amount of time
after :: IsProperty prop Path a => TimeSpan -> prop -> PathProperty a Bool
after ts prop = do
    timeNow <- getTime
    afterTime (ts + timeNow) prop

{--------------------------------------------------------------------
    Temporal properties
--------------------------------------------------------------------}

-- |Attempts to get the value in the next frame. Defaults to Nothing if there is no next frame.
next :: PathProperty a (Maybe (Value a))
next = do
    Path xs <- getPropInput
    case xs of
        (_ : x : _) -> pure (Just x)
        _ -> pure Nothing

-- |Specifies that a given property must be true over all frames
always :: IsProperty prop Path a => prop -> PathProperty a Bool
always prop = (False `release` prop) `ifFalseWarn` "Condition in Always operator is not globablly satisfiable"

-- |Specifies that a given property must be false over all frames
never :: IsProperty prop Path a => prop -> PathProperty a Bool
never prop = pNot (eventually prop) `ifFalseWarn` "Condition in Never operator is not globablly falsifiable"

-- |Specifies that a given property must be true at either the current frame or some point in the future
eventually :: IsProperty prop Path a => prop -> PathProperty a Bool
eventually prop = pNot (always (pNot prop)) `ifFalseWarn` "Condition in Eventually operator is never satisfied"

-- |Specifies that some condition must be true until a given stop condition is met. If the stop condition never occurs, the condition must be true forever.
weakUntil :: (IsProperty propa Path a, IsProperty propb Path a)
          => propa -- ^Condition to be true until the stop condition is met
          -> propb -- ^Stop condition
          -> PathProperty a Bool
p `weakUntil` q = (q `release` (q \/ p)) `ifFalseWarn` "Condition in WeakUntil operator returned False prior to the stop condition being satisfied"

-- |Specifies that some condition must be true until a given stop condition is met. Note that the stop condition must eventually occur.
until :: (IsProperty propa Path a, IsProperty propb Path a)
      => propa -- ^Condition to be true until the stop condition is met
      -> propb -- ^Stop condition that must eventually occur
      -> PathProperty a Bool
p `until` q = do
    eventuallyQ <- eventually q `ifFalseWarn` "Stop condition in Until operator is never satisfied"
    if eventuallyQ
        then (p `weakUntil` q) `ifFalseWarn` "Condition in Until operator returned False prior to the stop condition being satisfied"
        else return False

-- |Specifies that some property much be true until and including the point that a release condition is met. If the release condition is never met the property must be true forever.
release :: (IsProperty propa Path a, IsProperty propb Path a)
        => propa -- ^Release condition
        -> propb -- ^Property
        -> PathProperty a Bool
p `release` q = do
    qNow <- toProperty q `ifFalseWarn` "Condition in Release operator returned False prior to the release condition being satisfied"
    if qNow
        then do
            pNow <- toProperty p
            if pNow
                then return True
                else tryNextFrame True (p `release` q)
        else return False

-- |Specifies that some property must hold after some property becomes true
once :: (IsProperty propa Path a, IsProperty propb Path a)
      => propa -- ^Property to be true
      -> propb -- ^Triggering property
      -> PathProperty a Bool
p `once` q = do
    pResult <- toProperty q
    if pResult
        then toProperty p `ifFalseWarn` "Property in Once operator was falsified after triggering property"
        else tryNextFrame True (p `once` q)

{--------------------------------------------------------------------
    Now vs. future properties
--------------------------------------------------------------------}

-- |Specifies that the next value must be larger than the current. Defaults to true if there is no next frame.
increasing :: Ord a => PathProperty a Bool
increasing = liftA2 (\a -> maybe True (>a)) getValue next

-- |Specifies that the next value must be smaller than the current. Defaults to true if there is no next frame.
decreasing :: Ord a => PathProperty a Bool
decreasing = liftA2 (\a -> maybe True (<a)) getValue next
