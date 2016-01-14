{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Test.FRP.Path where

import Prelude hiding (until)

import Data.List
import Data.Monoid

import Control.Monad
import Control.Applicative

import Test.FRP.General

{--------------------------------------------------------------------
    Path type
--------------------------------------------------------------------}

newtype Path a = Path { unPath :: [a] }
    deriving (Show, Foldable, Functor, Applicative, Monad, Alternative, MonadPlus)

{--------------------------------------------------------------------
    Definitions
--------------------------------------------------------------------}

type PathProperty a b = Property Path a b

{--------------------------------------------------------------------
    Basic properties
--------------------------------------------------------------------}

getTime :: PathProperty a TimePoint
getTime = do
    Path xs <- getPropInput
    case xs of
        [] -> fatal "Could not get current time because the frame has no time point attatched to it"
        (Value (_, tp) : _) -> return tp

hasNext :: PathProperty a Bool
hasNext = do
    Path xs <- getPropInput
    case xs of
        (_ : _ : _) -> pure True
        _ -> pure False

{--------------------------------------------------------------------
    Time jumping properties
--------------------------------------------------------------------}

betweenTimes :: IsProperty prop Path a => TimePoint -> TimePoint -> prop -> PathProperty a Bool
betweenTimes tpstart tpend prop = do
    Path filtered <- getFilteredInput (\(Value (_, t)) -> t > tpstart && t < tpend)
    if null filtered
        then fatal "Operator `betweenTimes` could not find any values between the given time points"
        else runSubProperty (Path filtered) (toProperty prop) `ifFalseWarn` "False: Condition did not hold between the specified times"

beforeTime :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
beforeTime tp prop = do
    Path filtered <- getFilteredInput (\(Value (_, t)) -> t < tp)
    if null filtered
        then fatal "Operator `beforeTime` could not find any values before the given time point"
        else runSubProperty (Path filtered) (toProperty prop) `ifFalseWarn` "False: Condition did not hold before the specified time"

afterTime :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
afterTime tp prop = do
    Path filtered <- getFilteredInput (\(Value (_, t)) -> t > tp)
    if null filtered
        then fatal "Operator `afterTime` could not find any values after the given time point"
        else runSubProperty (Path filtered) (toProperty prop) `ifFalseWarn` "False: Condition did not hold after the specified time"

nextFrame :: IsProperty prop Path a => prop -> PathProperty a Bool
nextFrame prop = do
    Path xs <- getPropInput
    case xs of
        [] -> fatal "Cannot perform operation in next frame as there is no frame after the current one"
        (_:ys) -> runSubProperty (Path ys) (toProperty prop) `ifFalseWarn` "Condition did not hold in the next frame"

tryNextFrame :: (IsProperty failProp Path a, IsProperty successProp Path a) => failProp -> successProp -> PathProperty a Bool
tryNextFrame failProp successProp = do
    canCont <- hasNext
    if canCont
        then nextFrame successProp
        else toProperty failProp

{--------------------------------------------------------------------
    Time constraining properties
--------------------------------------------------------------------}

between :: IsProperty prop Path a => TimeSpan -> TimeSpan -> prop -> PathProperty a Bool
between tsStart tsEnd prop = do
    timeNow <- getTime
    betweenTimes (tsStart + timeNow) (tsEnd + timeNow) prop

before :: IsProperty prop Path a => TimeSpan -> prop -> PathProperty a Bool
before ts prop = do
    timeNow <- getTime
    beforeTime (ts + timeNow) prop

after :: IsProperty prop Path a => TimeSpan -> prop -> PathProperty a Bool
after ts prop = do
    timeNow <- getTime
    afterTime (ts + timeNow) prop

{--------------------------------------------------------------------
    Temporal properties
--------------------------------------------------------------------}

next :: PathProperty a (Maybe (Value a))
next = do
    Path xs <- getPropInput
    case xs of
        (_ : x : _) -> pure (Just x)
        _ -> pure Nothing

always :: IsProperty prop Path a => prop -> PathProperty a Bool
always prop = (False `release` prop) `ifFalseWarn` "Condition in Always operator is not globablly satisfiable"

eventually :: IsProperty prop Path a => prop -> PathProperty a Bool
eventually prop = pNot (always (pNot prop)) `ifFalseWarn` "Condition in Eventually operator is never satisfied"

weakUntil :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `weakUntil` q = (q `release` (q \/ p)) `ifFalseWarn` "Condition in WeakUntil operator returned False prior to the stop condition being satisfied"

until :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `until` q = do
    eventuallyQ <- eventually q `ifFalseWarn` "Stop condition in Until operator is never satisfied"
    if eventuallyQ
        then (p `weakUntil` q) `ifFalseWarn` "Condition in Until operator returned False prior to the stop condition being satisfied"
        else return False

release :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `release` q = do
    qNow <- toProperty q `ifFalseWarn` "Condition in Release operator returned False prior to the release condition being satisfied"
    if qNow
        then do
            pNow <- toProperty p
            if pNow
                then return True
                else tryNextFrame True (p `release` q)
        else return False

{--------------------------------------------------------------------
    Now vs. future properties
--------------------------------------------------------------------}

increasing :: Ord a => PathProperty a Bool
increasing = liftA2 (\a -> maybe True (>a)) getValue next

decreasing :: Ord a => PathProperty a Bool
decreasing = liftA2 (\a -> maybe True (<a)) getValue next
