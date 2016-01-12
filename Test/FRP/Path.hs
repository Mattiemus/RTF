{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ExistentialQuantification, DeriveFoldable, DeriveFunctor #-}

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
    deriving (Foldable, Functor)

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

between :: IsProperty prop Path a => TimePoint -> TimePoint -> prop -> PathProperty a Bool
between tpstart tpend prop = do
    Path xs <- getPropInput
    let filtered = filter (\(Value (_, t)) -> t > tpstart && t < tpend) xs
    if null filtered
        then fatal "Operator `between` could not find any values between the given time points"
        else runSubProperty (Path filtered) (toProperty prop)

before :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
before tp prop = do
    Path xs <- getPropInput
    let filtered = filter (\(Value (_, t)) -> t < tp) xs
    if null filtered
        then fatal "Operator `before` could not find any values before the given time point"
        else runSubProperty (Path filtered) (toProperty prop)

after :: IsProperty prop Path a => TimePoint -> prop -> PathProperty a Bool
after tp prop = do
    Path xs <- getPropInput
    let filtered = filter (\(Value (_, t)) -> t > tp) xs
    if null filtered
        then fatal "Operator `after` could not find any values after the given time point"
        else runSubProperty (Path filtered) (toProperty prop)

nextFrame :: Property Path a b -> PathProperty a b
nextFrame prop = do
    Path xs <- getPropInput
    case xs of
        [] -> fatal "Cannot perform operation in next frame as operation was performed at the end of input"
        (_:ys) -> runSubProperty (Path ys) prop

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
always prop = do
    result <- False `release` prop
    unless result (warn "False: Condition in Always operator is not globablly satisfiable")
    return result

eventually :: IsProperty prop Path a => prop -> PathProperty a Bool
eventually prop = do
    result <- fmap not (always (fmap not (toProperty prop)))
    unless result (warn "False: Condition in Eventually operator is never satisfied")
    return result

weakUntil :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `weakUntil` q = do
    result <- q `release` (q \/ p)
    unless result (warn "False: Condition in WeakUntil operator returned False prior to the stop condition being satisfied")
    return result

until :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `until` q = do
    eventuallyQ <- eventually q
    if eventuallyQ
        then do
            pWeakUntilQ <- p `weakUntil` q
            unless pWeakUntilQ (warn "False: Condition in Until operator returned False prior to the stop condition being satisfied")
            return pWeakUntilQ
        else do
            warn "False: Stop condition in Until operator is never satisfied"
            return False

release :: (IsProperty propa Path a, IsProperty propb Path a) => propa -> propb -> PathProperty a Bool
p `release` q = do
    qNow <- toProperty q
    if not qNow
        then do
            warn "False: Condition in Release operator returned False prior to the release condition being satisfied"
            return qNow
        else do
            pNow <- toProperty p
            if pNow
                then return True
                else do
                    canCont <- hasNext
                    if canCont
                        then nextFrame (p `release` q)
                        else return True
