{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, DeriveFoldable, DeriveFunctor #-}

module Test.FRP.General where

import Data.Foldable

import Control.Applicative
import Control.Monad

{--------------------------------------------------------------------
    Messages
--------------------------------------------------------------------}

type Message = String

class Monad m => Messageable m where
    warn :: Message -> m ()
    fatal :: Message -> m a

ifFalseWarn :: Messageable m => m Bool -> Message -> m Bool
ifFalseWarn gen msg = do
    result <- gen
    unless result (warn msg)
    return result

{--------------------------------------------------------------------
    Results
--------------------------------------------------------------------}

data Result a
    = Result {
         resultValue :: Either Message a
        ,resultWarnings :: [Message]
    }
    deriving (Show)

instance Functor Result where
    fmap f result = result { resultValue = fmap f (resultValue result) }

instance Applicative Result where
    pure x = Result { resultValue = Right x, resultWarnings = [] }
    Result { resultValue = f, resultWarnings = warnsA } <*> Result { resultValue = x, resultWarnings = warnsB } =
        Result { resultValue = f <*> x, resultWarnings = warnsB ++ warnsA }

instance Monad Result where
    return = pure
    (Result { resultValue = Left s, resultWarnings = warns }) >>= _ = Result { resultValue = Left s, resultWarnings = warns }
    (Result { resultValue = Right x, resultWarnings = warnsA }) >>= f =
        let Result { resultValue = result, resultWarnings = warnsB } = f x
        in Result { resultValue = result, resultWarnings = warnsB ++ warnsA }

appendMessages :: Result a -> Result b -> Result b
a `appendMessages` b = b { resultWarnings = resultWarnings b ++ resultWarnings a }

printResult :: Result Bool -> IO ()
printResult result =
    case result of
        Result { resultValue = Left err, resultWarnings = warns } -> do
            putStrLn $ "Test failed due to fatal error: " ++ err
            putStrLn ""
            mapM_ putStrLn warns
        Result { resultValue = Right False, resultWarnings = warns } -> do
            putStrLn "Test failed"
            putStrLn ""
            mapM_ putStrLn warns
        Result { resultValue = Right True } ->
            putStrLn "Test passed"

{--------------------------------------------------------------------
    Property
--------------------------------------------------------------------}

newtype Property container a b = Prop { unProp :: container (Value a) -> Result b }

instance Functor (Property container a) where
    fmap f (Prop prop) = Prop $ \input -> fmap f (prop input)

instance Applicative (Property container a) where
    pure x = Prop $ \_ -> pure x
    (Prop fs) <*> (Prop x) = Prop $ \input -> fs input <*> x input

instance Monad (Property container a) where
    return = pure
    Prop prop >>= f = Prop $ \input ->
        case prop input of
            Result { resultValue = Left s, resultWarnings = warns } -> Result { resultValue = Left s, resultWarnings = warns }
            results@Result { resultValue = Right x } -> results `appendMessages` unProp (f x) input

instance Messageable (Property container a) where
    warn s = returnResult Result { resultValue = Right (), resultWarnings = [s] }
    fatal s = returnResult Result { resultValue = Left s, resultWarnings = [] }

returnResult :: Result b -> Property container a b
returnResult res = Prop $ const res

getPropInput :: Property container a (container (Value a))
getPropInput = Prop $ \input -> pure input

getFilteredInput :: MonadPlus container => (Value a -> Bool) -> Property container a (container (Value a))
getFilteredInput f = Prop $ \input -> pure (mfilter f input)

getValue :: Foldable container => Property container a (Value a)
getValue = do
    input <- getPropInput
    let xs = toList input
    case xs of
        [] -> fatal "Could not get current value because the frame has no value attatched to it"
        (x : _) -> return x

runProperty :: container (Value a) -> Property container a b -> Result b
runProperty input (Prop prop) = prop input

runSubProperty :: container (Value a) -> Property container a b -> Property container a b
runSubProperty input prop = returnResult $ runProperty input prop

{--------------------------------------------------------------------
    General properties
--------------------------------------------------------------------}

class IsProperty prop container a where
    toProperty :: prop -> Property container a Bool

instance a ~ b => IsProperty Bool container b where
    toProperty = pure

instance (Foldable container, a ~ b) => IsProperty (a -> Bool) container b where
    toProperty prop = do
        Value (x, _) <- getValue
        pure (prop x)

instance (containera ~ containerb, a ~ b) => IsProperty (Property containera a Bool) containerb b where
    toProperty = id

{--------------------------------------------------------------------
    General property operations
--------------------------------------------------------------------}

(\/) :: (IsProperty propa container a, IsProperty propb container a) => propa -> propb -> Property container a Bool
a \/ b = liftA2 (||) (toProperty a) (toProperty b)

(/\) :: (IsProperty propa container a, IsProperty propb container a) => propa -> propb -> Property container a Bool
a /\ b = liftA2 (&&) (toProperty a) (toProperty b)

implies :: (IsProperty propa container a, IsProperty propb container a) => propa -> propb -> Property container a Bool
a `implies` b = do
    aVal <- toProperty a
    if aVal
        then toProperty b
        else return True

(-->) :: (IsProperty propa container a, IsProperty propb container a) => propa -> propb -> Property container a Bool
(-->) = implies

pNot :: IsProperty propa container a => propa -> Property container a Bool
pNot prop = fmap not (toProperty prop)

{--------------------------------------------------------------------
    Time
--------------------------------------------------------------------}

type TimePoint = Float

type TimeSpan = Float

{--------------------------------------------------------------------
    Values
--------------------------------------------------------------------}

newtype Value a = Value (a, TimePoint)
    deriving (Show, Foldable, Functor)

instance Eq a => Eq (Value a) where
    Value (x, _) == Value (y, _) = x == y

instance Ord a => Ord (Value a) where
    Value (x, _) `compare` Value (y, _) = x `compare` y
