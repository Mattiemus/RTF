{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, DeriveFoldable, DeriveFunctor #-}

module Test.FRP.General where

import Data.Foldable

{--------------------------------------------------------------------
    Messages
--------------------------------------------------------------------}

type Message = String

class Monad m => Messageable m where
    warn :: Message -> m ()
    fatal :: Message -> m a

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
    Result { resultValue = f, resultWarnings = warnsa } <*> Result { resultValue = x, resultWarnings = warnsb } =
        Result { resultValue = f <*> x, resultWarnings = warnsa ++ warnsb }

instance Monad Result where
    return = pure
    result >>= f = error "Monad::Result::(>>=) not implemented"

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

newtype Property container a b = Prop (container (Value a) -> Result b)

instance Functor (Property container a) where
    fmap f (Prop prop) = Prop $ \input -> fmap f (prop input)

instance Applicative (Property container a) where
    pure x = Prop $ \_ -> pure x
    fs <*> xs = error "Applicative::Property::(<*>) not implemented"

instance Monad (Property container a) where
    return = pure
    Prop prop >>= f = Prop $ \input ->
        case prop input of
            Result { resultValue = Left s, resultWarnings = warns } -> Result { resultValue = Left s, resultWarnings = warns }
            results@Result { resultValue = Right x } ->
                case f x of
                    Prop fProp -> results `appendMessages` fProp input

instance Messageable (Property container a) where
    warn s = returnResult Result { resultValue = Right (), resultWarnings = [s] }
    fatal s = returnResult Result { resultValue = Left s, resultWarnings = [] }

returnResult :: Result b -> Property container a b
returnResult res = Prop $ const res

getPropInput :: Property container a (container (Value a))
getPropInput = Prop $ \input -> pure input

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

class IsProperty prop cont a where
    toProperty :: prop -> Property cont a Bool

instance a ~ b => IsProperty Bool cont b where
    toProperty = pure

instance (Foldable cont, a ~ b) => IsProperty (a -> Bool) cont b where
    toProperty prop = do
        input <- getPropInput
        let xs = toList input
        case xs of
            [] -> fatal "Not enough input to satisfy property"
            (Value (x, _) : _) -> pure (prop x)

instance (conta ~ contb, a ~ b) => IsProperty (Property conta a Bool) contb b where
    toProperty = id

{--------------------------------------------------------------------
    General property operations
--------------------------------------------------------------------}

(\/) :: (IsProperty propa cont a, IsProperty propb cont a) => propa -> propb -> Property cont a Bool
a \/ b = do
    aVal <- toProperty a
    bVal <- toProperty b
    return (aVal || bVal)

(/\) :: (IsProperty propa cont a, IsProperty propb cont a) => propa -> propb -> Property cont a Bool
a /\ b = do
    aVal <- toProperty a
    bVal <- toProperty b
    return (aVal && bVal)

{--------------------------------------------------------------------
    Timepoints
--------------------------------------------------------------------}

type TimePoint = Float

{--------------------------------------------------------------------
    Values
--------------------------------------------------------------------}

newtype Value a = Value (a, TimePoint)
    deriving (Foldable, Functor)

instance Eq a => Eq (Value a) where
    Value (x, _) == Value (y, _) = x == y

instance Ord a => Ord (Value a) where
    Value (x, _) `compare` Value (y, _) = x `compare` y
