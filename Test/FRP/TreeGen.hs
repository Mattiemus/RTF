{-# LANGUAGE ExistentialQuantification #-}

module Test.FRP.TreeGen where

import Data.Tree

import System.Random

import Test.FRP.General
import Test.FRP.Tree
import Test.FRP.Path

{--------------------------------------------------------------------
    Generator state
--------------------------------------------------------------------}

data GenState g
    = RandomGen g => GenState {
         genRandom :: g
        ,genClock :: TimePoint
        ,genClockDelta :: TimeSpan
    }

defaultGenState :: RandomGen g => g -> GenState g
defaultGenState gen = GenState { genRandom = gen, genClock = 0.0, genClockDelta = 0.1 }

{--------------------------------------------------------------------
    Tree generator
--------------------------------------------------------------------}

data Gen g a b = Gen { unGen :: GenState g -> (b, Maybe (ProgTree (Value a)) -> Maybe (ProgTree (Value a)), GenState g) }

instance Functor (Gen g a) where
    fmap f (Gen g) = Gen $ \state ->
        let (x, tree, nextState) = g state
        in (f x, tree, nextState)

instance Applicative (Gen g a) where
    pure x = Gen $ \state -> (x, id, state)
    _ <*> _ = error "Applicative::Gen::(<*>) not implemented"

instance Monad (Gen g a) where
    return = pure
    Gen g >>= f = Gen $ \state ->
        let (x, treeA, nextState) = g state
            (y, treeB, lastState) = unGen (f x) nextState
        in (y, treeA . treeB, lastState)

{--------------------------------------------------------------------
    Execution
--------------------------------------------------------------------}

runDefaultGen :: Gen StdGen a b -> IO (b, Maybe (ProgTree (Value a)))
runDefaultGen (Gen g) = do
    -- Create the state
    randGen <- newStdGen
    let state = defaultGenState randGen
        (x, tree, _) = g state
        resultTree = tree Nothing
    return (x, resultTree)

runDefaultGen_ :: Gen StdGen a b -> IO (Maybe (ProgTree (Value a)))
runDefaultGen_ gen = do
    (_, result) <- runDefaultGen gen
    return result

{--------------------------------------------------------------------
    Primitive operations
--------------------------------------------------------------------}

get :: Gen g a (GenState g)
get = Gen $ \state -> (state, id, state)

put :: GenState g -> Gen g a ()
put state = Gen $ const ((), id, state)

state :: (GenState g -> GenState g) -> Gen g a ()
state f = Gen $ \state -> ((), id, f state)

withState :: (GenState g -> (b, GenState g)) -> Gen g a b
withState f = Gen $ \state ->
    let (x, newState) = f state
    in (x, id, newState)

getClock :: Gen g a TimePoint
getClock = withState (\s -> (genClock s, s))

setClock :: TimePoint -> Gen g a ()
setClock tp = state (\s -> s { genClock = tp })

stepClock :: Gen g a ()
stepClock = state (\s -> s { genClock = genClock s + genClockDelta s })

setClockDelta :: TimeSpan -> Gen g a ()
setClockDelta span = state (\s -> s { genClockDelta = span })

{--------------------------------------------------------------------
    Primitive generators
--------------------------------------------------------------------}

putVal :: Value a -> Gen g a ()
putVal x = Gen $ \state -> ((), addValue, state)
    where
        addValue Nothing = Just (ProgTree (pure x))
        addValue (Just next) = Just (ProgTree (Node x [unProgTree next]))

putValue :: a -> Gen g a ()
putValue x = do
    tp <- getClock
    putVal (Value (x, tp))
    stepClock

putValues :: [a] -> Gen g a ()
putValues = mapM_ putValue

putRandValue :: (RandomGen g, Random a) => Gen g a ()
putRandValue = do
    x <- withState (\s -> let (val, nextG) = random (genRandom s)
                          in (val, s { genRandom = nextG }))
    putValue x

putRandRValue :: (RandomGen g, Random a) => a -> a -> Gen g a ()
putRandRValue min max = do
    x <- withState (\s -> let (val, nextG) = randomR (min, max) (genRandom s)
                          in (val, s { genRandom = nextG }))
    putValue x
