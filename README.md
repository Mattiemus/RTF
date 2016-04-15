# Reactive Testing Framework
The reactive testing framework is a toolkit for testing functional reactive programs written in Haskell. The framework has been written to be generalised over any arrowised FRP library, and a compatability layer has been included to allow the library to work with netwire programs.

# Instillation
RTF includes a cabal build script, to build the samples run the following from a terminal:

    $ cabal configure
    $ cabal build
    
# Documentation
The source is fully documented, haddock documentation can be built using haddock:

    $ cabal configure 
    $ cabal haddock --internal
    
# Example
Writing tests is simple, and takes only three steps. First write your test (shim) program. In this case we are going to test the "hold" function, however in order to make testing easier, we pair the output from hold with the input we give to it.

    testWire :: Wire (Event Int) (Int, Event Int)
    testWire x = hold <+> pure x &&& id
    
We can then write our test property. RTF is a property based testing framework, so individual tests are essentially a high level description of what you want your program to do. 

    testProp :: Int -> PathProperty (Int, Event Int) Bool
    testProp x = (((==x) . fst) ‘weakUntil‘ (occurred . snd)) /\ (testPropCont ‘once‘ (occurred . snd))

    testPropCont :: PathProperty (Int, Event Int) Bool
    testPropCont = do
        (x, _) <- getCurrentValue
        tryNextFrame True (testProp x)

As we use property based testing, we dont need to provide the input - the library will generate that for us! The last step is to run the test.

    Main> runBasicTest (testWire 10) (testProp 10)
    All tests completed successfully
    
In this case everything succeeded, if we were to provide a test that would fail, say mismatch the first output expected and actual values, we will get a debug trace of what went wrong.

    Main> runBasicTest (testWire 10) (testProp 15)
    Test failed

    Path property was not satisfied along one of the selected paths
    Condition in WeakUntil operator returned False prior to the stop condition being satisfied
    Condition in Release operator returned False prior to the release condition being satisfied

    Test failed. Stopping.

# Areas for Improvement
This library was written as a research project, and as such has a few shortcommings, most notably:

  * Debugging is difficult as the values that caused the failure are difficult to find - even if they were printed they would likely be incorrect, as it may have been a prior value which caused the failure  
  * As our framework uses monads it is possible to cause space-time leaks, however this is largely mitigated by running the tests as strict IO operators, though this means we loose the benefits of lazy evaluation
  * More fine grained control over manually provided input is required - especially when creating bounded randomised values