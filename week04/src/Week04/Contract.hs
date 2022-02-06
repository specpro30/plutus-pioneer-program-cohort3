{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a (where 'w' allows contract to write log messages for communicating with outside contract, 
--                   where 's' specifies endpoints, 'e' is error messages, 'a' is the result )
-- EmulatorTrace a

myContract1 :: Contract () Empty Text ()  -- this contract will throw an exception and stop contract execution
myContract1 = do
    void $ Contract.throwError "BOOM!"   -- throws an exception "BOOM!" will stop execution of contract
    Contract.logInfo @String "hello from the contract"  -- logging to console (not the 'w' kind?)

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1 -- need to use void for a unit result instead of returning a handle to contract

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- this contract will throw an exception and continue with contract execution
myContract2 :: Contract () Empty Void ()  -- Void is from Data.Void is type that has no values so Void as e will throw no exceptions
myContract2 = Contract.handleError        -- will throw a specified exception (below) instead and continue with contract execution
    (\err -> Contract.logError $ "caught: " ++ unpack err)  -- convert text to string with unpack
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- invoking endpoints and takes data from outside
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String  -- endpoint of 'foo' of type int '.\/' is a type operator is a type constructor
                                                              -- to help define another endpoint

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo   -- wait and block until the outside input is provided
                                                      -- once the int input is available then continues
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3  -- need to use handle 'h' to activate wallet
    callEndpoint @"foo" h 42            -- passing the value of int 42 and logs it
    callEndpoint @"bar" h "Haskell"     -- passing the value of string Haskell and logs it

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- this contract uses the 'w' to communicate with the outside world (in this case the emulator trace)
-- in real world case it would be a dapp UI
myContract4 :: Contract [Int] Empty Text ()   -- a list of [int] which is a Monoid starts with 'mempty'
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]                             -- will write a list with 1 inside with 'mappend'
    void $ Contract.waitNSlots 10
    tell [2]                             -- will write a list with 2 inside
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h        -- look up the state of a running contract and show it as xs
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h        -- look up the state of a running contract and show it as ys
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10 
    zs <- observableState h        -- look up the state of a running contract and show it as zs
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
