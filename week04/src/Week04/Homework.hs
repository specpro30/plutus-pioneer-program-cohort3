{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Data.Void             (Void)

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract1 :: Contract () PaySchema Text ()
payContract1 = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx 
    payContract1

payContract2 :: Contract () PaySchema Void ()
payContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught ya: " ++ unpack err)
    payContract1
    
-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h <- activateContractWallet (knownWallet 1) payContract2
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash (knownWallet 2)
        , ppLovelace  = x
        }
    void $ Emulator.waitNSlots 1
    i <- activateContractWallet (knownWallet 1) payContract2
    callEndpoint @"pay" i $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash (knownWallet 2)
        , ppLovelace  = y
        }
    void $ Emulator.waitNSlots 1



payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
