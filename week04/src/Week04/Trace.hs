{-# LANGUAGE TypeApplications #-} -- allows you to use the '@' 
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras      -- for the Extra.logInfo 
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints -- start endpoints contract with knownWallet 1
    h2 <- activateContractWallet (knownWallet 2) endpoints -- start endpoints contract with knownWallet 2
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20  -- uses the 'def' which is the default config
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20     -- use void to ignore the 20 as a result
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 2           -- waits for two more slots
    Extras.logInfo $ "reached " ++ show s
