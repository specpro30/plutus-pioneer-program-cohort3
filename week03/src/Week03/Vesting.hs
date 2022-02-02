{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

data VestingDatum = VestingDatum        -- this is a custom type of Datum
    { beneficiary :: PaymentPubKeyHash  -- the hash of the public payment key
    , deadline    :: POSIXTime
    } deriving Show                     -- can display values of this type in console

PlutusTx.unstableMakeIsData ''VestingDatum  -- use the stable version in production

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool   -- no redeember is required as long as the deadline is reached and the key is correct
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&  -- helper function to check if signed by beneficiary
                         traceIfFalse "deadline not reached" deadlineReached                    -- helper function to check if dead line is reached 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  -- ctx is of type scriptContextTxInfo

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams     -- person giving the money 
        .\/ Endpoint "grab" ()             -- person receiving the money (the beneficiary)

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp  -- giving the gpAmount to the beneficiary at the output of the script address
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress  -- the utxos at the script address are filtered using the isSuitable function
    if Map.null utxos
        then logInfo @String $ "no gifts available" -- if no utxo then log this
        else do                                     -- if there are utxos with gifts then get all of them
            let orefs   = fst <$> Map.toList utxos  -- get all the txout refs of all the utxos that want to collect
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>  -- loop over all the orefs that are valid and get (spend) them
                          Constraints.mustValidateIn (from now)     -- must be later then now
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool   -- ChainIndexTxOut is the output of the utxo with the funds for the beneficiary
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False  -- there maybe hashes of Datums at the utxo which are not usable. this line checks for those and return false if found
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of  -- if there is a datum attached then deserialize it to the VestingDatum type
            Nothing -> False
            Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []

{- Prelude Plutus.V1.Ledger.Interval Week03.Deploy> import Wallet.Emulator
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> :i Wallet
type Wallet :: *
newtype Wallet = Wallet {getWalletId :: WalletId}
  	-- Defined in ‘Wallet.Emulator.Wallet’
instance Eq Wallet -- Defined in ‘Wallet.Emulator.Wallet’
instance Ord Wallet -- Defined in ‘Wallet.Emulator.Wallet’
instance Show Wallet -- Defined in ‘Wallet.Emulator.Wallet’
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> knownWallet 2
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> knownWallet 3
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> mockWalletPaymentPubKeyHash $ knownWallet 2
80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> mockWalletPaymentPubKeyHash $ knownWallet 3
2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Week03.Deploy> import Ledger.TimeSlot
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Ledger.TimeSlot Week03.Deploy> import Data.Default
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Ledger.TimeSlot Data.Default Week03.Deploy> slotToBeginPOSIXTime def 10
POSIXTime {getPOSIXTime = 1596059101000}
Prelude Plutus.V1.Ledger.Interval Wallet.Emulator Ledger.TimeSlot Data.Default Week03.Deploy> slotToBeginPOSIXTime def 20
POSIXTime {getPOSIXTime = 1596059111000} -}

