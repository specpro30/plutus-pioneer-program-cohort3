{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Typed where             --the typed version is more high level and nicer to program with but is more resource intensive

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins    as Builtins
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts      -- using the typed version of Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool         --don't care about datum so use unit () type, use Integer type for Redeemer, ScriptContext. If type type validator returns true then validation is OK otherwise if return false then it will give error
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42   --traceIfFalse (BuiltinString -> Bool --> Bool) displays an error message "wrong redeemer" if the Bool returns a false
                                                              --if the second argument Bool returns true then first argument (the string "wrong redeemer") is ignored and will return true
data Typed                                         --a new type that encodes info of Datum and Redeemer
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()             --Datum type is unit ()
    type instance RedeemerType Typed = Integer     --Redeemer type is Integer

typedValidator :: Scripts.TypedValidator Typed     --boiler plate for typed Plutus
typedValidator = Scripts.mkTypedValidator @Typed   
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])              --wrap provides translation between strong type and low level version
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =                                  --off chain code is almost the same to untyped version
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount    --mustPayToTheScript only involves one script (not a must but common to use just one script)
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
