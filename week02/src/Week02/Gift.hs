{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}  --Haskell pragma to allow the compiler to inline the definition of mkValidator in the || brackets
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()   --Datum, Redeemer and Context data type is BuiltinData.
                                                                 -- "()" Unit is the return type, similar to void
mkValidator _ _ _ = ()                                           --always passes validation. 

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])  --the oxford brackets "||" converts a Haskell expression to a plutus core syntax tree of that expression
                                                                        --"$$" splices the syntax tree into source code at that point. The result is like hardcoding plutus core at that point in code
                                                                        --the mkValidatorScript then turns it into a validator
valHash :: Ledger.ValidatorHash            --Creates a hash of the validator
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address               --turns validator into an address on the blockchain
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer        --end points allow user to trigger something. "give" will send Lovelaces
        .\/ Endpoint "grab" ()             --"grab" will spend Lovelaces

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount --The datum is just an arbitrary one. Will pay the amount to the valHash
    ledgerTx <- submitTx tx                                                                     --This line will wait for confirmation
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount                                --Logs to playground

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress                                --finds all utxo sitting at that address
    let orefs   = fst <$> Map.toList utxos                     --gets all the references to these utxos
        lookups = Constraints.unspentOutputs utxos      <>     --tell wallet where to find utxos
                  Constraints.otherScript validator            --spending transactio has to provide the validator in the transaction
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]  --list of constraints for each utxo at give address. the tx being constructed must spend the utxo
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             --helps wallet to construct the tx like how to find the utxos etc
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --logs message
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give          --blocks and waits for user to provide an integer
    grab' = endpoint @"grab" $ const grab  --blocks until the user unblocks

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
