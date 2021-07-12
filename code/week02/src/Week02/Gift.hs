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

-- ElEstado -> Lafuncion que se quiere ejecutar -> La tx (entradas y salidas)
-- Data es un ADT definido en PlutusTx
-- La funcion de validacion devuelve unit. Si es valida la tx -> () else error ?
-- mkValidator :: Datum -> Redeemer -> Context
-- En este caso esto es un pasa todo
-- Si se manda ADA al address de este script, cualquiera puede venir ir hacer lo que quiera
-- porque la validacion pasa siempre
-- Lo que se hace onchain es la validacion
-- El INLINABLE sive para que te copie y pege este codigo dentro del [|| <ACA> ||]
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

-- Creacion del validator, compilar la funcion Haskell a plutus core
-- El address del script se obtiene con el hash del codigo plutus compilado mismo
-- Esto usa template Haskel -> Generar codigo en compile time, Lo inserta en el codigo (AST) y dsp se compila
-- [|| mkValidator ||] Con eso se quotea y se obtiene el AST de esa funcion
-- compile es una funcion que agarra un AST y lo transforma al AST de plutus
-- $$() esta funcion toma un AST y lo splice en el codigo en ese punto
-- De esta forma escribimos en mkValidator en haskell y lo transforma solo en PlutusCore
-- CompiledCode ?
-- para ver el codigo plutus en la terminal -> unScript . getValidator $ validator
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- Para ver el hash del validador
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- Address del script
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer -- Para darle adas al script
        .\/ Endpoint "grab" ()      -- Agarra todos los adas y se los manda al user

-- Esta tx da plata entonces solo hace falta el hash
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

-- Spending tx tiene que pasar el validador
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxoAt scrAddress   -- Agarra todos los utxo que perteneces al script 
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
