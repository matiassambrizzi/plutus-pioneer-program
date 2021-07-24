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

-- Contract w s e a
-- EmulatorTrace a
--
--
-- Contract w s e a
--
-- w -> Writer, log messages, comunicacion entre contratos
-- s -> Endpoints
-- e -> Error messages
-- a -> Resultado
--
-- EmulatorTrace a
-- OverloadedStrings -> Usa el typeclass IsString, que define una funcion fromString :: String -> a
-- el compilador agarra el string y le aplica fromString ej
-- foo :: Text -> Text
-- foo x = x ++ "Hola" ==El compilador lo traduce a== foo x = x + fromString "Hola"
-- TypeApplications -> Sirve para pasar el tipo a las funcione polimorifcas con @
-- TypeOperators -> Extension que sirve para definir operadores sobre tipos ej \/

matiContract :: Contract () EmptySchema Text ()
matiContract = 
  void $ Contract.throwError "Boom" >>= \x ->
  Contract.logInfo @String "Hello world"

matiTrace :: EmulatorTrace ()
matiTrace = do
  handle <- activateContractWallet (Wallet 1) contractWithSchema
  callEndpoint @"hola" handle 42
  callEndpoint @"bar" handle "chau"

testMati :: IO ()
testMati = runEmulatorTraceIO matiTrace

matiContract' :: Contract () EmptySchema Void ()
matiContract' = 
  Contract.handleError 
    (\e -> Contract.logError $ "Agarramo el error " ++ unpack e) 
    matiContract

-- "hola" --> type level String, string que se usa como tipo
-- necesito DataKinds para esto ?
type TestSchema = Endpoint "hola" Int .\/ Endpoint "bar" String

contractWithSchema :: Contract () TestSchema Text ()
contractWithSchema = do
  x <- endpoint @"hola" 
  -- Monadic computation, hay q esescificar el endpoint, espera q que le pasen el valor desde fuera, en este caso Int
  Contract.logInfo x
  y <- endpoint @"bar"
  Contract.logInfo y

contractWithW :: Contract [Int] Empty Text ()
contractWithW = do
  void $ Contract.waitNSlots 10
  tell [1]
  void $ Contract.waitNSlots 10
  tell [2]

traceContractWithW :: EmulatorTrace ()
traceContractWithW = do
  h <- activateContractWallet (Wallet 1) contractWithW
  void $ Emulator.waitNSlots 5
  xs <- observableState h
  Extras.logInfo $ show xs

myContract1 :: Contract () Empty Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @"foo"
    Contract.logInfo n
    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
