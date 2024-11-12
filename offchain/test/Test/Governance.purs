module Test.Governance (suite) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test.Testnet (withWallets)
import Contract.Wallet (withKeyWallet) as Wallet
import Mote.Monad (group, test)
import Test.Utils
  ( TestnetTest
  , fails
  , getOwnTransactionInput
  )
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.Governance.Utils (updateGovernance)
import TrustlessSidechain.InitSidechain.Governance (initGovernance)
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)

suite :: TestnetTest
suite = group "Governance init and update" $ do
  testScenarioSuccess
  testScenarioFailure

testScenarioSuccess :: TestnetTest
testScenarioSuccess =
  test "Init governance, followed by update, and init native tokens"
    do
      let
        initialDistribution =
          [ BigNum.fromInt 5_000_000
          , BigNum.fromInt 5_000_000
          , BigNum.fromInt 30_000_000
          , BigNum.fromInt 50_000_000
          ]
      withWallets (initialDistribution /\ initialDistribution) \(alice /\ bob) ->
        do
          unliftApp do

            bobPkh <- withUnliftApp (Wallet.withKeyWallet bob)
              getOwnPaymentPubKeyHash

            genesisUtxo <- withUnliftApp (Wallet.withKeyWallet alice) do
              pkh <- getOwnPaymentPubKeyHash
              genesisUtxo <- getOwnTransactionInput

              void $ initGovernance genesisUtxo pkh
              void $ updateGovernance genesisUtxo bobPkh

              pure genesisUtxo

            withUnliftApp (Wallet.withKeyWallet bob) do
              void $ initNativeTokenMgmt genesisUtxo

testScenarioFailure :: TestnetTest
testScenarioFailure =
  test "Init governance, and try to init native tokens using wrong authority"
    do
      let
        initialDistribution =
          [ BigNum.fromInt 5_000_000
          , BigNum.fromInt 5_000_000
          , BigNum.fromInt 30_000_000
          , BigNum.fromInt 50_000_000
          ]
      withWallets (initialDistribution /\ initialDistribution) \(alice /\ bob) ->
        do
          unliftApp do

            genesisUtxo <- withUnliftApp (Wallet.withKeyWallet alice) do
              pkh <- getOwnPaymentPubKeyHash
              genesisUtxo <- getOwnTransactionInput
              void $ initGovernance genesisUtxo pkh
              pure genesisUtxo

            withUnliftApp (Wallet.withKeyWallet bob) do
              (void $ initNativeTokenMgmt genesisUtxo) # withUnliftApp fails
