module Test.Main (main) where

import Contract.Prelude

import CommitteCandidateValidator
  ( DeregisterParams(..)
  , RegisterParams(..)
  , deregister
  , register
  )
import Contract.Address (getWalletAddress, ownPaymentPubKeyHash)
import Contract.Monad (Contract, launchAff_, liftContractM, liftedM)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Utxos (utxosAt)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import Data.UInt as UInt
import RunFuelMintingPolicy (FuelParams(..), runFuelMP)
import SidechainParams (SidechainParams(..))
import Test.Config (config)

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = launchAff_ $ do
  let
    distribute = [ BigInt.fromInt 1_000_000_000, BigInt.fromInt 2_000_000_000 ]
      /\ [ BigInt.fromInt 2_000_000_000 ]

  runPlutipContract config distribute \(alice /\ _bob) → do
    withKeyWallet alice $ do
      mintAndBurnScenario
      registerAndDeregisterScenario

mintAndBurnScenario ∷ Contract () Unit
mintAndBurnScenario = do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← unwrap <$> liftedM "cannot get UTxOs" (utxosAt ownAddr)
  genesisMint ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let
    scParams = SidechainParams
      { chainId: BigInt.fromInt 1
      , genesisHash: hexToByteArrayUnsafe "aabbcc"
      , genesisMint: Just genesisMint
      , genesisUtxo: toTxIn "aabbcc" 0
      }
  runFuelMP scParams (Mint { amount: 5, recipient: pk })
  runFuelMP scParams
    (Burn { amount: 2, recipient: hexToByteArrayUnsafe "aabbcc" })
  runFuelMP scParams
    (Burn { amount: 3, recipient: hexToByteArrayUnsafe "aabbcc" })

registerAndDeregisterScenario ∷ Contract () Unit
registerAndDeregisterScenario = do
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← unwrap <$> liftedM "cannot get UTxOs" (utxosAt ownAddr)
  registrationUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let
    scParams = SidechainParams
      { chainId: BigInt.fromInt 1
      , genesisHash: hexToByteArrayUnsafe "aabbcc"
      , genesisMint: Nothing
      , genesisUtxo: toTxIn "aabbcc" 0
      }
    regParams =
      RegisterParams
        { sidechainParams: scParams
        , spoPubKey: hexToByteArrayUnsafe "ababab"
        , sidechainPubKey: hexToByteArrayUnsafe ""
        , spoSig: hexToByteArrayUnsafe ""
        , sidechainSig: hexToByteArrayUnsafe ""
        , inputUtxo: registrationUtxo
        }
    deregParams =
      DeregisterParams
        { sidechainParams: scParams
        , spoPubKey: hexToByteArrayUnsafe "ababab"
        }
  register regParams
  deregister deregParams

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
    , index: UInt.fromInt txIdx
    }