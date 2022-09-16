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
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.UInt as UInt
import InitSidechain (initSidechain)
import RunFuelMintingPolicy (FuelParams(..), runFuelMP)
import SidechainParams (InitSidechainParams(..), SidechainParams(..))
import Test.Config (config)
import UpdateCommitteeHash
  ( UpdateCommitteeHashParams(..)
  , aggregateKeys
  , updateCommitteeHash
  )
import Utils.Crypto (generatePrivKey, multiSign, toPubKeyUnsafe)

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
      initAndUpdateCommitteeHashScenario

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
  runFuelMP scParams (Mint { amount: BigInt.fromInt 5, recipient: pk })
  runFuelMP scParams
    (Burn { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" })
  runFuelMP scParams
    (Burn { amount: BigInt.fromInt 3, recipient: hexToByteArrayUnsafe "aabbcc" })

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

initAndUpdateCommitteeHashScenario ∷ Contract () Unit
initAndUpdateCommitteeHashScenario = do
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← unwrap <$> liftedM "cannot get UTxOs" (utxosAt ownAddr)
  let
    keyCount = 101
    threshold = 2.0 / 3.0
    reqSigns = Int.ceil $ Int.toNumber keyCount / threshold
  genesisUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  committeePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    initCommittee = map toPubKeyUnsafe committeePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initMint: Nothing
      , initUtxo: genesisUtxo
      , initCommittee
      }

  scParams ← initSidechain initScParams
  nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let nextCommittee = Array.sort $ map toPubKeyUnsafe nextCommitteePrvKeys
  nextCommitteeHash ← aggregateKeys nextCommittee
  let
    sigs = multiSign (Array.take reqSigns committeePrvKeys) nextCommitteeHash

    uchp =
      UpdateCommitteeHashParams
        { sidechainParams: scParams
        , newCommitteePubKeys: nextCommittee
        , committeePubKeys: initCommittee
        , committeeSignatures: sigs
        }

  updateCommitteeHash uchp
