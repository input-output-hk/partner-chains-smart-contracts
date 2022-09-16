module Test.UpdateCommitteeHash where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import InitSidechain (initSidechain)
import SidechainParams (InitSidechainParams(..))
import UpdateCommitteeHash
  ( UpdateCommitteeHashParams(..)
  , aggregateKeys
  , updateCommitteeHash
  )
import Utils.Crypto (generatePrivKey, multiSign, toPubKeyUnsafe)

testScenario ∷ Contract () Unit
testScenario = do
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
