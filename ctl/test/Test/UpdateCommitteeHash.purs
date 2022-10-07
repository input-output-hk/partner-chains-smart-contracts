module Test.UpdateCommitteeHash where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import InitSidechain (initSidechain)
import SidechainParams (InitSidechainParams(..))
import UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(..)
  , serialiseUchmHash
  , updateCommitteeHash
  )
import Utils.Crypto (generatePrivKey, multiSign, toPubKeyUnsafe)

testScenario ∷ Contract () Unit
testScenario = do
  logInfo' "UpdateCommitteeHash 'testScenario'"
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
  let
    keyCount = 25
  genesisUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initMint: Nothing
      , initUtxo: genesisUtxo
      , initCommittee: initCommitteePubKeys
      }

  scParams ← initSidechain initScParams
  nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    nextCommittee = Array.sort $ map toPubKeyUnsafe nextCommitteePrvKeys

  committeeMessage ←
    liftContractM
      "error 'Test.UpdateCommitteeHash.testScenario': failed to serialise and hash update committee hash message"
      $ serialiseUchmHash
      $ UpdateCommitteeHashMessage
          { sidechainParams: scParams
          , newCommitteePubKeys: nextCommittee
          , previousMerkleRoot: Nothing
          }
  let
    committeeSignatures = Array.zip
      initCommitteePubKeys
      (Just <$> multiSign initCommitteePrvKeys committeeMessage)

    uchp =
      UpdateCommitteeHashParams
        { sidechainParams: scParams
        , newCommitteePubKeys: nextCommittee
        , committeeSignatures: committeeSignatures
        , lastMerkleRoot: Nothing
        }

  updateCommitteeHash uchp
