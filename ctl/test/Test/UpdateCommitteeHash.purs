module Test.UpdateCommitteeHash where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Data.Array as Array
import Data.BigInt as BigInt
import InitSidechain (initSidechain)
import Serialization.Types (PrivateKey)
import SidechainParams (InitSidechainParams(..), SidechainParams)
import Test.Utils as Test.Utils
import UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(..)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto (generatePrivKey, multiSign, toPubKeyUnsafe)

-- | 'updateCommitteeHash' is a convenient wrapper around
-- 'UpdateCommitteeHash.updateCommitteeHash' for writing tests.
-- Note that this makes the entire current committee sign the message.
updateCommitteeHash ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe ByteArray
  } →
  Contract () Unit
updateCommitteeHash
  { sidechainParams
  , currentCommitteePrvKeys
  , newCommitteePrvKeys
  , previousMerkleRoot
  } = do
  let
    -- Order the private keys by lexicographical ordering of the signatures, so
    -- it's easy to give the sorted pubkey with its associated signature.
    currentCommitteePubKeys /\ currentCommitteePrvKeys' =
      Array.unzip
        $ Array.sortWith fst
        $ map (\prvKey → toPubKeyUnsafe prvKey /\ prvKey) currentCommitteePrvKeys

    newCommitteePubKeys = Array.sort $ map toPubKeyUnsafe newCommitteePrvKeys

  committeeMessage ←
    liftContractM
      "error 'Test.UpdateCommitteeHash.updateCommitteeHash': failed to serialise and hash update committee hash message"
      $ UpdateCommitteeHash.serialiseUchmHash
      $ UpdateCommitteeHashMessage
          { sidechainParams
          , newCommitteePubKeys: newCommitteePubKeys
          , previousMerkleRoot
          }
  let
    committeeSignatures = Array.zip
      currentCommitteePubKeys
      (Just <$> multiSign currentCommitteePrvKeys' committeeMessage)

    uchp =
      UpdateCommitteeHashParams
        { sidechainParams
        , newCommitteePubKeys: newCommitteePubKeys
        , committeeSignatures: committeeSignatures
        , previousMerkleRoot
        }

  UpdateCommitteeHash.updateCommitteeHash uchp

-- | 'testScenario' updates the committee hash
testScenario ∷ Contract () Unit
testScenario = do
  logInfo' "UpdateCommitteeHash 'testScenario'"
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  let
    keyCount = 25
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

  updateCommitteeHash
    { sidechainParams: scParams
    , currentCommitteePrvKeys: initCommitteePrvKeys
    , newCommitteePrvKeys: nextCommitteePrvKeys
    , previousMerkleRoot: Nothing
    }
