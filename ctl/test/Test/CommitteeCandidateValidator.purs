module Test.CommitteeCandidateValidator (testScenario) where

import Contract.Prelude

import CommitteCandidateValidator
  ( DeregisterParams(..)
  , RegisterParams(..)
  , deregister
  , register
  )
import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import SidechainParams (SidechainParams(..))
import Test.Utils (toTxIn)

testScenario ∷ Contract () Unit
testScenario = do
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
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
