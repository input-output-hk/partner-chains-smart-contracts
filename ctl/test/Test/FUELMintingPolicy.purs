module Test.FUELMintingPolicy (testScenario) where

import Contract.Prelude

import Contract.Address
  ( getWalletAddress
  , ownPaymentPubKeyHash
  )
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import FUELMintingPolicy (FuelParams(..), passiveBridgeMintParams, runFuelMP)
import SidechainParams (SidechainParams(..))
import Test.Utils (toTxIn)

{-| Testing Passive bridge minting (genesis mint) and burning multiple times -}
testScenario ∷ Contract () Unit
testScenario = do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
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
  void $ runFuelMP scParams
    ( passiveBridgeMintParams scParams
        { amount: BigInt.fromInt 5, recipient: pk }
    )
  void $ runFuelMP scParams
    (Burn { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" })
  void $ runFuelMP scParams
    (Burn { amount: BigInt.fromInt 3, recipient: hexToByteArrayUnsafe "aabbcc" })
