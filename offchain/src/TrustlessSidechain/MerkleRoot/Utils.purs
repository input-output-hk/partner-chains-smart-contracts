-- | `MerkleRoot.Utils` contains utility functions relating to the
-- | Merkle root endpoint including:
-- |
-- |      - Creating the data for onchain validators / minting policies
-- |
-- |      - Querying utxos regarding the Merkle root
-- |
-- | Note: the reason for the existence of this module is because there are some
-- | cyclic dependencies between `MerkleRoot` and `UpdateCommitteeHash` without
-- | this.
module TrustlessSidechain.MerkleRoot.Utils
  ( merkleRootCurrencyInfo
  , merkleRootTokenValidator
  , findMerkleRootTokenUtxo
  , findPreviousMerkleRootTokenUtxo
  , serialiseMrimHash
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (serializeData, toData)
import Contract.Scripts (Validator)
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value (TokenName)
import Contract.Value as Value
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( MerkleRootTokenPolicy
      , MerkleRootTokenValidator
      )
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `merkleRootCurrencyInfo` gets the minting policy and currency symbol
-- | corresponding to `MerkleRootTokenPolicy`
merkleRootCurrencyInfo ∷ SidechainParams → Contract CurrencyInfo
merkleRootCurrencyInfo sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mintingPolicy ← mkMintingPolicyWithParams MerkleRootTokenPolicy
    [ toData sidechainParams, toData versionOracleConfig ]
  currencySymbol ← getCurrencySymbol MerkleRootTokenPolicy mintingPolicy
  pure { mintingPolicy, currencySymbol }

-- | `merkleRootTokenValidator` gets the validator corresponding to
-- | 'MerkleRootTokenValidator' paramaterized by `SidechainParams`.
merkleRootTokenValidator ∷ SidechainParams → Contract Validator
merkleRootTokenValidator sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkValidatorWithParams MerkleRootTokenValidator
    [ toData sidechainParams, toData versionOracleConfig ]

-- | `findMerkleRootTokenUtxo merkleRoot smrm` locates a utxo which
-- |
-- |    1. is sitting at the some utxo with validator address
-- |    `merkleRootTokenValidator smrm.sidechainParams`
-- |
-- |    2. contains a token with `CurrencySymbol` `merkleRootTokenMintingPolicy smrm`
-- |    and `TokenName` as `merkleRoot`.
-- |
-- | Note: in the case that there is more than such utxo, this returns the first
-- | such utxo it finds that satisifies the aforementioned properties.
findMerkleRootTokenUtxo ∷
  TokenName →
  SidechainParams →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMerkleRootTokenUtxo merkleRoot sp = do
  netId ← Address.getNetworkId
  validator ← merkleRootTokenValidator sp
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← Monad.liftContractM
    "error 'findMerkleRootTokenUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  { currencySymbol } ← merkleRootCurrencyInfo sp

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: we just need the existence of the token i.e., there is a nonzero
    -- amount
    Value.valueOf value currencySymbol merkleRoot /= zero

-- | `findPreviousMerkleRootTokenUtxo maybeLastMerkleRoot smrm` returns `Nothing` in
-- | the case that `maybeLastMerkleRoot` is `Nothing`, and `Just` the result of
-- | `findMerkleRootTokenUtxo lastMerkleRoot smrm` provided that `Just lastMerkleRoot = maybeLastMerkleRoot`
-- | and there are no other errors.
-- | Note: the `Maybe` return type does NOT denote the absense or existence of
-- | finding the utxo... rather it reflects the `Maybe` in the last merkle root
-- | of whether it exists or not.
findPreviousMerkleRootTokenUtxo ∷
  Maybe RootHash →
  SidechainParams →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findPreviousMerkleRootTokenUtxo maybeLastMerkleRoot sp =
  case maybeLastMerkleRoot of
    Nothing → pure Nothing
    Just lastMerkleRoot' → do
      lastMerkleRootTokenName ← Monad.liftContractM
        "error 'saveRoot': invalid lastMerkleRoot token name"
        (Value.mkTokenName $ MerkleTree.unRootHash lastMerkleRoot')
      lkup ← findMerkleRootTokenUtxo lastMerkleRootTokenName sp
      lkup' ←
        Monad.liftContractM
          "error 'findPreviousMerkleRootTokenUtxo': failed to find last merkle root"
          $ lkup
      pure $ Just lkup'

-- | `serialiseMrimHash` is an alias for (ignoring the `Maybe`)
-- | ```purescript
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
serialiseMrimHash ∷ MerkleRootInsertionMessage → Maybe EcdsaSecp256k1Message
serialiseMrimHash =
  Utils.Crypto.ecdsaSecp256k1Message
    <<< Hashing.blake2b256Hash
    <<< cborBytesToByteArray
    <<< serializeData
