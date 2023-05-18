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
  ( merkleRootTokenMintingPolicy
  , merkleRootTokenValidator
  , findMerkleRootTokenUtxo
  , findPreviousMerkleRootTokenUtxo
  , serialiseMrimHash
  , normalizeSaveRootParams
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value (TokenName)
import Contract.Value as Value
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRootMint
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Crypto (SidechainMessage)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Utxos as Utils.Utxos

-- | `normalizeSaveRootParams` modifies the following fields in
-- | `SaveRootParams` fields to satisfy the following properties
-- |    - `committeeSignatures` is sorted (lexicographically) by the
-- |    `SidechainPublicKey`.
normalizeSaveRootParams ∷ SaveRootParams → SaveRootParams
normalizeSaveRootParams (SaveRootParams p) =
  SaveRootParams p
    { committeeSignatures = Utils.Crypto.normalizeCommitteePubKeysAndSignatures
        p.committeeSignatures
    }

-- | `merkleRootTokenMintingPolicy` gets the minting policy corresponding to
-- | `RawScripts.rawMerkleRootTokenMintingPolicy` paramaterized by the given
-- | `SignedMerkleRootMint`.
merkleRootTokenMintingPolicy ∷ SignedMerkleRootMint → Contract MintingPolicy
merkleRootTokenMintingPolicy sp = do
  let
    script = decodeTextEnvelope RawScripts.rawMerkleRootTokenMintingPolicy
      >>= plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
    [ PlutusData.toData sp ]
  pure $ PlutusMintingPolicy applied

-- | `merkleRootTokenValidator` gets the validator corresponding to
-- | 'RawScripts.rawMerkleRootTokenValidator' paramaterized by `SidechainParams`.
merkleRootTokenValidator ∷ SidechainParams → Contract Validator
merkleRootTokenValidator sp = do
  let
    script = decodeTextEnvelope RawScripts.rawMerkleRootTokenValidator
      >>= plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
    [ PlutusData.toData sp ]
  pure $ Validator applied

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
  SignedMerkleRootMint →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMerkleRootTokenUtxo merkleRoot smrm = do
  netId ← Address.getNetworkId
  validator ← merkleRootTokenValidator (unwrap smrm).sidechainParams
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← Monad.liftContractM
    "error 'findMerkleRootTokenUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  mintingPolicy ← merkleRootTokenMintingPolicy smrm
  currencySymbol ←
    Monad.liftContractM
      "error 'findMerkleRootTokenUtxo': failed to get currency symbol for minting policy"
      $ Value.scriptCurrencySymbol mintingPolicy

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
  SignedMerkleRootMint →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findPreviousMerkleRootTokenUtxo maybeLastMerkleRoot smrm =
  case maybeLastMerkleRoot of
    Nothing → pure Nothing
    Just lastMerkleRoot' → do
      lastMerkleRootTokenName ← Monad.liftContractM
        "error 'saveRoot': invalid lastMerkleRoot token name"
        (Value.mkTokenName $ MerkleTree.unRootHash lastMerkleRoot')
      lkup ← findMerkleRootTokenUtxo lastMerkleRootTokenName smrm
      lkup' ←
        Monad.liftContractM
          "error 'findPreviousMerkleRootTokenUtxo': failed to find last merkle root"
          $ lkup
      pure $ Just lkup'

-- | `serialiseMrimHash` is an alias for (ignoring the `Maybe`)
-- | ```purescript
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
serialiseMrimHash ∷ MerkleRootInsertionMessage → Maybe SidechainMessage
serialiseMrimHash =
  Utils.Crypto.sidechainMessage
    <<< Hashing.blake2b256Hash
    <<< cborBytesToByteArray
    <<< PlutusData.serializeData
