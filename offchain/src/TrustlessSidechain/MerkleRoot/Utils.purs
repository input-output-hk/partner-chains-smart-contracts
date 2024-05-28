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
  ( findPreviousMerkleRootTokenUtxo
  , merkleRootCurrencyInfo
  , merkleRootTokenValidator
  , findMerkleRootTokenUtxo
  , serialiseMrimHash
  )
  where

import Contract.Prelude
import Partial.Unsafe (unsafePartial)
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.PlutusData (toData)
import Cardano.AsCbor (encodeCbor)
import Contract.Scripts (Validator)
import Contract.Scripts as Scripts
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.Value as Value
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(InvalidData))
import Contract.Numeric.BigNum as BigNum
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo, toAddress)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( MerkleRootTokenPolicy
      , MerkleRootTokenValidator
      )
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.Asset (Asset(Asset))

-- | `merkleRootCurrencyInfo` gets the minting policy and currency symbol
-- | corresponding to `MerkleRootTokenPolicy`
merkleRootCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) CurrencyInfo
merkleRootCurrencyInfo sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  getCurrencyInfo MerkleRootTokenPolicy
    [ toData sidechainParams, toData versionOracleConfig ]

-- | `merkleRootTokenValidator` gets the validator corresponding to
-- | 'MerkleRootTokenValidator' paramaterized by `SidechainParams`.
merkleRootTokenValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
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
  ∀ r.
  AssetName →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findMerkleRootTokenUtxo merkleRoot sp = do
  validator ← merkleRootTokenValidator sp
  validatorAddress ← toAddress (Scripts.validatorHash validator)
  { currencySymbol } ← merkleRootCurrencyInfo sp

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: we just need the existence of the token i.e., there is a nonzero
    -- amount
    Value.valueOf (Asset currencySymbol merkleRoot) value /= BigNum.fromInt 0

-- | `findPreviousMerkleRootTokenUtxo maybeLastMerkleRoot smrm` returns `Nothing` in
-- | the case that `maybeLastMerkleRoot` is `Nothing`, and `Just` the result of
-- | `findMerkleRootTokenUtxo lastMerkleRoot smrm` provided that `Just lastMerkleRoot = maybeLastMerkleRoot`
-- | and there are no other errors.
-- | Note: the `Maybe` return type does NOT denote the absense or existence of
-- | finding the utxo... rather it reflects the `Maybe` in the last merkle root
-- | of whether it exists or not.
findPreviousMerkleRootTokenUtxo ∷
  ∀ r.
  Maybe RootHash →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findPreviousMerkleRootTokenUtxo maybeLastMerkleRoot sp =
  case maybeLastMerkleRoot of
    Nothing → pure Nothing
    Just lastMerkleRoot' → do
      lastMerkleRootTokenName ← Run.note
        (InvalidData "Invalid lastMerkleRoot token name")
        (mkAssetName $ MerkleTree.unRootHash lastMerkleRoot')
      lkup ← findMerkleRootTokenUtxo lastMerkleRootTokenName sp
      lkup' ←
        Run.note
          (InvalidData "failed to find last merkle root")
          lkup
      pure $ Just lkup'

-- | `serialiseMrimHash` is an alias for (ignoring the `Maybe`)
-- | ```purescript
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
serialiseMrimHash ∷ MerkleRootInsertionMessage → Maybe EcdsaSecp256k1Message
serialiseMrimHash message = unsafePartial
  ( Utils.Crypto.ecdsaSecp256k1Message
  $ Utils.Crypto.blake2b256Hash
  $ unwrap
  $ encodeCbor
  $ toData message
  )
