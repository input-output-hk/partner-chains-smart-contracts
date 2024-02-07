module TrustlessSidechain.Checkpoint.Utils
  ( checkpointValidator
  , initCheckpointMintTn
  , serialiseCheckpointMessage
  , findCheckpointUtxo
  , checkpointCurrencyInfo
  , checkpointAssetClass
  ) where

import Contract.Prelude

import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.PlutusData (serializeData, toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (Validator)
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointMessage
  , CheckpointParameter
  , InitCheckpointMint(InitCheckpointMint)
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (AssetClass, CurrencyInfo, assetClass)
import TrustlessSidechain.Utils.Address (getCurrencyInfo, toAddress)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CheckpointPolicy, CheckpointValidator)
  )
import TrustlessSidechain.Versioning.Types (VersionOracleConfig)
import TrustlessSidechain.Versioning.Utils as Versioning

-- | Wrapper around `checkpointPolicy` that accepts `SidechainParams`.
checkpointCurrencyInfo ∷
  SidechainParams →
  Contract CurrencyInfo
checkpointCurrencyInfo (SidechainParams sp) =
  let
    icm = InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
  in
    getCurrencyInfo CheckpointPolicy [ toData icm ]

checkpointAssetClass ∷
  SidechainParams →
  Contract AssetClass
checkpointAssetClass (SidechainParams sp) = do
  let ichm = InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
  { currencySymbol } ← getCurrencyInfo CheckpointPolicy [ toData ichm ]
  pure $ assetClass currencySymbol initCheckpointMintTn

checkpointValidator ∷
  CheckpointParameter → VersionOracleConfig → Contract Validator
checkpointValidator cp voc =
  mkValidatorWithParams CheckpointValidator [ toData cp, toData voc ]

-- | `initCheckpointMintTn` is the token name of the NFT which identifies
-- | the utxo which contains the checkpoint. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
initCheckpointMintTn ∷ Value.TokenName
initCheckpointMintTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

-- | `serialiseCheckpointMessage` is an alias for
-- | ```
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
-- | The result of this function is what is signed by the committee members.
serialiseCheckpointMessage ∷ CheckpointMessage → Maybe EcdsaSecp256k1Message
serialiseCheckpointMessage = Utils.Crypto.ecdsaSecp256k1Message
  <<< Hashing.blake2b256Hash
  <<< cborBytesToByteArray
  <<< serializeData

findCheckpointUtxo ∷
  CheckpointParameter →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findCheckpointUtxo checkpointParameter = do
  versionOracleConfig ← Versioning.getVersionOracleConfig $
    (unwrap checkpointParameter).sidechainParams
  validator ← checkpointValidator checkpointParameter versionOracleConfig
  validatorAddress ← toAddress (Scripts.validatorHash validator)

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this checkpoint nft.
    Value.valueOf value (fst (unwrap checkpointParameter).checkpointAssetClass)
      initCheckpointMintTn
      /= zero
