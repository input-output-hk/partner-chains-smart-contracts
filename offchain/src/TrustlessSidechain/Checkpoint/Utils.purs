module TrustlessSidechain.Checkpoint.Utils
  ( checkpointValidator
  , initCheckpointMintTn
  , checkpointAssetClass
  , serialiseCheckpointMessage
  , findCheckpointUtxo
  , getCheckpointPolicy
  , getCheckpointAssetClass
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (serializeData, toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( Validator
  )
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointMessage
  , CheckpointParameter
  , InitCheckpointMint(InitCheckpointMint)
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (AssetClass, CurrencyInfo, assetClass)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( CheckpointPolicy
      , CheckpointValidator
      )
  )

checkpointPolicy ∷ InitCheckpointMint → Contract CurrencyInfo
checkpointPolicy icm =
  getCurrencyInfo CheckpointPolicy [ toData icm ]

checkpointValidator ∷ CheckpointParameter → Contract Validator
checkpointValidator cp =
  mkValidatorWithParams CheckpointValidator [ toData cp ]

-- | `initCheckpointMintTn` is the token name of the NFT which identifies
-- | the utxo which contains the checkpoint. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
initCheckpointMintTn ∷ Value.TokenName
initCheckpointMintTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

checkpointAssetClass ∷ InitCheckpointMint → Contract AssetClass
checkpointAssetClass ichm = do
  { currencySymbol } ← checkpointPolicy ichm
  pure $ assetClass currencySymbol initCheckpointMintTn

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
  netId ← Address.getNetworkId
  validator ← checkpointValidator checkpointParameter
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← Monad.liftContractM
    "error 'findCheckpointUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this checkpoint nft.
    Value.valueOf value (fst (unwrap checkpointParameter).checkpointAssetClass)
      initCheckpointMintTn
      /= zero

-- | Wrapper around `checkpointPolicy` that accepts `SidechainParams`.
getCheckpointPolicy ∷
  SidechainParams →
  Contract CurrencyInfo
getCheckpointPolicy (SidechainParams sp) = do
  checkpointPolicy $ InitCheckpointMint { icTxOutRef: sp.genesisUtxo }

-- | Wrapper around `checkpointAssetClass` that accepts `SidechainParams`.
getCheckpointAssetClass ∷
  SidechainParams →
  Contract AssetClass
getCheckpointAssetClass (SidechainParams sp) = do
  checkpointAssetClass $ InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
