module TrustlessSidechain.Checkpoint.Utils
  ( checkpointValidator
  , checkpointNftTn
  , mintOneCheckpointInitToken
  , burnOneCheckpointInitToken
  , serialiseCheckpointMessage
  , findCheckpointUtxo
  , checkpointCurrencyInfo
  , checkpointAssetClass
  , checkpointInitTokenName
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointMessage
  , CheckpointParameter
  )
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  , mintOneInitToken
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo, toAddress)
import TrustlessSidechain.Utils.Asset (emptyAssetName, unsafeMkAssetName)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CheckpointPolicy, CheckpointValidator)
  )
import TrustlessSidechain.Versioning.Types (VersionOracleConfig)
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | A name for the checkpoint initialization token.  Must be unique among
-- | initialization tokens.
checkpointInitTokenName ∷ AssetName
checkpointInitTokenName = unsafeMkAssetName "Checkpoint InitToken"

-- | Build lookups and constraints to mint checkpoint initialization token.
mintOneCheckpointInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mintOneCheckpointInitToken sp =
  mintOneInitToken sp checkpointInitTokenName

-- | Build lookups and constraints to burn checkpoint initialization token.
burnOneCheckpointInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
burnOneCheckpointInitToken sp =
  burnOneInitToken sp checkpointInitTokenName

-- | Wrapper around `checkpointPolicy` that accepts `SidechainParams`.
checkpointCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
checkpointCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: checkpointInitTokenName
      }
  getCurrencyInfo CheckpointPolicy [ toData itac ]

checkpointAssetClass ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) AssetClass
checkpointAssetClass sp = do
  { currencySymbol } ← checkpointCurrencyInfo sp
  pure $ AssetClass currencySymbol checkpointNftTn

checkpointValidator ∷
  ∀ r.
  CheckpointParameter →
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) PlutusScript
checkpointValidator cp voc =
  mkValidatorWithParams CheckpointValidator [ toData cp, toData voc ]

-- | `checkpointNftTn` is the token name of the NFT which identifies
-- | the utxo which contains the checkpoint. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
checkpointNftTn ∷ AssetName
checkpointNftTn = emptyAssetName

-- | `serialiseCheckpointMessage` is an alias for
-- | ```
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
-- | The result of this function is what is signed by the committee members.
serialiseCheckpointMessage ∷ CheckpointMessage → Maybe EcdsaSecp256k1Message
serialiseCheckpointMessage message = unsafePartial
  ( Utils.Crypto.ecdsaSecp256k1Message
      $ Utils.Crypto.blake2b256Hash
      $ unwrap
      $ encodeCbor
      $ toData
      $ message
  )

findCheckpointUtxo ∷
  ∀ r.
  CheckpointParameter →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findCheckpointUtxo checkpointParameter = do
  versionOracleConfig ← Versioning.getVersionOracleConfig $
    (unwrap checkpointParameter).sidechainParams
  validator ← checkpointValidator checkpointParameter versionOracleConfig
  validatorAddress ← toAddress (PlutusScript.hash validator)

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    let
      asset = case (unwrap checkpointParameter).checkpointAssetClass of
        AssetClass currencySymbol tokenName →
          Asset currencySymbol tokenName

    -- Note: there should either be 0 or 1 tokens of this checkpoint nft.
    in
      Value.valueOf asset value
        /= (BigNum.fromInt 0)
