module TrustlessSidechain.Checkpoint.Utils
  ( checkpointPolicy
  , checkpointValidator
  , initCheckpointMintTn
  , checkpointAssetClass
  , aggregateKeys
  , serialiseCheckpointMessage
  , normalizeSignatures

  ) where

import Contract.Prelude

import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Checkpoint.Types
  ( InitCheckpointMint
  , CheckpointParameter
  , CheckpointMessage
  , CheckpointEndpointParam(CheckpointEndpointParam)
  )
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.Types (AssetClass, assetClass)
import TrustlessSidechain.Utils.Crypto (SidechainMessage, SidechainPublicKey)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SerialiseData as Utils.SerialiseData

checkpointPolicy ∷ InitCheckpointMint → Contract () MintingPolicy
checkpointPolicy sp = do
  let
    script = decodeTextEnvelope RawScripts.rawCheckpointPolicy
      >>= plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
    [ PlutusData.toData sp ]
  pure $ PlutusMintingPolicy applied

checkpointValidator ∷ CheckpointParameter → Contract () Validator
checkpointValidator sp = do
  let
    script = decodeTextEnvelope RawScripts.rawCheckpointValidator
      >>= plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
    [ PlutusData.toData sp ]
  pure $ Validator applied

normalizeSignatures ∷ CheckpointEndpointParam → CheckpointEndpointParam
normalizeSignatures (CheckpointEndpointParam p) = CheckpointEndpointParam
  p
    { committeeSignatures = Utils.Crypto.normalizeCommitteePubKeysAndSignatures
        p.committeeSignatures
    }

-- | `initCheckpointMintTn` is the token name of the NFT which identifies
-- | the utxo which contains the checkpoint. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
initCheckpointMintTn ∷ Value.TokenName
initCheckpointMintTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

{-# INLINEABLE committeeHashAssetClass #-}
checkpointAssetClass ∷ InitCheckpointMint → Contract () AssetClass
checkpointAssetClass ichm = do
  cp ← checkpointPolicy ichm
  curSym ← Monad.liftContractM "Couldn't get checkpoint currency symbol"
    (Value.scriptCurrencySymbol cp)

  pure $ assetClass curSym initCheckpointMintTn

-- | TODO: refactor
aggregateKeys ∷ Array SidechainPublicKey → ByteArray
aggregateKeys = Hashing.blake2b256Hash <<< foldMap
  Utils.Crypto.getSidechainPublicKeyByteArray

-- | `serialiseCheckpointMessage` is an alias for (ignoring the `Maybe`)
-- | ```
-- | Contract.Hashing.blake2b256Hash <<< Utils.SerialiseData.serialiseToData
-- | ```
-- | The result of this function is what is signed by the committee members.
serialiseCheckpointMessage ∷ CheckpointMessage → Maybe SidechainMessage
serialiseCheckpointMessage = Utils.Crypto.sidechainMessage <=<
  ((Hashing.blake2b256Hash <$> _) <<< Utils.SerialiseData.serialiseToData)
