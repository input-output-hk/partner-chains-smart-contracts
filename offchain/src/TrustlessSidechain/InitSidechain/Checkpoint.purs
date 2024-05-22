module TrustlessSidechain.InitSidechain.Checkpoint
  ( initCheckpoint
  ) where

import Contract.Prelude hiding (note)

import Contract.PlutusData (Datum(..))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array ((:))
import Data.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Checkpoint (CheckpointDatum(..), checkpointNftTn)
import TrustlessSidechain.Checkpoint.Types as Checkpoint.Types
import TrustlessSidechain.Checkpoint.Utils as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Init (init, insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning (getCheckpointPoliciesAndValidators)
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig)
import Type.Row (type (+))

-- | Idempotently initialise the checkpointing mechanism, consuming the minted
-- | checkpoint init token
initCheckpoint ∷
  ∀ r.
  SidechainParams →
  ByteArray →
  ATMSKinds →
  Int →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        }
    )
initCheckpoint sidechainParams initGenesisHash initATMSKind version = do

  logDebug' "Attempting to initialize Checkpoint versioning scripts"
  scriptsInitTxId ← insertScriptsIdempotent getCheckpointPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  if not $ null scriptsInitTxId then do
    logInfo' "Attempting to mint Checkpoint NFT from the init token"
    checkpointInitTxId ← init
      ( \op → balanceSignAndSubmit op
          <=< initCheckpointLookupsAndConstraints initGenesisHash
      )
      "Checkpoint init"
      Checkpoint.checkpointInitTokenName
      sidechainParams
    pure
      ( Just
          { initTransactionIds: checkpointInitTxId : scriptsInitTxId
          }
      )
  else pure Nothing

-- | `initCheckpointLookupsAndConstraints` creates lookups and constraints to
-- | mint and pay the NFT which uniquely identifies the utxo that holds the
-- | checkpoint.
initCheckpointLookupsAndConstraints ∷
  ∀ (r ∷ Row Type) r'.
  ByteArray →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r')
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCheckpointLookupsAndConstraints initGenesisHash sidechainParams = do

  -- Get checkpoint / associated values
  -----------------------------------
  -- Build lookups and constraints to burn checkpoint init token
  burnCheckpointInitToken ←
    Checkpoint.burnOneCheckpointInitToken sidechainParams

  -- Build lookups and constraints to mint checkpoint NFT
  checkpointNft ← Checkpoint.checkpointCurrencyInfo sidechainParams

  let
    checkpointNftValue =
      Value.singleton checkpointNft.currencySymbol checkpointNftTn one

    mintCheckpointNft =
      { lookups: Lookups.mintingPolicy checkpointNft.mintingPolicy
      , constraints: Constraints.mustMintValue checkpointNftValue
      }

  -- Construct initial checkpoint datum and pay it together with checkpoint NFT
  -- to checkpoint validator
  checkpointAssetClass ← Checkpoint.checkpointAssetClass sidechainParams

  let
    checkpointParameter = Checkpoint.Types.CheckpointParameter
      { sidechainParams
      , checkpointAssetClass
      }
    checkpointDatum = Datum
      $ PlutusData.toData
      $ CheckpointDatum
          { blockHash: initGenesisHash
          , blockNumber: BigInt.fromInt 0
          }

  versionOracleConfig ← getVersionOracleConfig sidechainParams
  checkpointValidator ← Checkpoint.checkpointValidator checkpointParameter
    versionOracleConfig

  let
    checkpointValidatorHash = validatorHash checkpointValidator

    payNftToCheckpointValidator =
      { lookups: Lookups.validator checkpointValidator
      , constraints: Constraints.mustPayToScript checkpointValidatorHash
          checkpointDatum
          DatumInline
          checkpointNftValue
      }

  pure $ burnCheckpointInitToken <> mintCheckpointNft <>
    payNftToCheckpointValidator
