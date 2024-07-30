module TrustlessSidechain.InitSidechain.Checkpoint
  ( initCheckpoint
  ) where

import Contract.Prelude hiding (note)

import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import JS.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Checkpoint
  ( CheckpointDatum(CheckpointDatum)
  , checkpointNftTn
  )
import TrustlessSidechain.Checkpoint.Types as Checkpoint.Types
import TrustlessSidechain.Checkpoint.Utils as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Init
  ( getScriptsToInsert
  , init
  , insertScriptsIdempotent
  )
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
    { scriptsInitTxIds ∷ Array TransactionHash
    , tokensInitTxId ∷ Maybe TransactionHash
    }
initCheckpoint sidechainParams initGenesisHash initATMSKind version = do

  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize Checkpoint versioning scripts"
  scriptsInitTxIds ← insertScriptsIdempotent getCheckpointPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  -- Before proceeding with minting tokens we need to make sure that all the
  -- required scripts have been succesfully versioned.  We begin by acquiring
  -- the scripts that are actually in the versioning system.
  checkpointScripts ← getCheckpointPoliciesAndValidators sidechainParams version

  { versionedPolicies, versionedValidators } ←
    getScriptsToInsert sidechainParams initATMSKind checkpointScripts version

  -- If all versioned scripts have been inserted we can proceed with minting the
  -- NFTs and other tokens.
  if null versionedPolicies && null versionedValidators then do
    logInfo' "Attempting to mint Checkpoint NFT from the init token"
    tokensInitTxId ← init
      ( \op → balanceSignAndSubmit op
          <=< initCheckpointLookupsAndConstraints initGenesisHash
      )
      "Checkpoint init"
      Checkpoint.checkpointInitTokenName
      sidechainParams
    pure { scriptsInitTxIds, tokensInitTxId }

  else
    pure { scriptsInitTxIds, tokensInitTxId: Nothing }

-- | `initCheckpointLookupsAndConstraints` creates lookups and constraints to
-- | mint and pay the NFT which uniquely identifies the utxo that holds the
-- | checkpoint.
initCheckpointLookupsAndConstraints ∷
  ∀ (r ∷ Row Type) r'.
  ByteArray →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r')
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
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
      Value.singleton checkpointNft.currencySymbol checkpointNftTn
        (BigNum.fromInt 1)
    checkpointNftMint =
      Mint.singleton checkpointNft.currencySymbol checkpointNftTn (Int.fromInt 1)

    mintCheckpointNft =
      { lookups: Lookups.plutusMintingPolicy checkpointNft.mintingPolicy
      , constraints: Constraints.mustMintValue checkpointNftMint
      }

  -- Construct initial checkpoint datum and pay it together with checkpoint NFT
  -- to checkpoint validator
  checkpointAssetClass ← Checkpoint.checkpointAssetClass sidechainParams

  let
    checkpointParameter = Checkpoint.Types.CheckpointParameter
      { sidechainParams
      , checkpointAssetClass
      }
    checkpointDatum = PlutusData.toData
      $ CheckpointDatum
          { blockHash: initGenesisHash
          , blockNumber: BigInt.fromInt 0
          }

  versionOracleConfig ← getVersionOracleConfig sidechainParams
  checkpointValidator ← Checkpoint.checkpointValidator checkpointParameter
    versionOracleConfig

  let
    checkpointValidatorHash = PlutusScript.hash checkpointValidator

    payNftToCheckpointValidator =
      { lookups: Lookups.validator checkpointValidator
      , constraints: Constraints.mustPayToScript checkpointValidatorHash
          checkpointDatum
          DatumInline
          checkpointNftValue
      }

  pure $ burnCheckpointInitToken <> mintCheckpointNft <>
    payNftToCheckpointValidator
