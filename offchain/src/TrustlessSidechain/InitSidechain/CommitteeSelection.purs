module TrustlessSidechain.InitSidechain.CommitteeSelection where

import Contract.Prelude

import Contract.PlutusData (Datum(Datum), PlutusData)
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array ((:))
import Data.BigInt (BigInt)
import Data.Maybe (isJust)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain.Init (init, insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash (UpdateCommitteeDatum(..))
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getCommitteeSelectionPoliciesAndValidators
  )
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig)
import Type.Row (type (+))

-- | Idempotently initialise the committee selection mechanism, consuming the
-- | committee oracle init token
initCommitteeSelection ∷
  ∀ r.
  SidechainParams →
  Maybe CandidatePermissionTokenMintInfo →
  BigInt →
  PlutusData →
  ATMSKinds →
  Int →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        , sidechainParams ∷ SidechainParams
        , sidechainAddresses ∷ SidechainAddresses
        }
    )
initCommitteeSelection
  sidechainParams
  initCandidatePermissionTokenMintInfo
  initSidechainEpoch
  initAggregatedCommittee
  initATMSKind
  version = do
  let
    run = init
      ( \op → balanceSignAndSubmit op
          <=< initCommitteeHashLookupsAndConstraints
            initSidechainEpoch
            initAggregatedCommittee
      )
      "Committee init"
      CommitteeOraclePolicy.committeeOracleInitTokenName

  scriptsInitTxId ← insertScriptsIdempotent
    (getCommitteeSelectionPoliciesAndValidators initATMSKind)
    sidechainParams
    initATMSKind
    version

  if not $ null scriptsInitTxId then do
    sidechainAddresses ←
      GetSidechainAddresses.getSidechainAddresses $
        SidechainAddressesEndpointParams
          { sidechainParams
          , atmsKind: initATMSKind
          , usePermissionToken: isJust initCandidatePermissionTokenMintInfo
          , version
          }
    committeeSelectionInitTxId ← run sidechainParams
    pure
      ( Just
          { initTransactionIds: committeeSelectionInitTxId : scriptsInitTxId
          , sidechainParams
          , sidechainAddresses
          }
      )
  else pure Nothing

-- | `initCommitteeHashLookupsAndConstraints` creates lookups and constraints
-- | to pay the NFT (which uniquely identifies the committee hash utxo) to the
-- | validator script for the update committee hash.
initCommitteeHashLookupsAndConstraints ∷
  ∀ r.
  BigInt →
  PlutusData →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashLookupsAndConstraints
  initSidechainEpoch
  initAggregatedCommittee
  sidechainParams =
  do

    -- Build lookups and constraints to burn committee oracle init token
    burnCommitteeOracleInitToken ←
      CommitteeOraclePolicy.burnOneCommitteeOracleInitToken sidechainParams

    -- Build lookups and constraints to mint committee oracle NFT
    -----------------------------------
    committeeNft ←
      CommitteeOraclePolicy.committeeOracleCurrencyInfo sidechainParams

    let
      committeeNftValue =
        Value.singleton
          committeeNft.currencySymbol
          CommitteeOraclePolicy.committeeOracleTn
          one

      mintCommitteeNft =
        { lookups: Lookups.mintingPolicy committeeNft.mintingPolicy
        , constraints: Constraints.mustMintValue committeeNftValue
        }

    -- Setting up the update committee hash validator
    -----------------------------------
    let
      aggregatedKeys = initAggregatedCommittee
      committeeHashDatum = Datum
        $ PlutusData.toData
        $ UpdateCommitteeDatum
            { aggregatePubKeys: aggregatedKeys
            , sidechainEpoch: initSidechainEpoch
            }

    versionOracleConfig ← getVersionOracleConfig sidechainParams

    committeeHashValidator ← UpdateCommitteeHash.updateCommitteeHashValidator
      sidechainParams
      versionOracleConfig

    let
      committeeHashValidatorHash = validatorHash committeeHashValidator

    -- Building the transaction
    -----------------------------------
    let
      lookups ∷ ScriptLookups Void
      lookups =
        Lookups.validator committeeHashValidator

      constraints ∷ TxConstraints Void Void
      constraints = Constraints.mustPayToScript committeeHashValidatorHash
        committeeHashDatum
        DatumInline
        committeeNftValue

    pure $ burnCommitteeOracleInitToken <> mintCommitteeNft <>
      { constraints, lookups }
