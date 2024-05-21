-- | `InitSidechain` implements the endpoint for intializing the sidechain with
-- | a set of default features.

module TrustlessSidechain.InitSidechain
  ( InitSidechainParams(..)
  , initSidechain
  , toSidechainParams
  ) where

import Contract.Prelude

import Contract.PlutusData (PlutusData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionHash, TransactionInput)
import Data.Array (concat)
import Data.BigInt (BigInt)
import Data.Maybe (isJust)
import Run (Run)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  )
import TrustlessSidechain.InitSidechain.Checkpoint
  ( initCheckpoint
  )
import TrustlessSidechain.InitSidechain.FUEL
  ( initFuel
  )
import TrustlessSidechain.InitSidechain.TokensMint (mintAllTokens)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Maybe
  ( maybeToArray
  )
import Type.Row (type (+))

-- | Parameters to initialize a sidechain (purely an offchain type)
newtype InitSidechainParams = InitSidechainParams
  { initChainId ∷ BigInt
  , initGenesisHash ∷ ByteArray
  , -- `initUtxo` is a `TransactionInput` used for creating `AssetClass`s for the
    -- internal function of the side chain
    initUtxo ∷ TransactionInput
  , initThresholdNumerator ∷ BigInt
  , initThresholdDenominator ∷ BigInt
  , initCandidatePermissionTokenMintInfo ∷ Maybe BigInt
  , initATMSKind ∷ ATMSKinds
  , initGovernanceAuthority ∷ Governance.GovernanceAuthority
  , initAggregatedCommittee ∷ PlutusData
  , -- `initSidechainEpoch` is the initial sidechain epoch of the first committee
    initSidechainEpoch ∷ BigInt
  }

instance Show InitSidechainParams where
  show = genericShow

derive instance Generic InitSidechainParams _

derive instance Newtype InitSidechainParams _

-- | `toSidechainParams` creates a `SidechainParams` from an
-- `InitSidechainParams` the canonical way.
toSidechainParams ∷
  ∀ (r ∷ Row Type).
  { initChainId ∷ BigInt
  , initUtxo ∷ TransactionInput
  , initThresholdNumerator ∷ BigInt
  , initThresholdDenominator ∷ BigInt
  , initGovernanceAuthority ∷ Governance.GovernanceAuthority
  | r
  } →
  SidechainParams
toSidechainParams isp = SidechainParams
  { chainId: isp.initChainId
  , genesisUtxo: isp.initUtxo
  , thresholdNumerator: isp.initThresholdNumerator
  , thresholdDenominator: isp.initThresholdDenominator
  , governanceAuthority: isp.initGovernanceAuthority
  }

-- | `InitSidechain` implements the endpoint for intializing the sidechain.
-- Sidechain initialization consists of:
--
--   1. Burning genesis UTxO and using it to mint multiple initialization
--      tokens.  See SIP-06 for a detailed explanation.
--
--   2. Running all individual initialization commands, currently consistint of:
--
--      a) Fuel initialization.  This includes initializing the distributed set,
--         committee selection and the merkle tree machinery.
--
--      b) Checkpointing initialization
--
--      c) Candidate permission token initialization
--
-- See documentation for `initFuel`, `initCheckpoint`, and
-- `initCandidatePermissionToken` for details.
--
-- Sidechain initialization is idempotent, i.e. in case of failure it should be
-- possible to simply re-run the command and have it finish the initialization
-- step that have not yet been conducted.
initSidechain ∷
  ∀ r.
  InitSidechainParams →
  Int →
  Run (APP + r)
    { transactionId ∷ TransactionHash
    , initTransactionIds ∷ Array TransactionHash
    , sidechainParams ∷ SidechainParams
    , sidechainAddresses ∷ SidechainAddresses
    }
initSidechain (InitSidechainParams isp) version = do
  let sidechainParams = toSidechainParams isp

  -- Burn genesis UTxO and mint initialization tokens
  { transactionId: txId } ← mintAllTokens sidechainParams isp.initATMSKind
    version

  -- Initialize FUEL, including distributed set, merkle root, and committee
  -- selection.
  fuelInitTx ←
    initFuel sidechainParams
      isp.initSidechainEpoch
      isp.initAggregatedCommittee
      isp.initATMSKind
      version

  -- Initialize checkpointing.
  checkpointInitTx ←
    initCheckpoint sidechainParams
      isp.initGenesisHash
      isp.initATMSKind
      version

  -- Initialize candidate permission tokens.
  permissionTokensInitTx ←
    initCandidatePermissionToken sidechainParams
      isp.initCandidatePermissionTokenMintInfo

  -- Grabbing the required sidechain addresses of particular validators /
  -- minting policies
  sidechainAddresses ←
    GetSidechainAddresses.getSidechainAddresses $
      SidechainAddressesEndpointParams
        { sidechainParams
        , atmsKind: isp.initATMSKind
        , usePermissionToken: isJust isp.initCandidatePermissionTokenMintInfo
        , version
        }
  pure
    { transactionId: txId
    , initTransactionIds: concat
        $ (_.initTransactionIds <$> maybeToArray fuelInitTx)
        <> (_.initTransactionIds <$> maybeToArray permissionTokensInitTx)
        <>
          (_.initTransactionIds <$> maybeToArray checkpointInitTx)
    , sidechainParams
    , sidechainAddresses
    }
