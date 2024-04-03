-- | `InitSidechain` implements the endpoint for intializing the sidechain.
-- | There's two ways to initialize the sidechain.
-- |
-- |      1. In a single transaction with `initSidechain` (the old way)
-- |
-- |      2. In two transactions. This is the new way which is to accomodate the
-- |      time difference between sidechain creation, and the first committee setup:
-- |
-- |          - Start with `initSidechainTokens` (returns the sidechain
-- |          parameters), which will mint the genesis token for the committee hash
-- |          (and set up other required tokens for the distributed set)
-- |
-- |          - Then, call `paySidechainTokens` which will pay the genesis
-- |          token for the committee hash (assuming you have it in your wallet)
-- |          to the required committee hash validator (with the initial committee).
module TrustlessSidechain.InitSidechain
  ( InitSidechainParams'
  , InitSidechainParams(..)
  , InitTokensParams
  , initSidechain
  , toSidechainParams
  ) where

import Contract.Prelude

import Contract.PlutusData (PlutusData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Data.BigInt (BigInt)
import Data.Maybe (isJust)
import Data.Monoid (mempty)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.Checkpoint
  ( initCheckpointLookupsAndConstraints
  )
import TrustlessSidechain.InitSidechain.CommitteeSelection
  ( initCommitteeHashLookupsAndConstraints
  )
import TrustlessSidechain.InitSidechain.FUEL
  ( initFuelAndDsLookupsAndConstraints
  )
import TrustlessSidechain.InitSidechain.TokensMint (mintAllTokens)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning as Versioning
import Type.Row (type (+))

-- | Parameters for the first step (see description above) of the initialisation procedure
-- | Using a open row type, to allow composing the two contracts
type InitTokensParams r =
  { initChainId ∷ BigInt
  , initGenesisHash ∷ ByteArray
  , -- `initUtxo` is a `TransactionInput` used for creating `AssetClass`s for the
    -- internal function of the side chain
    initUtxo ∷ TransactionInput
  , initThresholdNumerator ∷ BigInt
  , initThresholdDenominator ∷ BigInt
  , initCandidatePermissionTokenMintInfo ∷ Maybe CandidatePermissionTokenMintInfo
  , initATMSKind ∷ ATMSKinds
  , initGovernanceAuthority ∷ Governance.GovernanceAuthority
  | r
  }

-- | Parameters to initialize a sidechain (purely an offchain type)
newtype InitSidechainParams = InitSidechainParams InitSidechainParams'

instance Show InitSidechainParams where
  show = genericShow

derive instance Generic InitSidechainParams _

derive instance Newtype InitSidechainParams _

-- | Parameters for the second step (see description above) of the
-- | initialisation procedure.
-- | In particular, note that this augments `InitSidechainParams` with an
-- | initial committee, and the initial committee's epoch
type InitSidechainParams' =
  InitTokensParams
    ( -- `initAggregatedCommittee` is the initial aggregated committee of the
      -- sidechain
      initAggregatedCommittee ∷ PlutusData
    , -- `initSidechainEpoch` is the initial sidechain epoch of the first committee
      initSidechainEpoch ∷ BigInt
    )

-- | `toSidechainParams` creates a `SidechainParams` from an
-- | `InitSidechainParams` the canonical way.
toSidechainParams ∷ ∀ (r ∷ Row Type). InitTokensParams r → SidechainParams
toSidechainParams isp = SidechainParams
  { chainId: isp.initChainId
  , genesisUtxo: isp.initUtxo
  , thresholdNumerator: isp.initThresholdNumerator
  , thresholdDenominator: isp.initThresholdDenominator
  , governanceAuthority: isp.initGovernanceAuthority
  }

-- | `initCandidatePermissionTokenLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the candidiate permission tokens (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the candidiate permission tokens if
-- |     `initCandidatePermissionTokenMintInfo` is `Just` (otherwise returns empty)
initCandidatePermissionTokenLookupsAndConstraints ∷
  ∀ (r ∷ Row Type) r'.
  InitTokensParams r →
  Run (EXCEPT OffchainError + r')
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCandidatePermissionTokenLookupsAndConstraints isp =
  case isp.initCandidatePermissionTokenMintInfo of
    Nothing → pure mempty
    Just { candidatePermissionTokenAmount: amount } → do
      let sidechainParams = toSidechainParams isp

      CandidatePermissionToken.candidatePermissionTokenLookupsAndConstraints
        sidechainParams
        amount

-- | `initSidechain` creates the `SidechainParams` and executes
-- | `initSidechainTokens` and `paySidechainTokens` in one transaction. Briefly,
-- | this will do the following:
-- |
-- |     - Mints the committee hash NFT
-- |
-- |     - Pays the committee hash NFT to the update committee hash validator
-- |
-- |     - Mints the checkpoint NFT
-- |
-- |     - Pays the checkpoint NFT to the checkpoint hash validator
-- |
-- |     - Mints various tokens for the distributed set (and pay to the required
-- |       validators)
-- |
-- |     - Mints and pays versioning tokens to versioning script
-- |
-- |     - Optionally, mints candidate permission tokens
-- |
-- | For details, see `initSidechainTokens` and `paySidechainTokens`.
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

  -- Grabbing all contraints for initialising the committee hash
  -- and distributed set.
  -- Note: this uses the monoid instance of functions to monoids to run
  -- all functions to get the desired lookups and contraints.
  ----------------------------------------
  -- TODO: lookups and constraints should be constructed depending on the
  -- version argument.  See Issue #10
  { transactionId: txId } ← mintAllTokens sidechainParams isp.initATMSKind
    version

  -- Mint and pay versioning tokens to versioning script.
  ----------------------------------------
  versionedScriptsTxIds ← Versioning.initializeVersion
    { atmsKind: isp.initATMSKind, sidechainParams }
    version

  checkpointInitTxId ←
    initCheckpointLookupsAndConstraints isp.initGenesisHash sidechainParams
      >>= balanceSignAndSubmit "Checkpoint init"
  dsInitTxId ← initFuelAndDsLookupsAndConstraints sidechainParams version
    >>= balanceSignAndSubmit "Distributed set init"
  permissionTokensInitTxId ←
    initCandidatePermissionTokenLookupsAndConstraints isp
      >>= balanceSignAndSubmit "Candidate permission tokens init"
  committeeInitTxId ←
    initCommitteeHashLookupsAndConstraints isp.initSidechainEpoch
      isp.initAggregatedCommittee
      sidechainParams
      >>= balanceSignAndSubmit "Committee init"

  -- Grabbing the required sidechain addresses of particular validators /
  -- minting policies as in issue #224
  -----------------------------------------
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
    , initTransactionIds: versionedScriptsTxIds <>
        [ checkpointInitTxId
        , dsInitTxId
        , permissionTokensInitTxId
        , committeeInitTxId
        ]
    , sidechainParams
    , sidechainAddresses
    }
