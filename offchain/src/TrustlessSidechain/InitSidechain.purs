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
  ( initSidechain
  , initSpendGenesisUtxo
  , InitSidechainParams(InitSidechainParams)
  , InitSidechainParams'
  , InitTokensParams
  , toSidechainParams
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedM)
import Contract.Monad as Monad
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid (mempty)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint
  ( CheckpointDatum(CheckpointDatum)
  , checkpointNftTn
  )
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.Checkpoint.Types as Checkpoint.Types
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , DsConfDatum(DsConfDatum)
  , DsDatum(DsDatum)
  , DsKeyMint(DsKeyMint)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Error
  ( OffchainError(ConversionError, NoGenesisUTxO)
  )
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning as Versioning
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( FUELMintingPolicy
      , DsKeyPolicy
      )
  )
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig)

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
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
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

-- | `initCheckpointLookupsAndConstraints` creates lookups and constraints to
-- | mint and pay the NFT which uniquely identifies the utxo that holds the
-- | checkpoint.
initCheckpointLookupsAndConstraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCheckpointLookupsAndConstraints inp = do

  -- Get checkpoint / associated values
  -----------------------------------
  let
    sidechainParams = toSidechainParams inp

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
          { blockHash: inp.initGenesisHash
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

-- | `initCommitteeHashLookupsAndConstraints` creates lookups and constraints
-- | to pay the NFT (which uniquely identifies the committee hash utxo) to the
-- | validator script for the update committee hash.
initCommitteeHashLookupsAndConstraints ∷
  InitSidechainParams' →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashLookupsAndConstraints isp = do
  -- Sidechain parameters
  -----------------------------------
  let sidechainParams = toSidechainParams isp

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
    aggregatedKeys = isp.initAggregatedCommittee
    committeeHashDatum = Datum
      $ PlutusData.toData
      $ UpdateCommitteeDatum
          { aggregatePubKeys: aggregatedKeys
          , sidechainEpoch: isp.initSidechainEpoch
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

-- | `initDistributedSetLookupsAndContraints` creates the lookups and
-- | constraints required when initalizing the distributed set (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the necessary tokens to run the distributed set i.e., it mints a
-- |      token to hold keys in the distributed set, and a distinguished NFT to
-- |      provide the configuration (the necessary state so that FUEL is minted
-- |      iff the corresponding state is inserted in the distributed set.) of the
-- |      distributed set.
-- |
-- |      - Pays the aforementioned tokens to their required validators.
-- |
-- | Note: this does NOT include a lookup or constraint to spend the distinguished
-- | `initUtxo` in the `InitSidechainParams`, and this MUST be provided
-- | seperately.
initDistributedSetLookupsAndConstraints ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initDistributedSetLookupsAndConstraints sidechainParams = do
  -- Build lookups and constraints to burn distributed set init token
  burnDsInitToken ←
    DistributedSet.burnOneDsInitToken sidechainParams

  -- Initializing the distributed set
  -----------------------------------
  -- Configuration policy of the distributed set
  { currencySymbol: dsConfCurrencySymbol, mintingPolicy: dsConfPolicy } ←
    DistributedSet.dsConfCurrencyInfo sidechainParams

  -- Validator for insertion of the distributed set / the associated datum and
  -- tokens that should be paid to this validator.
  let ds = Ds dsConfCurrencySymbol
  insertValidator ← DistributedSet.insertValidator ds
  let
    insertValidatorHash = Scripts.validatorHash insertValidator
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConfCurrencySymbol
      }

  dsKeyPolicy ← DistributedSet.dsKeyPolicy dskm
  dsKeyCurrencySymbol ← getCurrencySymbol DsKeyPolicy dsKeyPolicy
  dsKeyPolicyTokenName ←
    Monad.liftContractM
      ( show $ ConversionError
          "Failed to convert 'DistributedSet.rootNode.nKey' into a TokenName"
      )
      $ Value.mkTokenName
      $ (unwrap DistributedSet.rootNode).nKey

  let
    insertValidatorValue = Value.singleton dsKeyCurrencySymbol
      dsKeyPolicyTokenName
      one
    insertValidatorDatum = Datum
      $ PlutusData.toData
      $ DsDatum
          (unwrap DistributedSet.rootNode).nNext

  -- FUEL minting policy
  { fuelMintingPolicy } ←
    FUELMintingPolicy.V1.getFuelMintingPolicy sidechainParams

  fuelMintingPolicyCurrencySymbol ←
    getCurrencySymbol FUELMintingPolicy fuelMintingPolicy

  -- Validator for the configuration of the distributed set / the associated
  -- datum and tokens that should be paid to this validator.
  dsConfValidator ← DistributedSet.dsConfValidator ds
  let
    dsConfValidatorHash = Scripts.validatorHash dsConfValidator
    dsConfValue = Value.singleton dsConfCurrencySymbol
      DistributedSet.dsConfTokenName
      one
    dsConfValidatorDatum = Datum
      $ PlutusData.toData
      $ DsConfDatum
          { dscKeyPolicy: dsKeyCurrencySymbol
          , dscFUELPolicy: fuelMintingPolicyCurrencySymbol
          }

  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.validator insertValidator
        <> Lookups.mintingPolicy dsConfPolicy
        <> Lookups.mintingPolicy dsKeyPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintValue insertValidatorValue
        <> Constraints.mustPayToScript insertValidatorHash
          insertValidatorDatum
          DatumInline
          insertValidatorValue
        <> Constraints.mustMintValue dsConfValue
        <> Constraints.mustPayToScript dsConfValidatorHash
          dsConfValidatorDatum
          DatumInline
          dsConfValue

  pure $ burnDsInitToken <> { lookups, constraints }

-- | Build lookups and constraints to spend the genesis UTxO.  This is
-- | re-exported from the module for the purposes of testing, so that we can
-- | mint init tokens in tests when needed.
initSpendGenesisUtxo ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initSpendGenesisUtxo sidechainParams = do
  let txIn = (unwrap sidechainParams).genesisUtxo
  txOut ← liftedM (show NoGenesisUTxO) (getUtxo txIn)
  pure
    { constraints: Constraints.mustSpendPubKeyOutput txIn
    , lookups: Lookups.unspentOutputs
        ( Map.singleton txIn
            ( TransactionOutputWithRefScript
                { output: txOut, scriptRef: Nothing }
            )
        )
    }

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
  InitSidechainParams →
  Int →
  Contract
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
  { constraints, lookups } ←
    ( Checkpoint.mintOneCheckpointInitToken
        <> DistributedSet.mintOneDsInitToken
        <> CandidatePermissionToken.mintOneCandidatePermissionInitToken
        <> CommitteeOraclePolicy.mintOneCommitteeOracleInitToken
        <> initSpendGenesisUtxo
    ) sidechainParams <>
      Versioning.mintVersionInitTokens
        { atmsKind: isp.initATMSKind
        , sidechainParams
        }
        version

  txId ← balanceSignAndSubmit "Initialise Sidechain" { lookups, constraints }

  -- Mint and pay versioning tokens to versioning script.
  ----------------------------------------
  versionInitTxIds ← Versioning.initializeVersion
    { atmsKind: isp.initATMSKind, sidechainParams }
    version

  checkpointInitTxId ← initCheckpointLookupsAndConstraints isp
    >>= balanceSignAndSubmit "Checkpoint init"
  dsInitTxId ← initDistributedSetLookupsAndConstraints sidechainParams
    >>= balanceSignAndSubmit "Distributed set init"
  permissionTokensInitTxId ←
    initCandidatePermissionTokenLookupsAndConstraints isp
      >>= balanceSignAndSubmit "Candidate permission tokens init"
  committeeInitTxId ← initCommitteeHashLookupsAndConstraints isp
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
    , initTransactionIds: versionInitTxIds <>
        [ checkpointInitTxId
        , dsInitTxId
        , permissionTokensInitTxId
        , committeeInitTxId
        ]
    , sidechainParams
    , sidechainAddresses
    }
