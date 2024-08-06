module TrustlessSidechain.InitSidechain.FUEL
  ( initFuel
  ) where

import Cardano.Types.AssetName as AssetName
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript as PlutusScript
import Contract.PlutusData as PlutusData
import Contract.Prelude
  ( Maybe(Nothing)
  , bind
  , discard
  , null
  , pure
  , show
  , unwrap
  , ($)
  , (&&)
  , (<>)
  , (>>=)
  , (>>>)
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT, note, throw)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , DsConfDatum(DsConfDatum)
  , DsDatum(DsDatum)
  , DsKeyMint(DsKeyMint)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(GenericInternalError, ConversionError)
  )
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.InitSidechain.Init
  ( getScriptsToInsert
  , init
  , insertScriptsIdempotent
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getCommitteeSelectionPoliciesAndValidators
  , getDsPoliciesAndValidators
  , getFuelPoliciesAndValidators
  , getMerkleRootPoliciesAndValidators
  )
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig)
import Type.Row (type (+))

-- | Initialize the distributed set, FUELMintingPolicy, FUELBurningPolicy, and
-- | Merkle tree.
initFuel ∷
  ∀ r.
  SidechainParams →
  BigInt → -- sidechain epoch
  PlutusData →
  ATMSKinds →
  Int → -- version
  Run (APP + r)
    { scriptsInitTxIds ∷ Array TransactionHash
    , tokensInitTxId ∷ Maybe TransactionHash
    }
initFuel
  sidechainParams
  initSidechainEpoch
  initAggregatedCommittee
  initATMSKind
  version = do

  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize FUEL versioning scripts"
  scriptsInitTxIdFuel ← insertScriptsIdempotent getFuelPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  logDebug' "Attempting to initialize Ds versioning scripts"
  scriptsInitTxIdDs ← insertScriptsIdempotent getDsPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  logDebug' "Attempting to initialize MerkleRoot versioning scripts"
  scriptsInitTxIdMT ← insertScriptsIdempotent getMerkleRootPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  logDebug' "Attempting to initialize committee selection scripts"
  scriptsInitTxIdCommittee ← insertScriptsIdempotent
    (getCommitteeSelectionPoliciesAndValidators initATMSKind)
    sidechainParams
    initATMSKind
    version

  let
    scriptsInitTxIds =
      scriptsInitTxIdFuel <> scriptsInitTxIdDs <> scriptsInitTxIdMT <>
        scriptsInitTxIdCommittee

  -- Before proceeding with minting tokens we need to make sure that all the
  -- required scripts have been succesfully versioned.  We begin by acquiring
  -- the scripts that are actually in the versioning system.
  fuelScripts ← getFuelPoliciesAndValidators sidechainParams version
  dsScripts ← getDsPoliciesAndValidators sidechainParams version
  merkleRootScripts ←
    getMerkleRootPoliciesAndValidators sidechainParams version
  committeeScripts ← getCommitteeSelectionPoliciesAndValidators initATMSKind
    sidechainParams
    version

  { versionedPolicies, versionedValidators } ←
    getScriptsToInsert sidechainParams initATMSKind
      (fuelScripts <> dsScripts <> merkleRootScripts <> committeeScripts)
      version

  -- If all versioned scripts have been inserted we can proceed with minting the
  -- NFTs and other tokens.
  if null versionedPolicies && null versionedValidators then do
    -- NOTE: We check whether init-fuel is allowed
    -- by checking whether the DistributedSet.dsInitTokenName
    -- exists. There is no such init token for FUELMintingPolicy,
    -- FUELProxyPolicy etc.
    logInfo' "Attempting to mint DS and Committee NFTs from the init tokens"
    tokensInitTxId ← init
      ( \op sp → do
          fuel ← initFuelAndDsLookupsAndConstraints sp version
          committee ← initCommitteeHashLookupsAndConstraints
            initSidechainEpoch
            initAggregatedCommittee
            sp
          balanceSignAndSubmit op (fuel <> committee)
      )
      "Initialize FUEL, Distributed Set, and committee selection"
      DistributedSet.dsInitTokenName
      sidechainParams

    pure { scriptsInitTxIds, tokensInitTxId }

  else
    pure { scriptsInitTxIds, tokensInitTxId: Nothing }

-- | `initFuelAndDsLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the distributed set used
-- | for the FUEL mechanism (this does NOT submit any transaction).
-- | In particular, it includes lookups / constraints
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
initFuelAndDsLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
initFuelAndDsLookupsAndConstraints sidechainParams version = do
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
    insertValidatorHash = PlutusScript.hash insertValidator
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConfCurrencySymbol
      }

  dsKeyPolicy ← DistributedSet.dsKeyPolicy dskm
  let dsKeyCurrencySymbol = PlutusScript.hash dsKeyPolicy
  dsKeyPolicyTokenName ←
    note
      ( ConversionError
          "Failed to convert 'DistributedSet.rootNode.nKey' into a TokenName"
      )
      $ AssetName.mkAssetName
      $ (unwrap DistributedSet.rootNode).nKey

  let
    insertValidatorValue = Value.singleton dsKeyCurrencySymbol
      dsKeyPolicyTokenName
      (BigNum.fromInt 1)
    mintValidatorValue = Mint.singleton dsKeyCurrencySymbol
      dsKeyPolicyTokenName
      (Int.fromInt 1)
    insertValidatorDatum = PlutusData.toData
      $ DsDatum
          (unwrap DistributedSet.rootNode).nNext

  -- FUEL Minting policy (versioned)
  fuelMintingPolicyCurrencySymbol ←
    case version of
      1 → FUELMintingPolicy.V1.getFuelMintingPolicy sidechainParams
        >>= _.fuelMintingPolicy
        >>> PlutusScript.hash
        >>> pure
      2 → FUELMintingPolicy.V2.getFuelMintingPolicy sidechainParams
        >>= _.fuelMintingPolicy
        >>> PlutusScript.hash
        >>> pure
      _ → throw $ GenericInternalError ("Invalid version: " <> show version)

  -- Validator for the configuration of the distributed set / the associated
  -- datum and tokens that should be paid to this validator.
  dsConfValidator ← DistributedSet.dsConfValidator ds
  let
    dsConfValidatorHash = PlutusScript.hash dsConfValidator
    dsConfValue = Value.singleton dsConfCurrencySymbol
      DistributedSet.dsConfTokenName
      (BigNum.fromInt 1)
    dsConfMint = Mint.singleton dsConfCurrencySymbol
      DistributedSet.dsConfTokenName
      (Int.fromInt 1)
    dsConfValidatorDatum = PlutusData.toData
      $ DsConfDatum
          { dscKeyPolicy: dsKeyCurrencySymbol
          , dscFUELPolicy: fuelMintingPolicyCurrencySymbol
          }

  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ ScriptLookups
    lookups =
      Lookups.validator insertValidator
        <> Lookups.plutusMintingPolicy dsConfPolicy
        <> Lookups.plutusMintingPolicy dsKeyPolicy

    constraints ∷ TxConstraints
    constraints =
      Constraints.mustMintValue mintValidatorValue
        <> Constraints.mustPayToScript insertValidatorHash
          insertValidatorDatum
          DatumInline
          insertValidatorValue
        <> Constraints.mustMintValue dsConfMint
        <> Constraints.mustPayToScript dsConfValidatorHash
          dsConfValidatorDatum
          DatumInline
          dsConfValue

  pure $ burnDsInitToken <> { lookups, constraints }

-- | `initCommitteeHashLookupsAndConstraints` creates lookups and constraints
-- | to pay the NFT (which uniquely identifies the committee hash utxo) to the
-- | validator script for the update committee hash.
initCommitteeHashLookupsAndConstraints ∷
  ∀ r.
  BigInt →
  PlutusData →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
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
          (BigNum.fromInt 1)

      committeeNftMint =
        Mint.singleton
          committeeNft.currencySymbol
          CommitteeOraclePolicy.committeeOracleTn
          (Int.fromInt 1)

      mintCommitteeNft =
        { lookups: Lookups.plutusMintingPolicy committeeNft.mintingPolicy
        , constraints: Constraints.mustMintValue committeeNftMint
        }

    -- Setting up the update committee hash validator
    -----------------------------------
    let
      aggregatedKeys = initAggregatedCommittee
      committeeHashDatum = PlutusData.toData
        $ UpdateCommitteeDatum
            { aggregatePubKeys: aggregatedKeys
            , sidechainEpoch: initSidechainEpoch
            }

    versionOracleConfig ← getVersionOracleConfig sidechainParams

    committeeHashValidator ← UpdateCommitteeHash.updateCommitteeHashValidator
      sidechainParams
      versionOracleConfig

    let
      committeeHashValidatorHash = PlutusScript.hash committeeHashValidator

    -- Building the transaction
    -----------------------------------
    let
      lookups ∷ ScriptLookups
      lookups =
        Lookups.validator committeeHashValidator

      constraints ∷ TxConstraints
      constraints = Constraints.mustPayToScript committeeHashValidatorHash
        committeeHashDatum
        DatumInline
        committeeNftValue

    pure $ burnCommitteeOracleInitToken <> mintCommitteeNft <>
      { constraints, lookups }
