module TrustlessSidechain.InitSidechain.FUEL where

import Contract.PlutusData (Datum(Datum))
import Contract.PlutusData as PlutusData
import Contract.Prelude
  ( Maybe(..)
  , Void
  , bind
  , discard
  , not
  , null
  , one
  , pure
  , show
  , unwrap
  , ($)
  , (<>)
  , (>>=)
  , (>>>)
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array ((:))
import Run (Run)
import Run.Except (EXCEPT, note, throw)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
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
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain.Init (init, insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getDsPoliciesAndValidators
  , getFuelPoliciesAndValidators
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(FUELMintingPolicy, DsKeyPolicy)
  )
import Type.Row (type (+))

-- NOTE: `initFuel` does *not* do everything necessary to
-- allow the user to mint / burn FUEL. It does not initialize
-- the committee, nor does it initialize the merkle root
-- mechanism. See this PR comment for a discussion of the
-- choice to leave it this way and possibly to alter the functionality
-- later, to make it such that this command indeed does everything
-- a user needs to mint / burn FUEL.
-- https://github.com/input-output-hk/trustless-sidechain/pull/753#discussion_r1551920822

-- | Initialize the distributed set, FUELMintingPolicy and FUELBurningPolicy.
initFuel ∷
  ∀ r.
  SidechainParams →
  ATMSKinds →
  Int →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        , sidechainParams ∷ SidechainParams
        , sidechainAddresses ∷ SidechainAddresses
        }
    )
initFuel
  sidechainParams
  initATMSKind
  version = do
  let
    msg = "Initialize FUEL and Distributed Set"
    run = init
      ( \op sp → initFuelAndDsLookupsAndConstraints sp version >>=
          balanceSignAndSubmit op
      )
      msg

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

  let
    scriptsInitTxId = scriptsInitTxIdFuel <> scriptsInitTxIdDs

  if not $ null scriptsInitTxId then do
    sidechainAddresses ←
      GetSidechainAddresses.getSidechainAddresses $
        SidechainAddressesEndpointParams
          { sidechainParams
          , atmsKind: initATMSKind
          -- NOTE: This field is used to configure minting the candidate
          -- permission tokens themselves, not the candidate permission
          -- init tokens. However it does affect the sidechainAddresses
          -- output. Whether to remove permission tokens is an ongoing
          -- discussion as of April 4, 2024. --brendanrbrown
          , usePermissionToken: false
          , version
          }

    -- NOTE: We check whether init-fuel is allowed
    -- by checking whether the DistributedSet.dsInitTokenName
    -- exists. There is no such init token for FUELMintingPolicy,
    -- FUELProxyPolicy etc.
    logInfo' msg
    fuelInitTxId ← run DistributedSet.dsInitTokenName sidechainParams

    pure
      ( Just
          { initTransactionIds: fuelInitTxId : scriptsInitTxId
          , sidechainParams
          , sidechainAddresses
          }
      )
  else do
    logInfo' "Versioning scripts for FUEL and Ds have already been initialized"
    pure Nothing

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
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
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
    insertValidatorHash = Scripts.validatorHash insertValidator
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConfCurrencySymbol
      }

  dsKeyPolicy ← DistributedSet.dsKeyPolicy dskm
  dsKeyCurrencySymbol ← getCurrencySymbol DsKeyPolicy dsKeyPolicy
  dsKeyPolicyTokenName ←
    note
      ( ConversionError
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

  -- FUEL Minting policy (versioned)
  fuelMintingPolicyCurrencySymbol ←
    case version of
      1 → FUELMintingPolicy.V1.getFuelMintingPolicy sidechainParams
        >>= _.fuelMintingPolicy
        >>> getCurrencySymbol FUELMintingPolicy
      2 → FUELMintingPolicy.V2.getFuelMintingPolicy sidechainParams
        >>= _.fuelMintingPolicy
        >>> getCurrencySymbol FUELMintingPolicy
      _ → throw $ GenericInternalError ("Invalid version: " <> show version)

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
