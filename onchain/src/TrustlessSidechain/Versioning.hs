{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | 'TrustlessSidechain.Versioning' module implements script versioning system.
 It provides VersionOraclePolicy for minting tokens that store versioned
 scripts, as well as VersionedOracleValidator script for storing the
 versioning tokens.  Each versioning token stores a reference script and a
 datum that identifies the script and its version.
-}
module TrustlessSidechain.Versioning (
  serialisableVersionOraclePolicy,
  serialisableVersionOracleValidator,
  getVersionedValidatorAddress,
  getVersionedCurrencySymbol,
  VersionOracle (..),
  VersionOracleConfig (..),
  -- | Script IDs
  fuelMintingPolicyId,
  merkleRootTokenPolicyId,
  merkleRootTokenValidatorId,
  committeeCandidateValidatorId,
  candidatePermissionPolicyId,
  committeeNftPolicyId,
  committeeHashPolicyId,
  committeeHashValidatorId,
  dsKeyPolicyId,
  dsConfPolicyId,
  dsConfValidatorId,
  dsInsertValidatorId,
  checkpointValidatorId,
  checkpointPolicyId,
  fuelBurningPolicyId,
  versionOraclePolicyId,
  versionOracleValidatorId,
  fuelProxyPolicyId,
  committeeCertificateVerificationPolicyId,
  committeeOraclePolicyId,
  committeePlainEcdsaSecp256k1ATMSPolicyId,
  committeePlainSchnorrSecp256k1ATMSPolicyId,
  dParameterPolicyId,
  dParameterValidatorId,
) where

import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol (CurrencySymbol), Datum (Datum), OutputDatum (OutputDatum), Script, ScriptContext (ScriptContext), ScriptHash (ScriptHash), ScriptPurpose (Minting, Spending), TokenName (TokenName), TxInInfo (TxInInfo), TxOut (TxOut), ValidatorHash (ValidatorHash), fromCompiledCode, txInInfoOutRef, txInfoInputs, txInfoOutputs, txOutValue)
import Plutus.V2.Ledger.Contexts (getContinuingOutputs, txInfoReferenceInputs)
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (mkUntypedMintingPolicy)
import TrustlessSidechain.Types (SidechainParams (SidechainParams), genesisUtxo, governanceAuthority)
import TrustlessSidechain.Utils (fromSingleton)

-- Note [Versioned script identifiers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Each versioned script has an assigned identifier.  On-chain code must permit
-- addition of new versioned scripts without any changes to existing code, i.e.
-- minting policies and validator hashes must remain stable when new script is
-- being added.  For this purpose we use integers as script identifiers, with
-- each known script being assigned its own number.  In the off-chain code we
-- decode these integers into an ADT using hand-written ToData/FromData
-- instances since it's not a problem to have changes there and it is safer and
-- more descriptive to operate on an ADT.  Integers defined in on-chain code
-- need to match instances in the off-chain portion of the code.

-- See Note [Versioned script identifiers]
fuelMintingPolicyId
  , merkleRootTokenPolicyId
  , merkleRootTokenValidatorId
  , committeeCandidateValidatorId
  , candidatePermissionPolicyId
  , committeeNftPolicyId
  , committeeHashPolicyId
  , committeeHashValidatorId
  , dsKeyPolicyId
  , dsConfPolicyId
  , dsConfValidatorId
  , dsInsertValidatorId
  , checkpointValidatorId
  , checkpointPolicyId
  , fuelBurningPolicyId
  , versionOraclePolicyId
  , versionOracleValidatorId
  , fuelProxyPolicyId
  , committeeCertificateVerificationPolicyId
  , committeeOraclePolicyId
  , committeePlainEcdsaSecp256k1ATMSPolicyId
  , committeePlainSchnorrSecp256k1ATMSPolicyId
  , dParameterPolicyId
  , dParameterValidatorId ::
    Integer
fuelMintingPolicyId = 0
merkleRootTokenPolicyId = 1
merkleRootTokenValidatorId = 2
committeeCandidateValidatorId = 3
candidatePermissionPolicyId = 4
committeeNftPolicyId = 5
committeeHashPolicyId = 6
committeeHashValidatorId = 7
dsKeyPolicyId = 8
dsConfPolicyId = 9
dsConfValidatorId = 10
dsInsertValidatorId = 11
checkpointValidatorId = 12
checkpointPolicyId = 13
fuelBurningPolicyId = 14
versionOraclePolicyId = 15
versionOracleValidatorId = 16
fuelProxyPolicyId = 17
committeeCertificateVerificationPolicyId = 18
committeeOraclePolicyId = 19
committeePlainEcdsaSecp256k1ATMSPolicyId = 20
committeePlainSchnorrSecp256k1ATMSPolicyId = 21
dParameterPolicyId = 22
dParameterValidatorId = 23

{- | Datum attached to 'VersionOraclePolicy' tokens stored on the
 'VersionOracleValidator' script.

 @since Unreleased
-}
data VersionOracle = VersionOracle
  { -- | Version of the script.
    -- @since Unreleased
    version :: Integer
  , -- | Unique identifier of the validator.
    -- @since Unreleased
    scriptId :: Integer
  }

-- | @since Unreleased
instance ToData VersionOracle where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionOracle {..}) =
    productToData2 version scriptId

-- | @since Unreleased
instance FromData VersionOracle where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 VersionOracle

-- | @since Unreleased
instance UnsafeFromData VersionOracle where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 VersionOracle

-- | @since Unreleased
instance Eq VersionOracle where
  VersionOracle v s == VersionOracle v' s' = v == v' && s == s'

{- | Configuration of the versioning system.  Contains currency symbol of
 VersionOraclePolicy tokens.  Required to identify versioning tokens that can be
 trusted.

 @since Unreleased
-}
newtype VersionOracleConfig = VersionOracleConfig
  { -- | @since Unreleased
    versionOracleCurrencySymbol :: CurrencySymbol
  }

-- | @since Unreleased
instance ToData VersionOracleConfig where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionOracleConfig {..}) =
    toBuiltinData versionOracleCurrencySymbol

-- | @since Unreleased
instance FromData VersionOracleConfig where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracleConfig <$> fromBuiltinData x

-- | @since Unreleased
instance UnsafeFromData VersionOracleConfig where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = VersionOracleConfig . unsafeFromBuiltinData

{- | Token name for versioning tokens.  Must match definition in off-chain
 | module.
-}
versionOracleTokenName :: TokenName
versionOracleTokenName = TokenName "Version oracle"

{- | Redeemer for the versioning oracle minting policy that instructs the script
 whether to mint or burn versioning tokens.

 @since Unreleased
-}
data VersionOraclePolicyRedeemer
  = -- | Mint initial versioning tokens.  Used during sidechain initialization.
    -- @since Unreleased
    InitializeVersionOracle
  | -- | Mint a new versioning token ensuring it contains correct datum and
    -- reference script.
    -- @since Unreleased
    MintVersionOracle VersionOracle ScriptHash
  | -- | Burn existing versioning token.
    -- @since Unreleased
    BurnVersionOracle VersionOracle

PlutusTx.makeIsDataIndexed
  ''VersionOraclePolicyRedeemer
  [ ('InitializeVersionOracle, 0)
  , ('MintVersionOracle, 1)
  , ('BurnVersionOracle, 2)
  ]

{- | Redeemer for versioning oracle validator script.  Used when existing
 versioning tokens are spent from the script, either to be burned or updated
 with a new script and datum.

 @since Unreleased
-}
data VersionOracleValidatorRedeemer
  = -- | Invalidate existing versioning token.
    -- @since Unreleased
    InvalidateVersionOracle VersionOracle
  | -- | Update existing versioning token.
    -- @since Unreleased
    UpdateVersionOracle VersionOracle ScriptHash

PlutusTx.makeIsDataIndexed
  ''VersionOracleValidatorRedeemer
  [ ('InvalidateVersionOracle, 0)
  , ('UpdateVersionOracle, 1)
  ]

{- | Manages minting and burning of versioning tokens.  (Note that these are
 ordinary tokens, not NFTs.)  No restrictions are placed on minting initial
 versioning tokens during sidechain initialization, other than the usual
 requirement of burning a genesis UTxO.
-}
mkVersionOraclePolicy ::
  SidechainParams ->
  VersionOraclePolicyRedeemer ->
  ScriptContext ->
  Bool
mkVersionOraclePolicy
  SidechainParams {..}
  InitializeVersionOracle
  (ScriptContext txInfo (Minting _)) =
    traceIfFalse "ERROR-VERSION-POLICY-01" isGenesisUtxoUsed
    where
      -- Ensure that the genesis UTxO is used by the transaction.
      isGenesisUtxoUsed :: Bool
      isGenesisUtxoUsed =
        genesisUtxo `elem` map txInInfoOutRef (txInfoInputs txInfo)
mkVersionOraclePolicy
  SidechainParams {..}
  (MintVersionOracle newVersionOracle newScriptHash)
  (ScriptContext txInfo (Minting currSymbol)) =
    fromSingleton "ERROR-VERSION-POLICY-02" verifyOut
      && traceIfFalse "ERROR-VERSION-POLICY-03" signedByGovernanceAuthority
    where
      versionToken :: AssetClass
      versionToken = AssetClass (currSymbol, versionOracleTokenName)

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` governanceAuthority

      -- Check that this transaction mints a token with correct datum and script
      -- hash.
      verifyOut :: [Bool]
      verifyOut =
        [ True
        | (TxOut _ value (OutputDatum (Datum datum)) (Just scriptHash')) <-
            txInfoOutputs txInfo
        , Just versionOracle' <- [PlutusTx.fromBuiltinData datum]
        , -- Check that output contains correct version oracle and a reference
        -- script with correct hash.
        versionOracle' == newVersionOracle
        , scriptHash' == newScriptHash
        , -- Check that datum is attached to a single version token.
        assetClassValueOf value versionToken == 1
        ]
mkVersionOraclePolicy
  SidechainParams {..}
  (BurnVersionOracle oldVersion)
  (ScriptContext txInfo (Minting currSymbol)) =
    fromSingleton "ERROR-VERSION-POLICY-04" versionInputPresent
      && traceIfFalse "ERROR-VERSION-POLICY-05" versionOutputAbsent
      && traceIfFalse "ERROR-VERSION-POLICY-06" signedByGovernanceAuthority
    where
      versionToken :: AssetClass
      versionToken = AssetClass (currSymbol, versionOracleTokenName)

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` governanceAuthority

      -- Check that the script version to be invalidated is present in exactly
      -- one transaction input.
      versionInputPresent :: [Bool]
      versionInputPresent =
        [ True
        | TxInInfo _ (TxOut _ value (OutputDatum (Datum datum)) _) <-
            txInfoInputs txInfo
        , Just oldVersion' <- [PlutusTx.fromBuiltinData datum]
        , -- Check we are burning correct token.
        oldVersion' == oldVersion
        , -- Check there is exactly one token in the input that we're going to
        -- burn.
        assetClassValueOf value versionToken == 1
        ]

      -- Check that the script version to be invalidated is absent from
      -- transaction outputs.
      versionOutputAbsent :: Bool
      versionOutputAbsent =
        null
          [ ()
          | txOut <- txInfoOutputs txInfo
          , assetClassValueOf (txOutValue txOut) versionToken > 0
          ]
mkVersionOraclePolicy _ _ _ =
  trace "ERROR-ORACLE-POLICY-07" False

{-# INLINEABLE mkVersionOraclePolicyUntyped #-}
mkVersionOraclePolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkVersionOraclePolicyUntyped params =
  mkUntypedMintingPolicy $
    mkVersionOraclePolicy
      (unsafeFromBuiltinData params)

serialisableVersionOraclePolicy ::
  Script
serialisableVersionOraclePolicy =
  fromCompiledCode
    $$(PlutusTx.compile [||mkVersionOraclePolicyUntyped||])

{- | Stores VersionOraclePolicy UTxOs, acting both as an oracle of available
 scripts as well as a script caching system.  UTxOs on the script are managed
 by governance authority, with VersionOraclePolicy ensuring that tokens are
 minted and burned correctly.
-}
{-# INLINEABLE mkVersionOracleValidator #-}
mkVersionOracleValidator ::
  SidechainParams ->
  CurrencySymbol ->
  VersionOracle ->
  VersionOracleValidatorRedeemer ->
  ScriptContext ->
  Bool
mkVersionOracleValidator
  SidechainParams {..}
  versionToken
  versionOracleDatum
  (InvalidateVersionOracle versionOracle)
  (ScriptContext txInfo (Spending _)) =
    traceIfFalse "ERROR-VERSION-ORACLE-01" signedByGovernanceAuthority
      && traceIfFalse "ERROR-VERSION-ORACLE-02" versionOraclesMatch
      && traceIfFalse "ERROR-VERSION-ORACLE-03" versionOutputAbsent
    where
      versionAsset = AssetClass (versionToken, versionOracleTokenName)

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` governanceAuthority

      -- Check that version oracle in the datum matches the redeemer
      versionOraclesMatch = versionOracleDatum == versionOracle

      -- Check that transaction doesn't output any version tokens.
      versionOutputAbsent =
        null
          [ ()
          | txOut <- txInfoOutputs txInfo
          , assetClassValueOf (txOutValue txOut) versionAsset > 0
          ]
mkVersionOracleValidator
  SidechainParams {..}
  versionToken
  (VersionOracle oldVersion oldScriptId)
  ( UpdateVersionOracle
      newVersionOracle@(VersionOracle _ newScriptId)
      newScriptHash
    )
  sc@(ScriptContext txInfo (Spending _)) =
    traceIfFalse "ERROR-VERSION-ORACLE-04" signedByGovernanceAuthority
      && fromSingleton "ERROR-VERSION-ORACLE-05" versionInputPresent
      && fromSingleton "ERROR-VERSION-ORACLE-06" verifyOut
    where
      versionAsset :: AssetClass
      versionAsset = AssetClass (versionToken, versionOracleTokenName)

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority = txInfo `Governance.isApprovedBy` governanceAuthority

      -- Check that the script version to be invalidated is present in exactly
      -- one transaction input.
      versionInputPresent :: [Bool]
      versionInputPresent =
        [ True
        | TxInInfo _ (TxOut _ value (OutputDatum (Datum datum)) _) <-
            txInfoInputs txInfo
        , Just (VersionOracle oldVersion' oldScriptId') <-
            [PlutusTx.fromBuiltinData datum]
        , -- Check we are invalidating correct token.
        oldVersion' == oldVersion
        , oldScriptId' == oldScriptId
        , -- Check that this input contains exactly one version token.
        assetClassValueOf value versionAsset == 1
        ]

      -- Check that this transaction produces exactly one output containing new
      -- reference script with attached datum
      verifyOut :: [Bool]
      verifyOut =
        [ True
        | (TxOut _ value (OutputDatum (Datum datum)) (Just scriptHash')) <-
            getContinuingOutputs sc
        , Just versionOracle' <- [PlutusTx.fromBuiltinData datum]
        , -- Check that output contains correct version oracle and a reference
        -- script with correct hash.
        versionOracle' == newVersionOracle
        , scriptHash' == newScriptHash
        , -- Script ID must remain the same.
        oldScriptId == newScriptId
        , -- Check that this input contains exactly one version token.
        assetClassValueOf value versionAsset == 1
        ]
mkVersionOracleValidator _ _ _ _ _ =
  trace "ERROR-VERSION-ORACLE-07" False

{-# INLINEABLE mkVersionOracleValidatorUntyped #-}
mkVersionOracleValidatorUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Version token currency symbol
  BuiltinData ->
  -- | Datum
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkVersionOracleValidatorUntyped params currSymbol datum redeemer scriptContext =
  check $
    mkVersionOracleValidator
      (PlutusTx.unsafeFromBuiltinData params)
      (PlutusTx.unsafeFromBuiltinData currSymbol)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (PlutusTx.unsafeFromBuiltinData scriptContext)

serialisableVersionOracleValidator ::
  Script
serialisableVersionOracleValidator =
  fromCompiledCode
    $$(PlutusTx.compile [||mkVersionOracleValidatorUntyped||])

{- | Searches for a specified validator script passed as a reference input.
 Note that if requested script ID corresponds to a minting policy this
 function will return a result anyway without reporting any errors.
-}
{-# INLINEABLE getVersionedValidatorAddress #-}
getVersionedValidatorAddress ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  Address
getVersionedValidatorAddress voConfig vo =
  scriptHashAddress . ValidatorHash . getVersionedScriptHash voConfig vo

{- | Searches for a specified minting policy passed as a reference input.  Note
 that if requested script ID corresponds to a validator this function will
 return a result anyway without reporting any errors.
-}
{-# INLINEABLE getVersionedCurrencySymbol #-}
getVersionedCurrencySymbol ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  CurrencySymbol
getVersionedCurrencySymbol voConfig vo sc =
  CurrencySymbol (getVersionedScriptHash voConfig vo sc)

{- | Searches script context for a reference input containing specified script
 as a reference input.  Said reference input must come from
 VersionOracleValidator script address.  Returns script hash if requested
 version exists, throws error otherwise.  Note that this function is a worker
 for 'getVersionedCurrencySymbol' and 'getVersionedValidatorAddress', and it
 is therefore the callers responsibility to correctly interpret the result as
 either a validator or a minting policy.
-}
{-# INLINEABLE getVersionedScriptHash #-}
getVersionedScriptHash ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  BuiltinByteString
getVersionedScriptHash
  (VersionOracleConfig {..})
  versionOracle
  (ScriptContext txInfo _) =
    fromSingleton "ERROR-VERSION-CURRENCY-01" $
      [ hash
      | -- Lookup reference input that:
      TxInInfo
        _
        ( TxOut
            _
            value
            (OutputDatum (Datum datum))
            (Just (ScriptHash hash))
          ) <-
        txInfoReferenceInputs txInfo
      , -- 1. Contains datum that matches desired version and scriptId.
      Just versionOracle' <- [PlutusTx.fromBuiltinData datum]
      , versionOracle' == versionOracle
      , -- 2. Contains exactly one VersionOraclePolicy token.
      let versionAsset =
            AssetClass
              ( versionOracleCurrencySymbol
              , versionOracleTokenName
              )
      , assetClassValueOf value versionAsset == 1
      ]
