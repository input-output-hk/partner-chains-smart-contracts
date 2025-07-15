{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | 'TrustlessSidechain.Versioning' module implements script versioning system.
-- It provides VersionOraclePolicy for minting tokens that store versioned
-- scripts, as well as VersionedOracleValidator script for storing the
-- versioning tokens.  Each versioning token stores a reference script and a
-- datum that identifies the script and its version.
module TrustlessSidechain.Versioning (
  serialisableVersionOraclePolicy,
  serialisableVersionOracleValidator,
  mkVersionOraclePolicy,
  mkVersionOracleValidator,
  mkVersionOraclePolicyUntyped,
  mkVersionOracleValidatorUntyped,
  getVersionedValidatorAddress,
  getVersionedCurrencySymbol,
  approvedByGovernance,
  VersionOracle (..),
  VersionOracleDatum (..),
  VersionOracleConfig (..),
  VersionOraclePolicyRedeemer (..),
  versionOracleTokenName,
) where

import PlutusLedgerApi.Data.V2 (
  Address,
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  ScriptContext,
  ScriptHash (ScriptHash),
  SerialisedScript,
  TokenName (TokenName),
  TxOutRef,
  Value,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txInInfoOutRef,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoReferenceInputs,
  txOutAddress,
  txOutValue,
  pattern Minting,
  pattern OutputDatum,
  pattern ScriptContext,
  pattern Spending,
  pattern TxInInfo,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Data.Value (getValue, valueOf)
import PlutusLedgerApi.V2.Data.Contexts (findOwnInput)
import PlutusTx qualified
import PlutusTx.Data.AssocMap (lookup, toSOPList)
import PlutusTx.Data.List qualified as List
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types
import TrustlessSidechain.Utils (
  fromSingleton,
 )

-- | Token name for versioning tokens.  Must match definition in off-chain
-- | module.
{-# INLINEABLE versionOracleTokenName #-}
versionOracleTokenName :: TokenName
versionOracleTokenName = TokenName "Version oracle"

-- | Manages minting and burning of versioning tokens.  (Note that these are
-- ordinary tokens, not NFTs.)  No restrictions are placed on minting initial
-- versioning tokens during sidechain initialization, other than the usual
-- requirement of burning a genesis UTxO.
--
-- OnChain error descriptions:
--
--   ERROR-VERSION-POLICY-01: Transaction should burn exactly one init token.
--
--   ERROR-VERSION-POLICY-02: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-03: Transaction should mint exactly oneversion oracle token.
--
--   ERROR-VERSION-POLICY-04: Script to be invalidated should be present in
--     exactly one transaction input.
--
--   ERROR-VERSION-POLICY-05: Transaction should be signed by the governance.
--
--   ERROR-VERSION-POLICY-06: Transaction should burn all versioning tokens in
--     the input.
--
--   ERROR-VERSION-POLICY-07: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-08: Script can only be used for Minting purpose.
--
--   ERROR-VERSION-POLICY-09: Transaction should be signed by the governance.
--
--   ERROR-VERSION-POLICY-10: Script purpose is not Minting.
{-# INLINEABLE mkVersionOraclePolicy #-}
mkVersionOraclePolicy ::
  TxOutRef ->
  Address ->
  VersionOraclePolicyRedeemer ->
  ScriptContext ->
  Bool
mkVersionOraclePolicy genesisUtxo validatorAddress redeemer ctx@(ScriptContext txInfo (Minting currSym)) =
  case redeemer of
    InitializeVersionOracle versionOracle scriptHash ->
      traceIfFalse "ERROR-VERSION-POLICY-01" isGenesisUtxoUsed
        && fromSingleton "ERROR-VERSION-POLICY-02" (verifyOut versionOracle scriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-03" mintOneVersionToken
    MintVersionOracle newVersionOracle newScriptHash ->
      fromSingleton "ERROR-VERSION-POLICY-04" (verifyOut newVersionOracle newScriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-05" signedByGovernanceAuthority
        && traceIfFalse "ERROR-VERSION-POLICY-06" mintOneVersionToken
    BurnVersionOracle oldVersion ->
      fromSingleton "ERROR-VERSION-POLICY-07" (versionInputPresent oldVersion)
        && traceIfFalse "ERROR-VERSION-POLICY-08" versionOutputAbsent
        && traceIfFalse "ERROR-VERSION-POLICY-09" signedByGovernanceAuthority
  where
    isGenesisUtxoUsed :: Bool
    isGenesisUtxoUsed =
      List.any ((genesisUtxo ==) . txInInfoOutRef) (txInfoInputs $ scriptContextTxInfo ctx)

    mintOneVersionToken :: Bool
    mintOneVersionToken =
      valueOf (txInfoMint txInfo) currSym versionOracleTokenName == 1

    -- Check that this transaction mints a token with correct datum and script
    -- hash.
    verifyOut :: VersionOracle -> ScriptHash -> [Bool]
    verifyOut versionOracle scriptHash =
      [ True
      | (TxOut address value (OutputDatum (Datum datum)) (Just scriptHash')) <-
          List.toSOP $ txInfoOutputs txInfo
      , address == validatorAddress
      , Just (VersionOracleDatum versionOracle' _) <-
          [PlutusTx.fromBuiltinData datum]
      , -- Check that output contains correct version oracle and a reference
      -- script with correct hash.
      versionOracle' == versionOracle
      , scriptHash' == scriptHash
      , -- Check that datum is attached to a single version token.
      valueOf value currSym versionOracleTokenName >= 1
      ]

    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      approvedByGovernance (VersionOracleConfig currSym) ctx

    -- Check that the script version to be invalidated is present in exactly
    -- one transaction input.
    versionInputPresent :: VersionOracle -> [Bool]
    versionInputPresent oldVersion =
      [ True
      | TxInInfo _ (TxOut address value (OutputDatum (Datum datum)) _) <-
          List.toSOP $ txInfoInputs txInfo
      , address == validatorAddress
      , Just (VersionOracleDatum oldVersion' _) <-
          [PlutusTx.fromBuiltinData datum]
      , -- Check we are burning correct token.
      oldVersion' == oldVersion
      , -- Check there is exactly one token in the input that we're going to
      -- burn.
      valueOf value currSym versionOracleTokenName == 1
      ]

    -- Check that the script version to be invalidated is absent from
    -- transaction outputs.
    versionOutputAbsent :: Bool
    versionOutputAbsent =
      null
        [ ()
        | txOut <- List.toSOP $ txInfoOutputs txInfo
        , valueOf (txOutValue txOut) currSym versionOracleTokenName > 0
        ]
mkVersionOraclePolicy _ _ _ _ = traceError "ERROR-ORACLE-POLICY-10"

{-# INLINEABLE mkVersionOraclePolicyUntyped #-}
mkVersionOraclePolicyUntyped ::
  -- | Genesis Utxo
  BuiltinData ->
  -- | Validator address
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  BuiltinUnit
mkVersionOraclePolicyUntyped genesisUtxo validatorAddress redeemer ctx =
  check
    $ mkVersionOraclePolicy
      (unsafeFromBuiltinData genesisUtxo)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

serialisableVersionOraclePolicy ::
  SerialisedScript
serialisableVersionOraclePolicy =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkVersionOraclePolicyUntyped||])

-- | Stores VersionOraclePolicy UTxOs, acting both as an oracle of available
-- scripts as well as a script caching system.  UTxOs on the script are managed
-- by governance authority, with VersionOraclePolicy ensuring that tokens are
-- minted and burned correctly.
{-# INLINEABLE mkVersionOracleValidator #-}
mkVersionOracleValidator ::
  BuiltinData ->
  VersionOracleDatum ->
  VersionOracle ->
  ScriptContext ->
  Bool
mkVersionOracleValidator
  _genesisUtxo
  (VersionOracleDatum versionOracle currencySymbol)
  versionOracle'
  ctx@(ScriptContext txInfo (Spending _currSym)) =
    traceIfFalse "ERROR-VERSION-ORACLE-01" signedByGovernanceAuthority
      && traceIfFalse "ERROR-VERSION-ORACLE-02" versionOraclesMatch
      && traceIfFalse "ERROR-VERSION-ORACLE-03" versionOutputAbsent
    where
      signedByGovernanceAuthority =
        approvedByGovernance (VersionOracleConfig currencySymbol) ctx

      -- Check that version oracle in the datum matches the redeemer
      versionOraclesMatch = versionOracle == versionOracle'

      ownAddress = case findOwnInput ctx of
        Just txIn -> txOutAddress $ txInInfoResolved txIn
        _ -> traceError "ERROR-VERSION-ORACLE-05"

      -- Check that transaction doesn't output any version tokens.
      versionOutputAbsent =
        List.all
          (\txOut -> (txOutAddress txOut == ownAddress) || valueOf (txOutValue txOut) currencySymbol versionOracleTokenName <= 0)
          (txInfoOutputs txInfo)
mkVersionOracleValidator _ _ _ _ = traceError "ERROR-VERSION-ORACLE-04"

{-# INLINEABLE mkVersionOracleValidatorUntyped #-}
mkVersionOracleValidatorUntyped ::
  -- | Genesis UTXO
  BuiltinData ->
  -- | Datum
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  BuiltinUnit
mkVersionOracleValidatorUntyped genesisUtxo datum redeemer ctx =
  check
    $ mkVersionOracleValidator
      genesisUtxo
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

serialisableVersionOracleValidator ::
  SerialisedScript
serialisableVersionOracleValidator =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkVersionOracleValidatorUntyped||])

-- | Searches for a specified validator script passed as a reference input.
-- Note that if requested script ID corresponds to a minting policy this
-- function will return a result anyway without reporting any errors.
{-# INLINEABLE getVersionedValidatorAddress #-}
getVersionedValidatorAddress ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  Address
getVersionedValidatorAddress voConfig vo =
  scriptHashAddress . ScriptHash . getVersionedScriptHash voConfig vo

-- | Searches for a specified minting policy passed as a reference input.  Note
-- that if requested script ID corresponds to a validator this function will
-- return a result anyway without reporting any errors.
{-# INLINEABLE getVersionedCurrencySymbol #-}
getVersionedCurrencySymbol ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  CurrencySymbol
getVersionedCurrencySymbol voConfig vo sc =
  CurrencySymbol (getVersionedScriptHash voConfig vo sc)

-- | Searches script context for a reference input containing specified script
-- as a reference input.  Said reference input must come from
-- VersionOracleValidator script address.  Returns script hash if requested
-- version exists, throws error otherwise.  Note that this function is a worker
-- for 'getVersionedCurrencySymbol' and 'getVersionedValidatorAddress', and it
-- is therefore the callers responsibility to correctly interpret the result as
-- either a validator or a minting policy.
{-# INLINEABLE getVersionedScriptHash #-}
getVersionedScriptHash ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  BuiltinByteString
getVersionedScriptHash
  VersionOracleConfig {versionOracleCurrencySymbol}
  versionOracle
  (ScriptContext txInfo _) =
    fromSingleton "ERROR-VERSION-CURRENCY-01"
      $ [ hash
        | -- Lookup reference input that:
        TxInInfo
          _
          ( TxOut
              _
              value
              (OutputDatum (Datum datum))
              (Just (ScriptHash hash))
            ) <-
          List.toSOP (txInfoReferenceInputs txInfo `List.append` txInfoInputs txInfo)
        , -- 1. Contains datum that matches desired version and scriptId.
        Just (VersionOracleDatum versionOracle' _) <- [PlutusTx.fromBuiltinData datum]
        , versionOracle' == versionOracle
        , -- 2. Contains exactly one VersionOraclePolicy token.
        valueOf value versionOracleCurrencySymbol versionOracleTokenName == 1
        ]

-- | Check whether a given transaction is approved by sidechain governance.  The
-- actual check is delegated to a governance minting policy stored in the
-- versioning system.  Caller specifies the requested governance version.  The
-- transaction must mint at least one token of the governance minting policy to
-- signify transaction approval.
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance ::
  VersionOracleConfig ->
  ScriptContext ->
  Bool
approvedByGovernance voc ctx =
  case ofGovernanceCs of
    Just [(_, amount)] | amount > 0 -> True -- must mint at least one token, any name
    _ -> False
  where
    ofGovernanceCs :: Maybe [(TokenName, Integer)]
    ofGovernanceCs =
      fmap toSOPList . lookup governanceTokenCurrencySymbol . getValue $ minted

    governanceTokenCurrencySymbol :: CurrencySymbol
    governanceTokenCurrencySymbol =
      getVersionedCurrencySymbol
        voc
        (VersionOracle {scriptId = ScriptId.governancePolicyId})
        ctx

    minted :: Value
    minted = txInfoMint . scriptContextTxInfo $ ctx
