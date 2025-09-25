{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : PartnerChains.Scripts.Versioning
Description : Versioning validator and minting policy.
-}
module PartnerChains.Scripts.Versioning (
  -- * Version Oracle Token minting policy
  -- $versionOraclePolicy
  mkVersionOraclePolicy,
  mkVersionOraclePolicyUntyped,
  compiledVersionOraclePolicy,
  serialisableVersionOraclePolicy,

  -- * Version Oracle Token validator
  -- $versionOracleValidator
  mkVersionOracleValidator,
  mkVersionOracleValidatorUntyped,
  compiledVersionOracleValidator,
  serialisableVersionOracleValidator,

  -- * Version oracle query functions
  getVersionedValidatorAddress,
  getVersionedCurrencySymbol,
  approvedByGovernance,
) where

import PartnerChains.ScriptId qualified as ScriptId
import PartnerChains.Types
import PartnerChains.Utils (
  fromSingleton,
 )
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
import PlutusTx.List (null)
import PlutusTx.Prelude

-- | Token name for versioning tokens.  Must match definition in off-chain.
{-# INLINEABLE versionOracleTokenName #-}
versionOracleTokenName :: TokenName
versionOracleTokenName = TokenName "Version oracle"

{- $versionOraclePolicy

This is part of Partner Chains' versioning system. The versioning system stores `ScriptId`s with their corresponding reference scripts.
This creates indirection that allows replacing scripts in the versioning system with different versions of them.

Versioned scripts can be looked up using `getVersionedCurrencySymbol` and `getVersionedScriptHash`.

Version Oracle Tokens identify authorized versioning UTXOs.

Redeemers:

1. `InitializeVersionOracle` initializes the first VersionOracle UTXO.

    * TX has to use the Genesis UTXO as an input.
    * TX must mint a single Version Oracle Token.
    * There must exist exactly one output at the validator address that carries:

        * VersionOracleDatum as datum matching script argument,
        * a script reference matching script argument,
        * and a single Version Oracle Token.

2. `MintVersionOracle` creates a new VersionOracle UTXO.

    * It is a governance action (see `approvedByGovernance`).
    * TX must mint a single Version Oracle Token.
    * There must exist exactly one output at the validator address that carries:

        * VersionOracleDatum as datum matching script argument,
        * a script reference matching script argument,
        * and a single Version Oracle Token.

3. `BurnVersionOracle` spends a VersionOracle UTXO.

    * It is a governance action (see `approvedByGovernance`).
    * There must exist exactly one input at the validator address that carries:

        * VersionOracleDatum as datum matching script argument,
        * and a single Version Oracle Token.

    * There must NOT exist any outputs carrying any Version Oracle Tokens.

Error codes:

* ERROR-VERSION-POLICY-01: Genesis UTXO was not used as input.
* ERROR-VERSION-POLICY-02: There is not a single output at the validator address with
    correct VersionOracleDatum, reference script, and a single version oracle token.
* ERROR-VERSION-POLICY-03: One versioning token was not minted.
* ERROR-VERSION-POLICY-04: There is not a single output at the validator address with
    correct VersionOracleDatum, reference script, and a single version oracle token.
* ERROR-VERSION-POLICY-05: Transaction should be signed by the governance.
* ERROR-VERSION-POLICY-06: One versioning token was not minted.
* ERROR-VERSION-POLICY-07: There is not a single input at the validator address with
    expected old VersionOracleDatum, and a single version oracle token.
* ERROR-VERSION-POLICY-08: There was an output carrying version oracle tokens.
* ERROR-VERSION-POLICY-09: Transaction should be signed by the governance.
* ERROR-VERSION-POLICY-10: Script purpose is not Minting.
-}
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
        && fromSingleton (\_ -> traceError "ERROR-VERSION-POLICY-02") (verifyOut versionOracle scriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-03" mintOneVersionToken
    MintVersionOracle newVersionOracle newScriptHash ->
      fromSingleton (\_ -> traceError "ERROR-VERSION-POLICY-04") (verifyOut newVersionOracle newScriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-05" signedByGovernanceAuthority
        && traceIfFalse "ERROR-VERSION-POLICY-06" mintOneVersionToken
    BurnVersionOracle oldVersion ->
      fromSingleton (\_ -> traceError "ERROR-VERSION-POLICY-07") (versionInputPresent oldVersion)
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
      valueOf value currSym versionOracleTokenName == 1
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
mkVersionOraclePolicy _ _ _ _ = traceError "ERROR-VERSION-POLICY-10"

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

compiledVersionOraclePolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVersionOraclePolicy = $$(PlutusTx.compile [||mkVersionOraclePolicyUntyped||])

serialisableVersionOraclePolicy ::
  SerialisedScript
serialisableVersionOraclePolicy = serialiseCompiledCode compiledVersionOraclePolicy

{- $versionOracleValidator

Stores VersionOraclePolicy UTXOs, acting both as an oracle of available
scripts as well as a script caching system. UTXOs on the script are managed
by the governance authority, with VersionOraclePolicy ensuring that tokens are
minted and burned correctly.

* Spending from the VersionOracle validator is a governance action (see `approvedByGovernance`).
* `VersionOracle` in redeemer must match the one in the datum.
* Transaction must not transfer any Version Oracle tokens to another address.

Error codes:

* ERROR-VERSION-VALIDATOR-01: Governance approval is not present
* ERROR-VERSION-VALIDATOR-02: Version oracle in the datum does not match the redeemer
* ERROR-VERSION-VALIDATOR-03: Transaction outputs version tokens to non-versioning address
* ERROR-VERSION-VALIDATOR-04: Invalid script purpose is provided
* ERROR-VERSION-VALIDATOR-05: Transaction does not have own input
-}
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
  ctx@(ScriptContext txInfo (Spending _utxo)) =
    traceIfFalse "ERROR-VERSION-VALIDATOR-01" signedByGovernanceAuthority
      && traceIfFalse "ERROR-VERSION-VALIDATOR-02" versionOraclesMatch
      && traceIfFalse "ERROR-VERSION-VALIDATOR-03" versionOutputAbsent
    where
      signedByGovernanceAuthority =
        approvedByGovernance (VersionOracleConfig currencySymbol) ctx

      -- Check that version oracle in the datum matches the redeemer
      versionOraclesMatch = versionOracle == versionOracle'

      ownAddress = case findOwnInput ctx of
        Just txIn -> txOutAddress $ txInInfoResolved txIn
        _ -> traceError "ERROR-VERSION-VALIDATOR-05"

      -- Check that transaction doesn't output any version tokens.
      versionOutputAbsent =
        List.all
          (\txOut -> (txOutAddress txOut == ownAddress) || valueOf (txOutValue txOut) currencySymbol versionOracleTokenName <= 0)
          (txInfoOutputs txInfo)
mkVersionOracleValidator _ _ _ _ = traceError "ERROR-VERSION-VALIDATOR-04"

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

compiledVersionOracleValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVersionOracleValidator = $$(PlutusTx.compile [||mkVersionOracleValidatorUntyped||])

serialisableVersionOracleValidator ::
  SerialisedScript
serialisableVersionOracleValidator = serialiseCompiledCode compiledVersionOracleValidator

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
  scriptHashAddress . ScriptHash . getVersionedScriptHash voConfig vo

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
  VersionOracleConfig {versionOracleCurrencySymbol}
  versionOracle
  (ScriptContext txInfo _) =
    fromSingleton (\_ -> traceError "ERROR-VERSION-CURRENCY-01")
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

{- | Check whether a given transaction is approved by Partner Chain governance.
The actual check is delegated to a governance minting policy stored in the
versioning system. Caller specifies the requested governance version.
The transaction must mint at least one token of the governance minting policy to
signify transaction approval.

Scripts calling `approvedByGovernance` must provide the VersionOracle UTXO for
`ScriptId.governancePolicyId` as a reference input.
-}
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
