{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- |
Module      : PartnerChains.Scripts.PermissionedCandidates
Description : Permissioned Candidates validator and minting policy.
-}
module PartnerChains.Scripts.PermissionedCandidates (
  -- * Permissioned candidates minting policy
  -- $mkMintingPolicy
  mkMintingPolicy,
  mkMintingPolicyUntyped,
  compiledMintingPolicy,
  serialisableValidator,

  -- * Permissioned candidates validator
  -- $permissionedCandidatesValidator
  permissionedCandidatesValidator,
  mkValidatorUntyped,
  compiledValidator,
  serialisableMintingPolicy,
) where

import PartnerChains.Scripts.Versioning (approvedByGovernance)
import PartnerChains.Types (
  PermissionedCandidatesPolicyRedeemer (
    PermissionedCandidatesBurn,
    PermissionedCandidatesMint
  ),
  PermissionedCandidatesValidatorRedeemer (
    RemovePermissionedCandidates,
    UpdatePermissionedCandidates
  ),
  VersionOracleConfig,
 )
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V2 (
  Address,
  ScriptContext,
  serialiseCompiledCode,
  txInfoMint,
  txInfoOutputs,
  pattern Minting,
  pattern ScriptContext,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Value (currencySymbolValueOf)
import PlutusTx qualified
import PlutusTx.Data.List qualified as List
import PlutusTx.Foldable (sum)
import PlutusTx.Prelude

{- $mkMintingPolicy

Permissioned candidates are stored in UTXOs carrying Permissioned Candidate tokens.

Redeemers:

1. `PermissionedCandidatesMint` mints Permissioned Candidate tokens for the purpose of adding new permissioned candidates.

    * It is a governance action (see `approvedByGovernance`).
    * All minted tokens must go to the `permissionedCandidatesValidatorAddress`.

2. `PermissionedCandidatesBurn` burns Permissioned Candidate tokens for the purpose of removing permissioned candidates.

    * It is a governance action (see `approvedByGovernance`).
    * Transaction must not output any Permissioned Candidate tokens.

Error codes:

* ERROR-PERMISSIONED-CANDIDATES-POLICY-01: Transaction not signed by the governance authority
* ERROR-PERMISSIONED-CANDIDATES-POLICY-02: Some tokens were not sent to the permissionedCandidatesValidatorAddress
* ERROR-PERMISSIONED-CANDIDATES-POLICY-03: Transaction not signed by the governance authority
* ERROR-PERMISSIONED-CANDIDATES-POLICY-04: Transaction outputs some PermissionedCandidatesTokens
* ERROR-PERMISSIONED-CANDIDATES-POLICY-05: Wrong ScriptContext - this should never happen
-}
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  Address ->
  PermissionedCandidatesPolicyRedeemer ->
  ScriptContext ->
  Bool
mkMintingPolicy
  _
  vc
  permissionedCandidatesValidatorAddress
  PermissionedCandidatesMint
  ctx@(ScriptContext txInfo (Minting currSym)) =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx

      -- Amount of PermissionedCandidatesToken sent to the
      -- PermissionedCandidatesValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value currSym
          | (TxOut address value _ _) <-
              List.toSOP $ txInfoOutputs txInfo
          , -- look at UTxOs that are sent to the
          -- PermissionedCandidatesValidatorAddress
          address == permissionedCandidatesValidatorAddress
          ]

      -- Amount of PermissionedCandidatesToken minted by this transaction
      mintAmount :: Integer
      mintAmount = currencySymbolValueOf (txInfoMint txInfo) currSym

      -- Check wether the amount of tokens minted equal to the amount of tokens
      -- sent to the PermissionedCandidatesValidator address
      allTokensSentToPermissionedCandidatesValidator :: Bool
      allTokensSentToPermissionedCandidatesValidator = mintAmount == outAmount
     in
      traceIfFalse "ERROR-PERMISSIONED-CANDIDATES-POLICY-01" signedByGovernanceAuthority
        && traceIfFalse
          "ERROR-PERMISSIONED-CANDIDATES-POLICY-02"
          allTokensSentToPermissionedCandidatesValidator
mkMintingPolicy
  _
  vc
  _
  PermissionedCandidatesBurn
  ctx@(ScriptContext txInfo (Minting currSym)) =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx

      -- Amount of PermissionedCandidatesToken sent output by this transaction
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value currSym
          | (TxOut _ value _ _) <-
              List.toSOP $ txInfoOutputs txInfo
          ]

      -- Check wether this transaction output any PermissionedCandidates tokens
      noOutputsWithPermissionedCandidatesToken :: Bool
      noOutputsWithPermissionedCandidatesToken = outAmount == 0
     in
      traceIfFalse
        "ERROR-PERMISSIONED-CANDIDATES-POLICY-03"
        signedByGovernanceAuthority
        && traceIfFalse
          "ERROR-PERMISSIONED-CANDIDATES-POLICY-04"
          noOutputsWithPermissionedCandidatesToken
mkMintingPolicy _ _ _ _ _ = traceError "ERROR-PERMISSIONED-CANDIDATES-POLICY-05"

{- $permissionedCandidatesValidator

Redeemers:

1. `UpdatePermissionedCandidates` allows inserting or updating the list of permissioned candidates.

    * It is a governance action (see `approvedByGovernance`).

2. `RemovePermissionedCandidates` is unused, and to be removed in the future.

Error codes:

  * ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01: Transaction not signed by the governance authority
  * ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02: Transaction not signed by the governance authority
-}
{-# INLINEABLE permissionedCandidatesValidator #-}
permissionedCandidatesValidator ::
  BuiltinData ->
  VersionOracleConfig ->
  -- Here raw BuiltinData is passed instead of
  -- 'VersionedGenericDatum ()' to allow to spend from this
  -- validator even if UTxO contains invalid datum
  BuiltinData ->
  PermissionedCandidatesValidatorRedeemer ->
  ScriptContext ->
  Bool
permissionedCandidatesValidator
  _
  vc
  _
  UpdatePermissionedCandidates
  ctx =
    traceIfFalse
      "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01"
      signedByGovernanceAuthority
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx
permissionedCandidatesValidator
  _
  vc
  _
  RemovePermissionedCandidates
  ctx =
    traceIfFalse
      "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02"
      signedByGovernanceAuthority
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx

mkMintingPolicyUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkMintingPolicyUntyped genesisUtxo vc validatorAddress redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

compiledMintingPolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledMintingPolicy = $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy = serialiseCompiledCode compiledMintingPolicy

mkValidatorUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkValidatorUntyped genesisUtxo vc datum redeemer ctx =
  check
    $ permissionedCandidatesValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      datum
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkValidatorUntyped||])

serialisableValidator :: SerialisedScript
serialisableValidator = serialiseCompiledCode compiledValidator
