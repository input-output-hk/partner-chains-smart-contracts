{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | This module provides functionality for storing a list of permissioned
-- candidates on the mainchain so that it can be accessed by the sidechain.
module TrustlessSidechain.PermissionedCandidates (
  serialisableValidator,
  serialisableMintingPolicy,
  permissionedCandidatesValidator,
  mkMintingPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 (
  Address,
  TxOut (TxOut),
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  PermissionedCandidatesPolicyRedeemer (
    PermissionedCandidatesBurn,
    PermissionedCandidatesMint
  ),
  PermissionedCandidatesValidatorRedeemer (
    RemovePermissionedCandidates,
    UpdatePermissionedCandidates
  ),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)
import TrustlessSidechain.Versioning (VersionOracleConfig, approvedByGovernance)

-- OnChain error descriptions:
--
--   ERROR-PERMISSIONED-CANDIDATES-POLICY-01: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-POLICY-02: some tokens were not sent to the
--   PermissionedCandidatesValidatorAddress
--
--   ERROR-PERMISSIONED-CANDIDATES-POLICY-03: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-POLICY-04: transaction outputs some
--   PermissionedCandidatesTokens
--
--   ERROR-PERMISSIONED-CANDIDATES-POLICY-05: Wrong ScriptContext - this should
--   never happen
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  Address ->
  PermissionedCandidatesPolicyRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkMintingPolicy
  _
  vc
  permissionedCandidatesValidatorAddress
  PermissionedCandidatesMint
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
        let txInfo = Unsafe.scriptContextTxInfo ctx
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
                    Unsafe.decode <$> Unsafe.txInfoOutputs txInfo
                , -- look at UTxOs that are sent to the
                -- PermissionedCandidatesValidatorAddress
                address == permissionedCandidatesValidatorAddress
                ]

            -- Amount of PermissionedCandidatesToken minted by this transaction
            mintAmount :: Integer
            mintAmount = currencySymbolValueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) currSym

            -- Check wether the amount of tokens minted equal to the amount of tokens
            -- sent to the PermissionedCandidatesValidator address
            allTokensSentToPermissionedCandidatesValidator :: Bool
            allTokensSentToPermissionedCandidatesValidator = mintAmount == outAmount
         in traceIfFalse "ERROR-PERMISSIONED-CANDIDATES-POLICY-01" signedByGovernanceAuthority
              && traceIfFalse
                "ERROR-PERMISSIONED-CANDIDATES-POLICY-02"
                allTokensSentToPermissionedCandidatesValidator
mkMintingPolicy
  _
  vc
  _
  PermissionedCandidatesBurn
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
        let txInfo = Unsafe.scriptContextTxInfo ctx
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
                    Unsafe.decode <$> Unsafe.txInfoOutputs txInfo
                ]

            -- Check wether this transaction output any PermissionedCandidates tokens
            noOutputsWithPermissionedCandidatesToken :: Bool
            noOutputsWithPermissionedCandidatesToken = outAmount == 0
         in traceIfFalse
              "ERROR-PERMISSIONED-CANDIDATES-POLICY-03"
              signedByGovernanceAuthority
              && traceIfFalse
                "ERROR-PERMISSIONED-CANDIDATES-POLICY-04"
                noOutputsWithPermissionedCandidatesToken
mkMintingPolicy _ _ _ _ _ = traceError "ERROR-PERMISSIONED-CANDIDATES-POLICY-05"

-- OnChain error descriptions:
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02: transaction not signed by the
--   governance authority
{-# INLINEABLE permissionedCandidatesValidator #-}
permissionedCandidatesValidator ::
  BuiltinData ->
  VersionOracleConfig ->
  -- Here raw BuiltinData is passed instead of
  -- 'VersionedGenericDatum ()' to allow to spend from this
  -- validator even if UTxO contains invalid datum
  BuiltinData ->
  PermissionedCandidatesValidatorRedeemer ->
  Unsafe.ScriptContext ->
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
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped genesisUtxo vc validatorAddress redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

mkValidatorUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped genesisUtxo vc datum redeemer ctx =
  check
    $ permissionedCandidatesValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      datum
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
