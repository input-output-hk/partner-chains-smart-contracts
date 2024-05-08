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

import Plutus.V2.Ledger.Api (
  Address,
  Script,
  TxOut (TxOut),
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
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
  SidechainParams,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)

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
  SidechainParams ->
  Address ->
  PermissionedCandidatesPolicyRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkMintingPolicy
  sp
  permissionedCandidatesValidatorAddress
  PermissionedCandidatesMint
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let txInfo = Unsafe.scriptContextTxInfo ctx
          -- Check that transaction was approved by governance authority
          signedByGovernanceAuthority :: Bool
          signedByGovernanceAuthority =
            txInfo `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

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
       in traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
            && traceIfFalse
              "ERROR-PERMISSIONED-CANDIDATES-POLICY-02"
              allTokensSentToPermissionedCandidatesValidator
mkMintingPolicy
  sp
  _
  PermissionedCandidatesBurn
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let txInfo = Unsafe.scriptContextTxInfo ctx
          -- Check that transaction was approved by governance authority
          signedByGovernanceAuthority :: Bool
          signedByGovernanceAuthority =
            txInfo `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

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
mkMintingPolicy _ _ _ _ = traceError "ERROR-PERMISSIONED-CANDIDATES-POLICY-05"

-- OnChain error descriptions:
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02: transaction not signed by the
--   governance authority
{-# INLINEABLE permissionedCandidatesValidator #-}
permissionedCandidatesValidator ::
  SidechainParams ->
  -- Here raw BuiltinData is passed instead of
  -- 'PermissionedCandidatesValidatorDatum' to allow to spend from this
  -- validator even if UTxO contains invalid datum
  BuiltinData ->
  PermissionedCandidatesValidatorRedeemer ->
  Unsafe.ScriptContext ->
  Bool
permissionedCandidatesValidator
  sp
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
        Unsafe.scriptContextTxInfo ctx `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp
permissionedCandidatesValidator
  sp
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
        Unsafe.scriptContextTxInfo ctx `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

mkMintingPolicyUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped sp validatorAddress redeemer ctx =
  check $
    mkMintingPolicy
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: Script
serialisableMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

mkValidatorUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped sp address redeemer ctx =
  check $
    permissionedCandidatesValidator
      (unsafeFromBuiltinData sp)
      address
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
