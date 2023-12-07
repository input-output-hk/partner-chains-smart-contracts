{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- | This module provides functionality for storing a list of permissioned
 candidates on the mainchain so that it can be accessed by the sidechain.
-}
module TrustlessSidechain.PermissionedCandidates (
  serialisableValidator,
  serialisableMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Address,
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInfo (txInfoMint, txInfoOutputs),
  TxOut (TxOut),
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (
  mkUntypedMintingPolicy,
  mkUntypedValidator,
 )
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
  ScriptContext ->
  Bool
mkMintingPolicy
  sp
  permissionedCandidatesValidatorAddress
  PermissionedCandidatesMint
  (ScriptContext txInfo (Minting cs)) =
    traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
      && traceIfFalse
        "ERROR-PERMISSIONED-CANDIDATES-POLICY-02"
        allTokensSentToPermissionedCandidatesValidator
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- Amount of PermissionedCandidatesToken sent to the
      -- PermissionedCandidatesValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value cs
          | (TxOut address value _ _) <-
              txInfoOutputs txInfo
          , -- look at UTxOs that are sent to the
          -- PermissionedCandidatesValidatorAddress
          address == permissionedCandidatesValidatorAddress
          ]

      -- Amount of PermissionedCandidatesToken minted by this transaction
      mintAmount :: Integer
      mintAmount = currencySymbolValueOf (txInfoMint txInfo) cs

      -- Check wether the amount of tokens minted equal to the amount of tokens
      -- sent to the PermissionedCandidatesValidator address
      allTokensSentToPermissionedCandidatesValidator :: Bool
      allTokensSentToPermissionedCandidatesValidator = mintAmount == outAmount
mkMintingPolicy
  sp
  _
  PermissionedCandidatesBurn
  (ScriptContext txInfo (Minting cs)) =
    traceIfFalse
      "ERROR-PERMISSIONED-CANDIDATES-POLICY-03"
      signedByGovernanceAuthority
      && traceIfFalse
        "ERROR-PERMISSIONED-CANDIDATES-POLICY-04"
        noOutputsWithPermissionedCandidatesToken
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- Amount of PermissionedCandidatesToken sent output by this transaction
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value cs
          | (TxOut _ value _ _) <-
              txInfoOutputs txInfo
          ]

      -- Check wether this transaction output any PermissionedCandidates tokens
      noOutputsWithPermissionedCandidatesToken :: Bool
      noOutputsWithPermissionedCandidatesToken = outAmount == 0
mkMintingPolicy _ _ _ _ = traceError "ERROR-PERMISSIONED-CANDIDATES-POLICY-05"

-- OnChain error descriptions:
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02: transaction moves
--   PermissionedCandidates tokens out of the PermissionedCandidates validator
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-03: transaction not signed by the
--   governance authority
--
--   ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-04: transaction did not burn
--   PermissionedCandidates tokens

{-# INLINEABLE permissionedCandidatesValidator #-}
permissionedCandidatesValidator ::
  SidechainParams ->
  -- Here raw BuiltinData is passed instead of
  -- 'PermissionedCandidatesValidatorDatum' to allow to spend from this
  -- validator even if UTxO contains invalid datum
  BuiltinData ->
  PermissionedCandidatesValidatorRedeemer ->
  ScriptContext ->
  Bool
permissionedCandidatesValidator
  sp
  _
  UpdatePermissionedCandidates
  (ScriptContext txInfo _) =
    traceIfFalse
      "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01"
      signedByGovernanceAuthority
    where
      -- This part was removed because we remove PermissionedCandidates from
      -- the versioning system. Until some other approach will appear we are not
      -- able to have mintingPolicy and validator reference each other.
      -- Once such approach is available we should put this logic back.
      -- && traceIfFalse "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02" amountsMatch

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

-- -- get PermissionedCandidates currency symbol
-- permissionedCandidatesCurrencySymbol =
--   getVersionedCurrencySymbol
--     versionOracleConfig
--     ( VersionOracle
--         { version = 1
--         , scriptId = permissionedCandidatesPolicyId
--         }
--     )
--     ctx

-- -- Amount of PermissionedCandidates token sent to the
-- -- PermissionedCandidatesValidator address
-- outAmount :: Integer
-- outAmount =
--   sum
--     [ currencySymbolValueOf value permissionedCandidatesCurrencySymbol
--     | (TxOut _ value _ _) <-
--         getContinuingOutputs ctx
--     ]

-- -- Amount of PermissionedCandidates token spent by this transaction
-- inAmount :: Integer
-- inAmount =
--   sum
--     [ currencySymbolValueOf value permissionedCandidatesCurrencySymbol
--     | TxInInfo _ (TxOut _ value _ _) <-
--         txInfoInputs txInfo
--     ]

-- -- Check wether the same amount of PermissionedCandidates token is spent
-- -- as is output back to the validator address
-- amountsMatch :: Bool
-- amountsMatch = inAmount == outAmount
permissionedCandidatesValidator
  sp
  _
  RemovePermissionedCandidates
  (ScriptContext txInfo _) =
    traceIfFalse
      "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-03"
      signedByGovernanceAuthority
    where
      -- This part was removed because we remove PermissionedCandidates from
      -- the versioning system. Until some other approach will appear we are not
      -- able to have mintingPolicy and validator reference each other.
      -- Once such approach is available we should put this logic back.
      -- && traceIfFalse "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-04" tokensBurned

      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

-- -- get PermissionedCandidates currency symbol
-- permissionedCandidatesCurrencySymbol =
--   getVersionedCurrencySymbol
--     versionOracleConfig
--     ( VersionOracle
--         { version = 1
--         , scriptId = permissionedCandidatesPolicyId
--         }
--     )
--     ctx

-- -- Amount of PermissionedCandidatesToken minted by this transaction
-- mintAmount :: Integer
-- mintAmount =
--   currencySymbolValueOf
--     (txInfoMint txInfo)
--     permissionedCandidatesCurrencySymbol

-- -- Check whether this transaction burned some PermissionedCandidates
-- -- tokens
-- tokensBurned :: Bool
-- tokensBurned = mintAmount < 0

mkMintingPolicyUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped sp validatorAddress =
  mkUntypedMintingPolicy $
    mkMintingPolicy
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData validatorAddress)

serialisableMintingPolicy :: Script
serialisableMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

mkValidatorUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped sp =
  mkUntypedValidator $
    permissionedCandidatesValidator
      (unsafeFromBuiltinData sp)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
