{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.DParameter (
  serialisableMintingPolicy,
  serialisableValidator,
) where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInInfo (TxInInfo),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (TxOut),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (
  mkUntypedMintingPolicy,
  mkUntypedValidator,
 )
import TrustlessSidechain.Types (
  DParameterPolicyRedeemer (DParameterBurn, DParameterMint),
  DParameterValidatorRedeemer (RemoveDParameter, UpdateDParameter),
  SidechainParams,
 )
import TrustlessSidechain.Utils (currencySymbolValueOf)
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  dParameterPolicyId,
  dParameterValidatorId,
  getVersionedCurrencySymbol,
  getVersionedValidatorAddress,
 )

-- OnChain error descriptions
--   ERROR-DPARAMETER-POLICY-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-POLICY-02: some tokens were not sent to the
--   dParameterValidatorAddress
--
--   ERROR-DPARAMETER-POLICY-03: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-POLICY-04: transaction outputs some dParameterTokens
--
--   ERROR-DPARAMETER-POLICY-05: Wrong ScriptContext - this should never happen
mkMintingPolicy ::
  SidechainParams ->
  VersionOracleConfig ->
  DParameterPolicyRedeemer ->
  ScriptContext ->
  Bool
mkMintingPolicy
  sp
  versionOracleConfig
  DParameterMint
  ctx@(ScriptContext txInfo (Minting cs)) =
    traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
      && traceIfFalse
        "ERROR-DPARAMETER-POLICY-02"
        allTokensSentToDParameterValidator
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- get DParameterValidator address
      dParameterValidatorAddress =
        getVersionedValidatorAddress
          versionOracleConfig
          ( VersionOracle
              { version = 1
              , scriptId = dParameterValidatorId
              }
          )
          ctx

      -- Amount of DParameterToken sent to the DParameterValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value cs
          | (TxOut address value _ _) <-
              txInfoOutputs txInfo
          , -- look at UTxOs that are sent to the dParameterValidatorAddress
          address == dParameterValidatorAddress
          ]

      -- Amount of DParameterToken minted by this transaction
      mintAmount :: Integer
      mintAmount = currencySymbolValueOf (txInfoMint txInfo) cs

      -- Check wether the amount of tokens minted equal to the amount of tokens
      -- sent to the DParameterValidator address
      allTokensSentToDParameterValidator :: Bool
      allTokensSentToDParameterValidator = mintAmount == outAmount
mkMintingPolicy sp _ DParameterBurn (ScriptContext txInfo (Minting cs)) =
  traceIfFalse "ERROR-DPARAMETER-POLICY-03" signedByGovernanceAuthority
    && traceIfFalse "ERROR-DPARAMETER-POLICY-04" noOutputsWithDParameterToken
  where
    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

    -- Amount of DParameterToken sent outputed by this transaction
    outAmount :: Integer
    outAmount =
      sum
        [ currencySymbolValueOf value cs
        | (TxOut _ value _ _) <-
            txInfoOutputs txInfo
        ]
    -- Check wether this transaction outputed any DParameter tokens
    noOutputsWithDParameterToken :: Bool
    noOutputsWithDParameterToken = outAmount == 0
mkMintingPolicy _ _ _ _ = traceError "ERROR-DPARAMETER-POLICY-05"

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-VALIDATOR-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-VALIDATOR-02: transaction moves DParameter tokens out of
--   the DParameter validator
--
--   ERROR-DPARAMETER-VALIDATOR-03: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-VALIDATOR-04: transaction did not burn DParameter tokens
{-# INLINEABLE dParameterValidator #-}
dParameterValidator ::
  SidechainParams ->
  VersionOracleConfig ->
  -- Here raw BuiltinData is passed instead of 'DParameterValidatorDatum'
  -- to allow to spend from this validator even if UTxO contains invalid
  -- datum
  BuiltinData ->
  DParameterValidatorRedeemer ->
  ScriptContext ->
  Bool
dParameterValidator
  sp
  versionOracleConfig
  _
  UpdateDParameter
  ctx@(ScriptContext txInfo _) =
    traceIfFalse "ERROR-DPARAMETER-VALIDATOR-01" signedByGovernanceAuthority
      && traceIfFalse "ERROR-DPARAMETER-VALIDATOR-02" amountsMatch
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- get DParameter currency symbol
      dParameterCurrencySymbol =
        getVersionedCurrencySymbol
          versionOracleConfig
          ( VersionOracle
              { version = 1
              , scriptId = dParameterPolicyId
              }
          )
          ctx

      -- Amount of DParameter token sent to the DParameterValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value dParameterCurrencySymbol
          | (TxOut _ value _ _) <-
              getContinuingOutputs ctx
          ]

      -- Amount of DParameter token spent by this transaction
      inAmount :: Integer
      inAmount =
        sum
          [ currencySymbolValueOf value dParameterCurrencySymbol
          | TxInInfo _ (TxOut _ value _ _) <-
              txInfoInputs txInfo
          ]

      -- Check wether the same amount of DParameter token is spent as is outputed
      -- back to the validator address
      amountsMatch :: Bool
      amountsMatch = inAmount == outAmount
dParameterValidator
  sp
  versionOracleConfig
  _
  RemoveDParameter
  ctx@(ScriptContext txInfo _) =
    traceIfFalse "ERROR-DPARAMETER-VALIDATOR-03" signedByGovernanceAuthority
      && traceIfFalse "ERROR-DPARAMETER-VALIDATOR-04" tokensBurned
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- get DParameter currency symbol
      dParameterCurrencySymbol =
        getVersionedCurrencySymbol
          versionOracleConfig
          ( VersionOracle
              { version = 1
              , scriptId = dParameterPolicyId
              }
          )
          ctx

      -- Amount of DParameterToken minted by this transaction
      mintAmount :: Integer
      mintAmount =
        currencySymbolValueOf (txInfoMint txInfo) dParameterCurrencySymbol

      -- Check whether this transaction burned some DParameter tokens
      tokensBurned :: Bool
      tokensBurned = mintAmount < 0

mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidatorUntyped sp versionOracleConfig =
  mkUntypedValidator $
    dParameterValidator
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData versionOracleConfig)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkMintingPolicyUntyped sp versioningConfig =
  mkUntypedMintingPolicy $
    mkMintingPolicy
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData versioningConfig)

serialisableMintingPolicy :: Script
serialisableMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
