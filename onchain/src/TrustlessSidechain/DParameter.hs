{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.DParameter (
  serialisableMintingPolicy,
  serialisableValidator,
  dParameterValidator,
  mkMintingPolicy,
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
import TrustlessSidechain.Types (
  DParameterPolicyRedeemer (DParameterBurn, DParameterMint),
  DParameterValidatorRedeemer (RemoveDParameter, UpdateDParameter),
  SidechainParams,
 )
import TrustlessSidechain.Utils (currencySymbolValueOf, mkUntypedMintingPolicy, mkUntypedValidator)

-- OnChain error descriptions:
--
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
  Address ->
  DParameterPolicyRedeemer ->
  ScriptContext ->
  Bool
mkMintingPolicy
  sp
  dParameterValidatorAddress
  DParameterMint
  (ScriptContext txInfo (Minting cs)) =
    traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
      && traceIfFalse
        "ERROR-DPARAMETER-POLICY-02"
        allTokensSentToDParameterValidator
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

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

    -- Amount of DParameterToken sent output by this transaction
    outAmount :: Integer
    outAmount =
      sum
        [ currencySymbolValueOf value cs
        | (TxOut _ value _ _) <-
            txInfoOutputs txInfo
        ]
    -- Check wether this transaction output any DParameter tokens
    noOutputsWithDParameterToken :: Bool
    noOutputsWithDParameterToken = outAmount == 0
mkMintingPolicy _ _ _ _ = traceError "ERROR-DPARAMETER-POLICY-05"

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-VALIDATOR-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-VALIDATOR-03: transaction not signed by the governance
--   authority

{-# INLINEABLE dParameterValidator #-}
dParameterValidator ::
  SidechainParams ->
  -- Here raw BuiltinData is passed instead of 'DParameterValidatorDatum'
  -- to allow to spend from this validator even if UTxO contains invalid
  -- datum
  BuiltinData ->
  DParameterValidatorRedeemer ->
  ScriptContext ->
  Bool
dParameterValidator
  sp
  _
  UpdateDParameter
  (ScriptContext txInfo _) =
    traceIfFalse "ERROR-DPARAMETER-VALIDATOR-01" signedByGovernanceAuthority
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp
dParameterValidator
  sp
  _
  RemoveDParameter
  (ScriptContext txInfo _) =
    traceIfFalse "ERROR-DPARAMETER-VALIDATOR-02" signedByGovernanceAuthority
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidatorUntyped sp =
  mkUntypedValidator $
    dParameterValidator
      (unsafeFromBuiltinData sp)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkMintingPolicyUntyped sp validatorAddress =
  mkUntypedMintingPolicy $
    mkMintingPolicy
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData validatorAddress)

serialisableMintingPolicy :: Script
serialisableMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
