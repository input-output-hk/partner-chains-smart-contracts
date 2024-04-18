{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.DataStorage (
  serialisableMintingPolicy,
  serialisableValidator,
  dataStorageValidator,
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
  SidechainParams,
 )
import TrustlessSidechain.Utils (currencySymbolValueOf, mkUntypedMintingPolicy, mkUntypedValidator)

-- OnChain error descriptions:
--
--   ERROR-DATASTORAGE-POLICY-01: transaction not signed by the governance
--   authority
--
--   ERROR-DATASTORAGE-POLICY-02: some tokens were not sent to the
--   dataStorageValidatorAddress
--
--   ERROR-DATASTORAGE-POLICY-03: Wrong ScriptContext - this should never happen
mkMintingPolicy ::
  SidechainParams ->
  Address ->
  () ->
  ScriptContext ->
  Bool
mkMintingPolicy
  sp
  dataStorageValidatorAddress
  _
  (ScriptContext txInfo (Minting cs)) =
    traceIfFalse "ERROR-DATASTORAGE-POLICY-01" signedByGovernanceAuthority
      && traceIfFalse
        "ERROR-DATASTORAGE-POLICY-02"
        allTokensSentToDataStorageValidator
    where
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedBy` get @"governanceAuthority" sp

      -- Amount of DataStorageToken sent to the DataStorageValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value cs
          | (TxOut address value _ _) <-
              txInfoOutputs txInfo
          , -- look at UTxOs that are sent to the dataStorageValidatorAddress
          address == dataStorageValidatorAddress
          ]

      -- Amount of DataStorageToken minted by this transaction
      mintAmount :: Integer
      mintAmount = currencySymbolValueOf (txInfoMint txInfo) cs

      -- Check wether the amount of tokens minted equal to the amount of tokens
      -- sent to the DataStorageValidator address
      allTokensSentToDataStorageValidator :: Bool
      allTokensSentToDataStorageValidator = mintAmount == outAmount
mkMintingPolicy _ _ _ _ = traceError "ERROR-DATASTORAGE-POLICY-03"

-- OnChain error descriptions:
--
--   ERROR-DATASTORAGE-VALIDATOR-01: transaction not signed by the governance
--   authority
--

{-# INLINEABLE dataStorageValidator #-}
dataStorageValidator ::
  SidechainParams ->
  -- Here raw BuiltinData is passed instead of 'dataStorageValidatorDatum'
  -- to allow to spend from this validator even if UTxO contains invalid
  -- datum
  BuiltinData ->
  () ->
  ScriptContext ->
  Bool
dataStorageValidator
  sp
  _
  _
  (ScriptContext txInfo _) =
    traceIfFalse "ERROR-DATASTORAGE-VALIDATOR-01" signedByGovernanceAuthority
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
    dataStorageValidator
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
