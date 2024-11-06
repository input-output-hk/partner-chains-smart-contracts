{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.DParameter (
  serialisableMintingPolicy,
  serialisableValidator,
  dParameterValidator,
  mkMintingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)
import TrustlessSidechain.Versioning (VersionOracleConfig, approvedByGovernance)

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-POLICY-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-POLICY-02: some tokens were not sent to the
--   dParameterValidatorAddress
--
--   ERROR-DPARAMETER-POLICY-03: Wrong ScriptContext - this should never happen
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  Unsafe.Address ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkMintingPolicy
  _genesisUtxo
  vc
  dParameterValidatorAddress
  _redeemer
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
        let txInfo = Unsafe.scriptContextTxInfo ctx

            -- Check that transaction was approved by governance authority
            signedByGovernanceAuthority :: Bool
            signedByGovernanceAuthority =
              approvedByGovernance vc ctx

            -- Amount of DParameterToken sent to the DParameterValidator address
            outAmount :: Integer
            outAmount =
              sum
                [ currencySymbolValueOf value currSym
                | txOut <- Unsafe.txInfoOutputs txInfo
                , let address = Unsafe.txOutAddress txOut
                , let value = Unsafe.decode $ Unsafe.txOutValue txOut
                , -- look at UTxOs that are sent to the dParameterValidatorAddress
                address == dParameterValidatorAddress
                ]

            -- Amount of DParameterToken minted by this transaction
            mintAmount :: Integer
            mintAmount = currencySymbolValueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) currSym

            -- Check whether the amount of tokens minted is equal to the
            -- amount of tokens sent to the DParameterValidator address
            allTokensSentToDParameterValidator :: Bool
            allTokensSentToDParameterValidator = mintAmount == outAmount
         in traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
              && traceIfFalse
                "ERROR-DPARAMETER-POLICY-02"
                allTokensSentToDParameterValidator
mkMintingPolicy _ _ _ _ _ = traceError "ERROR-DPARAMETER-POLICY-03"

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-VALIDATOR-01: transaction not signed by the governance
--   authority
--

{-# INLINEABLE dParameterValidator #-}
dParameterValidator ::
  BuiltinData ->
  -- Here raw BuiltinData is passed instead of 'DParameterValidatorDatum'
  -- to allow to spend from this validator even if UTxO contains invalid
  -- datum
  VersionOracleConfig ->
  BuiltinData -> -- VersionedGenericDatum ()
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
dParameterValidator _genesisUtxo vc _dat _redeemer ctx =
  traceIfFalse "ERROR-DPARAMETER-VALIDATOR-01" signedByGovernanceAuthority
  where
    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      approvedByGovernance vc ctx

mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidatorUntyped genesisUtxo vc dat redeemer ctx =
  check
    $ dParameterValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      dat
      redeemer
      (Unsafe.wrap ctx)

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkMintingPolicyUntyped genesisUtxo vc validatorAddress redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      (Unsafe.wrap validatorAddress)
      redeemer
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
