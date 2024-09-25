{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.DParameter (
  serialisableMintingPolicy,
  serialisableValidator,
  dParameterValidator,
  mkMintingPolicy,
) where

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.Governance.Admin qualified as Governance
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  SidechainParams,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-POLICY-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-POLICY-02: some tokens were not sent to the
--   dParameterValidatorAddress
--
mkMintingPolicy ::
  SidechainParams ->
  Unsafe.Address ->
  CurrencySymbol ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkMintingPolicy
  sp
  dParameterValidatorAddress
  dParameterProxyCurrencySymbol
  _redeemer
  ctx =
    let txInfo = Unsafe.scriptContextTxInfo ctx

        -- Check that transaction was approved by governance authority
        signedByGovernanceAuthority :: Bool
        signedByGovernanceAuthority =
          txInfo `Governance.isApprovedByAdminUnsafe` get @"governanceAuthority" sp

        -- Amount of DParameterToken sent to the DParameterValidator address
        outAmount :: Integer
        outAmount =
          sum
            [ currencySymbolValueOf value dParameterProxyCurrencySymbol
            | txOut <- Unsafe.txInfoOutputs txInfo
            , let address = Unsafe.txOutAddress txOut
            , let value = Unsafe.decode $ Unsafe.txOutValue txOut
            , -- look at UTxOs that are sent to the dParameterValidatorAddress
            address == dParameterValidatorAddress
            ]

        -- Amount of DParameterToken minted by this transaction
        mintAmount :: Integer
        mintAmount = currencySymbolValueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) dParameterProxyCurrencySymbol

        -- Check wether the amount of tokens minted equal to the amount of tokens
        -- sent to the DParameterValidator address
        allTokensSentToDParameterValidator :: Bool
        allTokensSentToDParameterValidator = mintAmount == outAmount
     in traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
          && traceIfFalse
            "ERROR-DPARAMETER-POLICY-02"
            allTokensSentToDParameterValidator

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-VALIDATOR-01: transaction not signed by the governance
--   authority
--

{-# INLINEABLE dParameterValidator #-}
dParameterValidator ::
  SidechainParams ->
  -- Here raw BuiltinData is passed instead of 'DParameterValidatorDatum'
  -- to allow to spend from this validator even if UTxO contains invalid
  -- datum
  BuiltinData ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
dParameterValidator sp _dat _redeemer ctx =
  traceIfFalse "ERROR-DPARAMETER-VALIDATOR-01" signedByGovernanceAuthority
  where
    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      Unsafe.scriptContextTxInfo ctx `Governance.isApprovedByAdminUnsafe` get @"governanceAuthority" sp

mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidatorUntyped sp dat redeemer ctx =
  check
    $ dParameterValidator
      (unsafeFromBuiltinData sp)
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
mkMintingPolicyUntyped sp validatorAddress proxyCurrencySymbol redeemer ctx =
  check
    $ mkMintingPolicy
      (unsafeFromBuiltinData sp)
      (Unsafe.wrap validatorAddress)
      (unsafeFromBuiltinData proxyCurrencySymbol)
      redeemer
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
