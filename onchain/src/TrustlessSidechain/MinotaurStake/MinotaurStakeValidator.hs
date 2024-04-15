{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'TrustlessSidechain.MinotaurStake.MinotaurStakeValidator' implements
-- a validator script for spending the "Minotaur Stake" token as part of
-- the Cardano Native Assets delegation mechanism of `trustless-sidechain`.
module TrustlessSidechain.MinotaurStake.MinotaurStakeValidator (serialisableMinotaurStakeValidator) where

import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  assetClassValueOf,
 )
import Plutus.V2.Ledger.Api (
  Address,
  Datum (Datum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  fromCompiledCode,
  txInfoInputs,
  txInfoMint,
 )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx qualified
import TrustlessSidechain.MinotaurStake.MinotaurStakeDatum (
  MinotaurStakeDatum (MinotaurStakeDatum),
  MinotaurStakeRedeemer (BurnMinotaurStake, MintMinotaurStake),
  minotaurStakeTokenName,
 )
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  fromSingleton,
  mkUntypedValidator,
 )

-- | Validator for spending a "Minotaur Stake" token, as part of
-- canceling a delegation.
--
-- OnChain error descriptions:
--
--   ERROR-MINO-VALIDATOR-01: Transaction is not signed by the correct Stake Key
--
--   ERROR-MINO-VALIDATOR-02: Transaction must burn exactly one MinotaurStake token
--
--   ERROR-MINO-VALIDATOR-03: Transaction TxOut datum must match input datum stake
--                            'stakePubKeyHash' and 'stakePoolId'
--
--   ERROR-MINO-VALIDATOR-04: Transaction must have exactly one UTxO at validator address
--
--   ERROR-MINO-VALIDATOR-05: Malformed datum in TxOut at validator address
--
--   ERROR-MINO-VALIDATOR-06: Redeemer must be BurnMinotaurStake
--
--   ERROR-MINO-VALIDATOR-07: Invalid script context
{-# INLINEABLE mkMinotaurStakeValidator #-}
mkMinotaurStakeValidator ::
  -- | Validator address
  Address ->
  MinotaurStakeDatum ->
  MinotaurStakeRedeemer ->
  ScriptContext ->
  Bool
mkMinotaurStakeValidator
  validatorAddress
  (MinotaurStakeDatum _ stakePkh poolId currSymbol)
  BurnMinotaurStake
  (ScriptContext txInfo (Spending _)) =
    traceIfFalse "ERROR-MINO-VALIDATOR-01" signedByStakeKey
      && traceIfFalse "ERROR-MINO-VALIDATOR-02" burnOneMinotaurStakeToken
      && traceIfFalse "ERROR-MINO-VALIDATOR-03" correctDatum
    where
      signedByStakeKey :: Bool
      signedByStakeKey = txInfo `txSignedBy` stakePkh

      minotaurStakeToken :: AssetClass
      minotaurStakeToken = AssetClass (currSymbol, minotaurStakeTokenName)

      burnOneMinotaurStakeToken :: Bool
      burnOneMinotaurStakeToken =
        assetClassValueOf (txInfoMint txInfo) minotaurStakeToken == -1

      validateDatum :: MinotaurStakeDatum -> Bool
      validateDatum (MinotaurStakeDatum _ stakePkh' poolId' _) =
        stakePkh == stakePkh' && poolId == poolId'

      datumAtValidatorAddress :: Maybe MinotaurStakeDatum
      datumAtValidatorAddress =
        fromSingleton
          "ERROR-MINO-VALIDATOR-04"
          [ PlutusTx.fromBuiltinData datum
          | (TxInInfo _ (TxOut address _ (OutputDatum (Datum datum)) _)) <- txInfoInputs txInfo
          , address == validatorAddress
          ]

      correctDatum :: Bool
      correctDatum =
        maybe
          (traceError "ERROR-MINO-VALIDATOR-05")
          validateDatum
          datumAtValidatorAddress
mkMinotaurStakeValidator _ _ MintMinotaurStake _ = traceError "ERROR-MINO-VALIDATOR-06"
mkMinotaurStakeValidator _ _ _ _ = traceError "ERROR-MINO-VALIDATOR-07"

{-# INLINEABLE mkMinotaurStakeValidatorUntyped #-}
mkMinotaurStakeValidatorUntyped ::
  -- | Validator address
  BuiltinData ->
  -- | Datum
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkMinotaurStakeValidatorUntyped =
  mkUntypedValidator . mkMinotaurStakeValidator . unsafeFromBuiltinData

serialisableMinotaurStakeValidator ::
  Script
serialisableMinotaurStakeValidator =
  fromCompiledCode
    $$(PlutusTx.compile [||mkMinotaurStakeValidatorUntyped||])
