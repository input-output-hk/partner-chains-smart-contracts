{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'TrustlessSidechain.Versioning' module implements script versioning system.
-- It provides VersionOraclePolicy for minting tokens that store versioned
-- scripts, as well as VersionedOracleValidator script for storing the
-- versioning tokens.  Each versioning token stores a reference script and a
-- datum that identifies the script and its version.
module TrustlessSidechain.MinotaurStake.MinotaurStakePolicy (serialisableMinotaurStakePolicy) where

import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  assetClassValueOf,
 )
import Plutus.V2.Ledger.Api (
  Address,
  Datum (Datum),
  OutputDatum (OutputDatum),
  PubKeyHash,
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  fromCompiledCode,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
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
  mkUntypedMintingPolicy,
 )

-- | Manages minting and burning of MinotaurStake tokens.
--
-- OnChain error descriptions:
--
--   ERROR-MINO-POLICY-01: Transaction is not signed by the correct Stake Key
--
--   ERROR-MINO-POLICY-02: Transaction must mint exactly one MinotaurStake token
--
--   ERROR-MINO-POLICY-03: Stake key not found in input
--
--   ERROR-MINO-POLICY-04: Transaction is not signed by the correct Stake Key
--
--   ERROR-MINO-POLICY-05: Transaction must mint exactly one MinotaurStake token
--
--   ERROR-MINO-POLICY-06: Stake key not found in input
--
--   ERROR-MINO-POLICY-07: bad datum attached to the output utxo
--
--   ERROR-MINO-POLICY-08: should never happen
mkMinotaurStakePolicy ::
  -- | Validator address
  Address ->
  MinotaurStakeRedeemer ->
  ScriptContext ->
  Bool
mkMinotaurStakePolicy
  validatorAddress
  MintMinotaurStake
  (ScriptContext txInfo (Minting currSymbol)) =
    traceIfFalse "ERROR-MINO-POLICY-01" signedByStakeKey
      && traceIfFalse "ERROR-MINO-POLICY-02" mintOneMinotaurStakeToken
    where
      minotaurStakeToken :: AssetClass
      minotaurStakeToken = AssetClass (currSymbol, minotaurStakeTokenName)

      mintOneMinotaurStakeToken :: Bool
      mintOneMinotaurStakeToken =
        assetClassValueOf (txInfoMint txInfo) minotaurStakeToken == 1

      -- Check that this transaction mints a token with correct datum
      stakePubKeyHash :: PubKeyHash
      stakePubKeyHash =
        fromSingleton
          "ERROR-MINO-POLICY-03"
          [ stakePkh
          | (TxOut address value (OutputDatum (Datum datum)) _) <-
              txInfoOutputs txInfo
          , let (MinotaurStakeDatum _ stakePkh _ currSymbol') =
                  case PlutusTx.fromBuiltinData datum of
                    Just x -> x
                    Nothing -> traceError "ERROR-MINO-POLICY-07"
          , assetClassValueOf value minotaurStakeToken == 1
          , address == validatorAddress
          , currSymbol' == currSymbol
          ]

      signedByStakeKey :: Bool
      signedByStakeKey = txInfo `txSignedBy` stakePubKeyHash
mkMinotaurStakePolicy
  validatorAddress
  BurnMinotaurStake
  (ScriptContext txInfo (Minting currSymbol)) =
    traceIfFalse "ERROR-MINO-POLICY-04" signedByStakeKey
      && traceIfFalse "ERROR-MINO-POLICY-05" burnOneMinotaurStakeToken
    where
      minotaurStakeToken :: AssetClass
      minotaurStakeToken = AssetClass (currSymbol, minotaurStakeTokenName)

      burnOneMinotaurStakeToken :: Bool
      burnOneMinotaurStakeToken =
        assetClassValueOf (txInfoMint txInfo) minotaurStakeToken == -1

      stakePubKeyHash :: PubKeyHash
      stakePubKeyHash =
        fromSingleton
          "ERROR-MINO-POLICY-06"
          [ stakePkh
          | (TxInInfo _ (TxOut address value (OutputDatum (Datum datum)) _)) <- txInfoInputs txInfo
          , let (MinotaurStakeDatum _ stakePkh _ currSymbol') =
                  case PlutusTx.fromBuiltinData datum of
                    Just x -> x
                    Nothing -> traceError "ERROR-MINO-POLICY-07"
          , assetClassValueOf value minotaurStakeToken == 1
          , address == validatorAddress
          , currSymbol' == currSymbol
          ]

      signedByStakeKey :: Bool
      signedByStakeKey = txInfo `txSignedBy` stakePubKeyHash
mkMinotaurStakePolicy _ _ _ = traceError "ERROR-MINO-POLICY-08"

{-# INLINEABLE mkMinotaurStakePolicyUntyped #-}
mkMinotaurStakePolicyUntyped ::
  -- | Validator address
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkMinotaurStakePolicyUntyped validatorAddress =
  mkUntypedMintingPolicy $
    mkMinotaurStakePolicy $
      unsafeFromBuiltinData validatorAddress

serialisableMinotaurStakePolicy ::
  Script
serialisableMinotaurStakePolicy =
  fromCompiledCode
    $$(PlutusTx.compile [||mkMinotaurStakePolicyUntyped||])
