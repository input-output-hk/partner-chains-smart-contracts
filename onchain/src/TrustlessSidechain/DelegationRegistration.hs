{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.DelegationRegistration (
  serialisableValidator,
  mkValidator,
) where

import Plutus.V2.Ledger.Api
import PlutusTx
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

mkValidator ::
  SidechainParams ->
  Unsafe.DelegatorWalletEntry ->
  () ->
  Unsafe.ScriptContext ->
  Bool
mkValidator _sidechainParams datum _redeemer ctx =
  traceIfFalse "ERROR-DELEGATOR-REGISTRATION-01" isSigned
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    pkh :: PubKeyHash
    pkh = Unsafe.decode $ Unsafe.stakePubKeyHash datum

    isSigned :: Bool
    isSigned = Unsafe.txSignedBy info pkh

{-# INLINEABLE mkValidatorUntyped #-}
mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkValidatorUntyped params delegatorWalletEntry redeemer scriptCtx =
  check
    $ mkValidator
      (unsafeFromBuiltinData params)
      (Unsafe.wrap delegatorWalletEntry)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap scriptCtx)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
