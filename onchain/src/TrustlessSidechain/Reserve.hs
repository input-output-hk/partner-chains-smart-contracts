{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module TrustlessSidechain.Reserve (
  mkReserveValidator,
  mkReserveValidatorUntyped,
  serialisableReserveValidator,
  mkReserveAuthPolicy,
  mkReserveAuthPolicyUntyped,
  serialisableReserveAuthPolicy,
  reserveAuthTokenTokenName,
) where

import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  CurrencySymbol,
  TokenName,
  Value (getValue),
  adaSymbol,
  adaToken,
  assetClassValueOf,
  flattenValue,
  valueOf,
 )
import Plutus.V2.Ledger.Api (
  Address,
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  TxOut (
    txOutAddress,
    txOutDatum,
    txOutValue
  ),
  fromCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.AssocMap (lookup, toList)
import PlutusTx.Bool
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ReserveDatum (),
  ReserveRedeemer,
  ReserveStats (ReserveStats),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
  getVersionedValidatorAddressUnsafe,
  governancePolicyId,
  reserveValidatorId,
 )

reserveAuthTokenTokenName :: TokenName
reserveAuthTokenTokenName = adaToken

mkReserveValidator ::
  VersionOracleConfig ->
  ReserveDatum ->
  ReserveRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkReserveValidator _ _ _ _ = True

mkReserveValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReserveValidatorUntyped voc rd rr ctx =
  check $
    mkReserveValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (Unsafe.ScriptContext ctx)

serialisableReserveValidator :: Script
serialisableReserveValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveValidatorUntyped||])

{-# INLINEABLE getOutputsAt #-}
getOutputsAt :: Unsafe.TxInfo -> Address -> [Unsafe.TxOut]
getOutputsAt txInfo address =
  ((== address) . txOutAddress . Unsafe.decode) `filter` Unsafe.txInfoOutputs txInfo

{-# INLINEABLE extractReserveUtxoDatum #-}
extractReserveUtxoDatum :: TxOut -> Maybe ReserveDatum
extractReserveUtxoDatum txOut = case txOutDatum txOut of
  OutputDatum datum -> PlutusTx.fromBuiltinData . getDatum $ datum
  _ -> Nothing

-- | This function will be moved to a governance module in the future
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance :: VersionOracleConfig -> Unsafe.ScriptContext -> Bool
approvedByGovernance voc ctx =
  flip (maybe False) ofGovernanceCs $ \case
    [(_, 1)] -> True -- any token name is allowed
    _ -> False
  where
    ofGovernanceCs :: Maybe [(TokenName, Integer)]
    ofGovernanceCs =
      fmap toList . lookup governanceTokenCurrencySymbol . getValue $ minted

    governanceTokenCurrencySymbol :: CurrencySymbol
    governanceTokenCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        voc
        (VersionOracle {version = 1, scriptId = governancePolicyId})
        ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx

-- | Error codes description follows:
--
--   ERROR-RESERVE-01: Governance approval is not present
--   ERROR-RESERVE-02: Single reserve authentication token is not minted
--   ERROR-RESERVE-03: Output reserve UTxO doesn't carry auth token
--   ERROR-RESERVE-04: Output reserve UTxO doesn't carry correct initial datum
--   ERROR-RESERVE-05: Output reserve UTxO carries other tokens
--   ERROR-RESERVE-06: No unique output UTxO at the reserve address
--   ERROR-RESERVE-07: Output reserve UTxO carries no inline datum or malformed datum
{-# INLINEABLE mkReserveAuthPolicy #-}
mkReserveAuthPolicy :: VersionOracleConfig -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkReserveAuthPolicy voc _ ctx =
  traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
    && traceIfFalse "ERROR-RESERVE-02" oneReserveAuthTokenIsMinted
    && traceIfFalse "ERROR-RESERVE-03" reserveUtxoCarriesReserveAuthToken
    && traceIfFalse "ERROR-RESERVE-04" reserveUtxoCarriesCorrectInitialDatum
    && traceIfFalse "ERROR-RESERVE-05" reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    ownCurrencySymbol :: CurrencySymbol
    ownCurrencySymbol = Unsafe.ownCurrencySymbol ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx

    reserveAddress :: Address
    reserveAddress =
      getVersionedValidatorAddressUnsafe
        voc
        (VersionOracle {version = 1, scriptId = reserveValidatorId})
        ctx

    reserveUtxo :: TxOut
    reserveUtxo =
      Unsafe.decode $
        Utils.fromSingleton "ERROR-RESERVE-06" $
          info `getOutputsAt` reserveAddress

    reserveUtxoValue :: Value
    reserveUtxoValue = txOutValue reserveUtxo

    reserveUtxoDatum :: ReserveDatum
    reserveUtxoDatum =
      fromMaybe (traceError "ERROR-RESERVE-07") $
        extractReserveUtxoDatum reserveUtxo

    isApprovedByGovernance :: Bool
    isApprovedByGovernance = approvedByGovernance voc ctx

    oneReserveAuthTokenIsMinted :: Bool
    oneReserveAuthTokenIsMinted =
      oneTokenMinted
        minted
        ownCurrencySymbol
        reserveAuthTokenTokenName

    reserveUtxoCarriesReserveAuthToken :: Bool
    reserveUtxoCarriesReserveAuthToken =
      valueOf reserveUtxoValue ownCurrencySymbol reserveAuthTokenTokenName == 1

    reserveUtxoCarriesCorrectInitialDatum :: Bool
    reserveUtxoCarriesCorrectInitialDatum =
      get @"stats" reserveUtxoDatum == ReserveStats 0

    reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken :: Bool
    reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken =
      assetClassValueOf reserveUtxoValue tokenKind' /= 0
        && (length . flattenValue $ reserveUtxoValue) == expectedNumOfAssets
      where
        expectedNumOfAssets =
          if AssetClass (adaSymbol, adaToken) == tokenKind'
            then 2 -- ADA + reserve auth token
            else 3 -- ADA + reserve auth token + tokens of `tokenKind`
        tokenKind' = get @"tokenKind" . get @"immutableSettings" $ reserveUtxoDatum

{-# INLINEABLE mkReserveAuthPolicyUntyped #-}
mkReserveAuthPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReserveAuthPolicyUntyped voc red ctx =
  check $
    mkReserveAuthPolicy
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData red)
      (Unsafe.ScriptContext ctx)

serialisableReserveAuthPolicy :: Script
serialisableReserveAuthPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveAuthPolicyUntyped||])
