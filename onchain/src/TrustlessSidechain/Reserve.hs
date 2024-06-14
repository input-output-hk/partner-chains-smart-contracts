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
  ReserveDatum (mutableSettings),
  ReserveRedeemer (DepositToReserve, UpdateReserve),
  ReserveStats (ReserveStats),
 )
import TrustlessSidechain.Types.Unsafe (getContinuingOutputs)
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
  getVersionedValidatorAddressUnsafe,
  governancePolicyId,
  reserveAuthPolicyId,
  reserveValidatorId,
 )

reserveAuthTokenTokenName :: TokenName
reserveAuthTokenTokenName = adaToken

-- | Error codes description follows:
--
--   ERROR-RESERVE-01: Governance approval is not present
--   ERROR-RESERVE-02: Other tokens than governance token are minted or burnt
--   ERROR-RESERVE-03: Datum of a propagated reserve utxo changes
--   ERROR-RESERVE-04: Assets of a propagated reserve utxo don't increase by reserve tokens
--   ERROR-RESERVE-05: No unique input utxo carrying authentication token
--   ERROR-RESERVE-06: No unique output utxo at the reserve address and carrying authentication token
--   ERROR-RESERVE-07: Datum of input reserve utxo malformed
--   ERROR-RESERVE-08: Datum of output reserve utxo malformed
--   ERROR-RESERVE-09: Datum of a propagated reserve utxo changes not only by immutable settings
--   ERROR-RESERVE-10: Assets of a propagated reserve utxo change
mkReserveValidator ::
  VersionOracleConfig ->
  ReserveDatum ->
  ReserveRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkReserveValidator voc _ redeemer ctx = case redeemer of
  DepositToReserve ->
    traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-02" noOtherTokensButGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-03" datumDoesNotChange
      && traceIfFalse "ERROR-RESERVE-04" assetsChangeOnlyByPositiveAmountOfReserveTokens
  UpdateReserve ->
    traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-02" noOtherTokensButGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-09" datumChangesOnlyByMutableSettings
      && traceIfFalse "ERROR-RESERVE-10" assetsDoNotChange
  _ -> error ()
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx

    reserveAuthCurrencySymbol :: CurrencySymbol
    reserveAuthCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        voc
        (VersionOracle {version = 1, scriptId = reserveAuthPolicyId})
        ctx

    carriesAuthToken :: TxOut -> Bool
    carriesAuthToken txOut =
      valueOf
        (txOutValue txOut)
        reserveAuthCurrencySymbol
        reserveAuthTokenTokenName
        == 1

    -- this function verifies that assets of a propagated unique reserve utxo
    -- change only by reserve tokens and returns the difference wrapped in `Maybe`
    -- otherwise it returns `Nothing`
    changeOfReserveTokens :: Maybe Integer
    changeOfReserveTokens =
      let diff = txOutValue outputReserveUtxo - txOutValue inputReserveUtxo
          ac = get @"tokenKind" . get @"immutableSettings" $ inputDatum
       in case flattenValue diff of
            [(cs, tn, num)] | AssetClass (cs, tn) == ac -> Just num
            _ -> Nothing

    inputReserveUtxo :: TxOut
    inputReserveUtxo =
      Unsafe.decode $
        Utils.fromSingleton "ERROR-RESERVE-05" $
          filter (carriesAuthToken . Unsafe.decode) $
            Unsafe.txInInfoResolved <$> Unsafe.txInfoInputs info

    outputReserveUtxo :: TxOut
    outputReserveUtxo =
      Unsafe.decode $
        Utils.fromSingleton "ERROR-RESERVE-06" $
          filter (carriesAuthToken . Unsafe.decode) $
            getContinuingOutputs ctx

    inputDatum :: ReserveDatum
    inputDatum =
      case extractReserveUtxoDatum inputReserveUtxo of
        Just d -> d
        Nothing -> traceError "ERROR-RESERVE-07"

    outputDatum :: ReserveDatum
    outputDatum =
      case extractReserveUtxoDatum outputReserveUtxo of
        Just d -> d
        Nothing -> traceError "ERROR-RESERVE-08"

    isApprovedByGovernance :: Bool
    isApprovedByGovernance = approvedByGovernance voc ctx

    -- this is valid only if `isApprovedByGovernance` is True
    noOtherTokensButGovernanceMinted :: Bool
    noOtherTokensButGovernanceMinted = (length . flattenValue $ minted) == 1

    datumDoesNotChange :: Bool
    datumDoesNotChange =
      txOutDatum inputReserveUtxo == txOutDatum outputReserveUtxo

    assetsChangeOnlyByPositiveAmountOfReserveTokens :: Bool
    assetsChangeOnlyByPositiveAmountOfReserveTokens =
      maybe False (> 0) changeOfReserveTokens

    datumChangesOnlyByMutableSettings :: Bool
    datumChangesOnlyByMutableSettings =
      let updatedMutablePart = mutableSettings outputDatum
       in toBuiltinData inputDatum {mutableSettings = updatedMutablePart}
            == toBuiltinData outputDatum

    assetsDoNotChange :: Bool
    assetsDoNotChange =
      txOutValue inputReserveUtxo == txOutValue outputReserveUtxo

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
--   ERROR-RESERVE-AUTH-01: Governance approval is not present
--   ERROR-RESERVE-AUTH-02: Single reserve authentication token is not minted
--   ERROR-RESERVE-AUTH-03: Output reserve UTxO doesn't carry auth token
--   ERROR-RESERVE-AUTH-04: Output reserve UTxO doesn't carry correct initial datum
--   ERROR-RESERVE-AUTH-05: Output reserve UTxO carries other tokens
--   ERROR-RESERVE-AUTH-06: No unique output UTxO at the reserve address
--   ERROR-RESERVE-AUTH-07: Output reserve UTxO carries no inline datum or malformed datum
{-# INLINEABLE mkReserveAuthPolicy #-}
mkReserveAuthPolicy :: VersionOracleConfig -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkReserveAuthPolicy voc _ ctx =
  traceIfFalse "ERROR-RESERVE-AUTH-01" isApprovedByGovernance
    && traceIfFalse "ERROR-RESERVE-AUTH-02" oneReserveAuthTokenIsMinted
    && traceIfFalse "ERROR-RESERVE-AUTH-03" reserveUtxoCarriesReserveAuthToken
    && traceIfFalse "ERROR-RESERVE-AUTH-04" reserveUtxoCarriesCorrectInitialDatum
    && traceIfFalse "ERROR-RESERVE-AUTH-05" reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken
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
        Utils.fromSingleton "ERROR-RESERVE-AUTH-06" $
          info `getOutputsAt` reserveAddress

    reserveUtxoValue :: Value
    reserveUtxoValue = txOutValue reserveUtxo

    reserveUtxoDatum :: ReserveDatum
    reserveUtxoDatum =
      case extractReserveUtxoDatum reserveUtxo of
        Just d -> d
        Nothing -> traceError "ERROR-RESERVE-AUTH-07"

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
