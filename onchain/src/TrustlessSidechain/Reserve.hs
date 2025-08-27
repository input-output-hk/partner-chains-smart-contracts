{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module TrustlessSidechain.Reserve (
  mkReserveValidator,
  mkReserveValidatorUntyped,
  serialisableReserveValidator,
  mkReserveAuthPolicy,
  mkReserveAuthPolicyUntyped,
  serialisableReserveAuthPolicy,
  reserveAuthTokenTokenName,
) where

import Data.Function (on)
import GHC.Exts (fromString)
import GHC.Num (fromInteger)
import PlutusLedgerApi.Data.V2 (
  Address,
  Datum (getDatum),
  ScriptContext,
  SerialisedScript,
  TxInfo,
  TxOut,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txInInfoResolved,
  txInfoInputs,
  txInfoMint,
  txOutDatum,
  txOutValue,
  pattern OutputDatum,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Value (
  AssetClass (AssetClass, unAssetClass),
  CurrencySymbol,
  TokenName,
  Value,
  adaSymbol,
  adaToken,
  assetClassValueOf,
  flattenValue,
  valueOf,
 )
import PlutusLedgerApi.V2.Data.Contexts (getContinuingOutputs, ownCurrencySymbol)
import PlutusTx qualified
import PlutusTx.Bool
import PlutusTx.Data.List qualified as List
import PlutusTx.Foldable (sum)
import PlutusTx.List (length, sortBy)
import PlutusTx.Prelude hiding (fromInteger)
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types (
  ImmutableReserveSettings (tokenKind),
  MutableReserveSettings (incentiveAmount, vFunctionTotalAccrued),
  ReserveDatum (immutableSettings, mutableSettings, stats),
  ReserveRedeemer (
    DepositToReserve,
    Handover,
    TransferToIlliquidCirculationSupply,
    UpdateReserve
  ),
  ReserveStats (ReserveStats, tokenTotalAmountTransferred),
  VersionedGenericDatum,
  datum,
 )
import TrustlessSidechain.Utils (ifThenElse)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId),
  VersionOracleConfig,
  approvedByGovernance,
  getVersionedCurrencySymbol,
  getVersionedValidatorAddress,
 )

reserveAuthTokenTokenName :: TokenName
reserveAuthTokenTokenName = adaToken

vFunctionTotalAccruedTokenName :: TokenName
vFunctionTotalAccruedTokenName = adaToken

-- | Error codes description follows:
--
--   ERROR-RESERVE-01: Governance approval is not present
--   ERROR-RESERVE-02: Datum of the propagated reserve utxo changes
--   ERROR-RESERVE-03: Assets of the propagated reserve utxo don't increase by reserve tokens
--   ERROR-RESERVE-04: No unique input utxo carrying authentication token
--   ERROR-RESERVE-05: No unique output utxo at the reserve address and carrying authentication token
--   ERROR-RESERVE-06: Datum of input reserve utxo malformed
--   ERROR-RESERVE-07: Datum of output reserve utxo malformed
--   ERROR-RESERVE-08: Governance approval is not present
--   ERROR-RESERVE-09: Datum of the propagated reserve utxo changes not only by immutable settings
--   ERROR-RESERVE-10: Assets of the propagated reserve utxo change
--   ERROR-RESERVE-11: Assets of the propagated reserve utxo don't decrease by reserve tokens in desired way
--   ERROR-RESERVE-12: Datum of the propagated reserve utxo changes not only by stats in desired way
--   ERROR-RESERVE-13: Incorrect amount of reserve tokens goes into an illiquid circulation supply
--   ERROR-RESERVE-14: No unique output utxo at the illiquid circulation supply address
--   ERROR-RESERVE-15: Governance approval is not present
--   ERROR-RESERVE-16: An authentication token is not burnt
--   ERROR-RESERVE-17: Not all reserve tokens are transferred to illiquid circulation supply
--   ERROR-RESERVE-18: Continuing output exists without an authentication token
mkReserveValidator ::
  VersionOracleConfig ->
  BuiltinData ->
  ReserveRedeemer ->
  ScriptContext ->
  Bool
mkReserveValidator voc _ redeemer ctx = case redeemer of
  DepositToReserve ->
    traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-02" datumDoesNotChange
      && traceIfFalse "ERROR-RESERVE-03" assetsChangeOnlyByPositiveAmountOfReserveTokens
  UpdateReserve ->
    traceIfFalse "ERROR-RESERVE-08" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-09" datumChangesOnlyByMutableSettings
      && traceIfFalse "ERROR-RESERVE-10" assetsDoNotChange
  TransferToIlliquidCirculationSupply ->
    traceIfFalse "ERROR-RESERVE-11" assetsChangeOnlyByCorrectAmountOfReserveTokens
      && traceIfFalse "ERROR-RESERVE-12" datumChangesOnlyByStats
      && traceIfFalse "ERROR-RESERVE-13" correctAmountOfReserveTokensTransferredToICS
  Handover ->
    traceIfFalse "ERROR-RESERVE-15" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-16" oneReserveAuthTokenBurnt
      && traceIfFalse "ERROR-RESERVE-17" allReserveTokensTransferredToICS
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted :: Value
    minted = txInfoMint . scriptContextTxInfo $ ctx

    reserveAuthCurrencySymbol :: CurrencySymbol
    reserveAuthCurrencySymbol =
      getVersionedCurrencySymbol
        voc
        (VersionOracle {scriptId = ScriptId.reserveAuthPolicyId})
        ctx

    illiquidCirculationSupplyAddress :: Address
    illiquidCirculationSupplyAddress =
      getVersionedValidatorAddress
        voc
        (VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyValidatorId})
        ctx

    carriesAuthToken :: TxOut -> Bool
    carriesAuthToken txOut =
      valueOf
        (txOutValue txOut)
        reserveAuthCurrencySymbol
        reserveAuthTokenTokenName
        == 1

    tokenKind' :: AssetClass
    !tokenKind' = toAsData . tokenKind . immutableSettings $ datum inputDatum

    -- This function verifies that assets of a propagated unique reserve utxo
    -- change only by reserve tokens and returns the difference of reserve tokens
    -- wrapped in `Just`. If ADA is not a reserve token then this function allows
    -- arbitrary change of ADA on the propagated reserve utxo.
    -- That's to account for `minAda` changes.
    --
    -- If other assets besides reserve tokens change it returns `Nothing`.
    changeOfReserveTokens :: Maybe Integer
    changeOfReserveTokens =
      let nestedTuples (a, b, c) = (a, (b, c)) -- use lexicographical order
          ord = compare `on` nestedTuples
          isAda cs tn = cs == adaSymbol && tn == adaToken
          isReserveToken cs tn = AssetClass (cs, tn) == tokenKind'
          diff =
            sortBy ord
              $ flattenValue -- sorting to have ADA at the head of the list
              $ txOutValue outputReserveUtxo
              - txOutValue inputReserveUtxo
          adaAsReserveToken = tokenKind' == AssetClass (adaSymbol, adaToken)
       in case diff of
            -- in two following cases reserve tokens do not change
            [] -> Just 0
            [(cs, tn, _)] | isAda cs tn && not adaAsReserveToken -> Just 0
            -- in two following cases reserve tokens do change
            [(cs1, tn1, _), (cs2, tn2, num2)]
              | isAda cs1 tn1 && isReserveToken cs2 tn2 ->
                  Just num2
            [(cs, tn, num)] | isReserveToken cs tn -> Just num
            -- every other change is invalid
            _ -> Nothing

    inputReserveUtxo :: TxOut
    !inputReserveUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-RESERVE-04")
        $ List.filter carriesAuthToken
        $ List.map txInInfoResolved
        $ txInfoInputs info

    outputReserveUtxo :: TxOut
    outputReserveUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-RESERVE-05")
        $ List.filter (traceIfFalse "ERROR-RESERVE-18" . carriesAuthToken)
        $ getContinuingOutputs ctx

    inputDatum :: VersionedGenericDatum ReserveDatum
    !inputDatum = Utils.fromJust (\_ -> traceError "ERROR-RESERVE-06") (extractReserveUtxoDatumUnsafe inputReserveUtxo)

    outputDatum :: VersionedGenericDatum ReserveDatum
    outputDatum = Utils.fromJust (\_ -> traceError "ERROR-RESERVE-07") (extractReserveUtxoDatumUnsafe outputReserveUtxo)

    isApprovedByGovernance :: Bool
    isApprovedByGovernance = approvedByGovernance voc ctx

    datumDoesNotChange :: Bool
    datumDoesNotChange =
      txOutDatum inputReserveUtxo == txOutDatum outputReserveUtxo

    assetsChangeOnlyByPositiveAmountOfReserveTokens :: Bool
    assetsChangeOnlyByPositiveAmountOfReserveTokens =
      maybe False (> 0) changeOfReserveTokens

    datumChangesOnlyByMutableSettings :: Bool
    datumChangesOnlyByMutableSettings =
      let updatedMutablePart = mutableSettings $ datum outputDatum
       in toBuiltinData inputDatum {datum = (datum inputDatum) {mutableSettings = updatedMutablePart}}
            == toBuiltinData outputDatum

    assetsDoNotChange :: Bool
    assetsDoNotChange =
      changeOfReserveTokens == Just 0

    outputIlliquidCirculationSupplyUtxo :: TxOut
    outputIlliquidCirculationSupplyUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-RESERVE-14")
        $ Utils.getOutputsAt info illiquidCirculationSupplyAddress

    vFunctionTotalAccrued' :: CurrencySymbol
    vFunctionTotalAccrued' =
      toAsData . vFunctionTotalAccrued . mutableSettings $ datum inputDatum

    incentiveAmount' :: Integer
    incentiveAmount' =
      incentiveAmount . mutableSettings $ datum inputDatum

    numOfVtTokensMinted :: Integer
    numOfVtTokensMinted =
      valueOf minted vFunctionTotalAccrued' vFunctionTotalAccruedTokenName

    tokensTransferredUpUntilNow :: Integer
    tokensTransferredUpUntilNow =
      tokenTotalAmountTransferred . stats $ datum inputDatum

    assetsChangeOnlyByCorrectAmountOfReserveTokens :: Bool
    assetsChangeOnlyByCorrectAmountOfReserveTokens =
      changeOfReserveTokens == Just (tokensTransferredUpUntilNow - numOfVtTokensMinted)

    datumChangesOnlyByStats :: Bool
    datumChangesOnlyByStats =
      let updatedStats = ReserveStats numOfVtTokensMinted
       in toBuiltinData inputDatum {datum = (datum inputDatum) {stats = updatedStats}}
            == toBuiltinData outputDatum

    reserveTokensOn :: TxOut -> Integer
    reserveTokensOn txOut =
      uncurry
        (valueOf . txOutValue $ txOut)
        (unAssetClass tokenKind')

    -- It is allowed to claim an incentiveAmount tokens when withdrawing assets
    -- from reserve.  All the remaining funds must be sent to ICS.
    correctAmountOfReserveTokensTransferredToICS :: Bool
    correctAmountOfReserveTokensTransferredToICS =
      reserveTokensOnOutputICSUtxo
        + incentiveAmount'
        == (numOfVtTokensMinted - tokensTransferredUpUntilNow)
        + reserveTokensOnICSInputUtxos

    -- This check is performed during handover.  At this point claiming any
    -- incentive is disallowed.
    allReserveTokensTransferredToICS :: Bool
    allReserveTokensTransferredToICS =
      reserveTokensOnOutputICSUtxo
        == reserveTokensOn inputReserveUtxo
        + reserveTokensOnICSInputUtxos

    reserveTokensOnOutputICSUtxo :: Integer
    reserveTokensOnOutputICSUtxo =
      reserveTokensOn outputIlliquidCirculationSupplyUtxo

    reserveTokensOnICSInputUtxos :: Integer
    reserveTokensOnICSInputUtxos =
      sum $ List.toSOP . List.map reserveTokensOn $ Utils.getInputsAt info illiquidCirculationSupplyAddress

    oneReserveAuthTokenBurnt ::
      Bool
    oneReserveAuthTokenBurnt =
      valueOf
        minted
        reserveAuthCurrencySymbol
        reserveAuthTokenTokenName
        == -1

mkReserveValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkReserveValidatorUntyped voc rd rr ctx =
  check
    $ mkReserveValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      rd
      (PlutusTx.unsafeFromBuiltinData rr)
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableReserveValidator :: SerialisedScript
serialisableReserveValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkReserveValidatorUntyped||])

{-# INLINEABLE extractReserveUtxoDatum #-}
extractReserveUtxoDatum :: TxOut -> Maybe (VersionedGenericDatum ReserveDatum)
extractReserveUtxoDatum txOut = case txOutDatum txOut of
  OutputDatum datum -> PlutusTx.fromBuiltinData . getDatum $ datum
  _ -> Nothing

{-# INLINEABLE extractReserveUtxoDatumUnsafe #-}
extractReserveUtxoDatumUnsafe :: TxOut -> Maybe (VersionedGenericDatum ReserveDatum)
extractReserveUtxoDatumUnsafe TxOut {txOutDatum = OutputDatum datum} = PlutusTx.fromBuiltinData . getDatum $ datum
extractReserveUtxoDatumUnsafe _ = Nothing

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
mkReserveAuthPolicy ::
  VersionOracleConfig ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkReserveAuthPolicy voc _ ctx =
  if valueOf minted ownCurrSym reserveAuthTokenTokenName < 0
    then True -- delegating to reserve validator
    else
      traceIfFalse "ERROR-RESERVE-AUTH-01" isApprovedByAdminGovernance
        && traceIfFalse "ERROR-RESERVE-AUTH-02" oneReserveAuthTokenIsMinted
        && traceIfFalse "ERROR-RESERVE-AUTH-03" reserveUtxoCarriesReserveAuthToken
        && traceIfFalse "ERROR-RESERVE-AUTH-04" reserveUtxoCarriesCorrectInitialDatum
        && traceIfFalse "ERROR-RESERVE-AUTH-05" reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCurrSym :: CurrencySymbol
    ownCurrSym = ownCurrencySymbol ctx

    minted :: Value
    minted = txInfoMint . scriptContextTxInfo $ ctx

    reserveAddress :: Address
    reserveAddress =
      getVersionedValidatorAddress
        voc
        (VersionOracle {scriptId = ScriptId.reserveValidatorId})
        ctx

    reserveUtxo :: TxOut
    reserveUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-RESERVE-AUTH-06")
        $ Utils.getOutputsAt info reserveAddress

    reserveUtxoValue :: Value
    reserveUtxoValue = txOutValue reserveUtxo

    reserveUtxoDatum :: VersionedGenericDatum ReserveDatum
    reserveUtxoDatum =
      case extractReserveUtxoDatum reserveUtxo of
        Just d -> d
        Nothing -> traceError "ERROR-RESERVE-AUTH-07"

    isApprovedByAdminGovernance :: Bool
    isApprovedByAdminGovernance = approvedByGovernance voc ctx

    oneReserveAuthTokenIsMinted :: Bool
    oneReserveAuthTokenIsMinted =
      Utils.oneTokenMinted
        info
        ownCurrSym
        reserveAuthTokenTokenName

    reserveUtxoCarriesReserveAuthToken :: Bool
    reserveUtxoCarriesReserveAuthToken =
      valueOf reserveUtxoValue ownCurrSym reserveAuthTokenTokenName == 1

    reserveUtxoCarriesCorrectInitialDatum :: Bool
    reserveUtxoCarriesCorrectInitialDatum =
      stats (datum reserveUtxoDatum) == ReserveStats 0

    reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken :: Bool
    reserveUtxoCarriesOnlyAdaTokenKindAndAuthToken =
      assetClassValueOf reserveUtxoValue tokenKind'
        /= 0
        && (length . flattenValue $ reserveUtxoValue)
        == expectedNumOfAssets
      where
        expectedNumOfAssets =
          if AssetClass (adaSymbol, adaToken) == tokenKind'
            then 2 -- ADA + reserve auth token
            else 3 -- ADA + reserve auth token + tokens of `tokenKind`
        tokenKind' :: AssetClass
        tokenKind' = toAsData . tokenKind . immutableSettings $ datum reserveUtxoDatum

{-# INLINEABLE mkReserveAuthPolicyUntyped #-}
mkReserveAuthPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkReserveAuthPolicyUntyped voc red ctx =
  check
    $ mkReserveAuthPolicy
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData red)
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableReserveAuthPolicy :: SerialisedScript
serialisableReserveAuthPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkReserveAuthPolicyUntyped||])

-- | Takes a decoded piece of data and turns it into the wrapped `BuiltinData` equivalent
--   provided by `asData`, to make it compatible with functions from `PlutusLedgerApi.Vn.Data` modules.
--   TODO: Wrap our own types with `asData` and get rid of this function.
toAsData :: (ToData a, UnsafeFromData b) => a -> b
toAsData = unsafeFromBuiltinData . toBuiltinData
