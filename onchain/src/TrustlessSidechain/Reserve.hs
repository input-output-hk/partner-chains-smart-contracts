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
import TrustlessSidechain.HaskellPrelude (on)
import TrustlessSidechain.PlutusPrelude
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
--   ERROR-RESERVE-02: Other tokens than governance token are minted or burnt
--   ERROR-RESERVE-03: Datum of a propagated reserve utxo changes
--   ERROR-RESERVE-04: Assets of a propagated reserve utxo don't increase by reserve tokens
--   ERROR-RESERVE-05: No unique input utxo carrying authentication token
--   ERROR-RESERVE-06: No unique output utxo at the reserve address and carrying authentication token
--   ERROR-RESERVE-07: Datum of input reserve utxo malformed
--   ERROR-RESERVE-08: Datum of output reserve utxo malformed
--   ERROR-RESERVE-09: Datum of a propagated reserve utxo changes not only by immutable settings
--   ERROR-RESERVE-10: Assets of a propagated reserve utxo change
--   ERROR-RESERVE-11: V(t) tokens are not minted
--   ERROR-RESERVE-12: Other tokens than V(t) tokens are minted or burnt
--   ERROR-RESERVE-13: Assets of a propagated reserve utxo don't decrease by reserve tokens in desired way
--   ERROR-RESERVE-14: Datum of a propagated reserve utxo changes not only by stats in desired way
--   ERROR-RESERVE-15: Incorrect amount of reserve tokens goes into an illiquid circulation supply
--   ERROR-RESERVE-16: No unique output utxo at the illiquid circulation supply address
--   ERROR-RESERVE-17: An authentication token is not burnt
--   ERROR-RESERVE-18: Other tokens than auth token and governance token are minted or burnt
--   ERROR-RESERVE-19: Not all reserve tokens are transferred to illiquid circulation supply
--   ERROR-RESERVE-20: Reserve utxo input exists without an authentication token
--   ERROR-RESERVE-21: Reserve utxo output exists without an authentication token
--   ERROR-RESERVE-22: Governance approval is not present
--   ERROR-RESERVE-23: Other tokens than governance token are minted or burnt
--   ERROR-RESERVE-24: Governance approval is not present
--   ERROR-RESERVE-25: Continuing output exists without an authentication token
mkReserveValidator ::
  VersionOracleConfig ->
  BuiltinData ->
  ReserveRedeemer ->
  ScriptContext ->
  Bool
mkReserveValidator voc _ redeemer ctx = case redeemer of
  DepositToReserve ->
    traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-02" noOtherTokensButGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-03" datumDoesNotChange
      && traceIfFalse "ERROR-RESERVE-04" assetsChangeOnlyByPositiveAmountOfReserveTokens
  UpdateReserve ->
    traceIfFalse "ERROR-RESERVE-22" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-23" noOtherTokensButGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-09" datumChangesOnlyByMutableSettings
      && traceIfFalse "ERROR-RESERVE-10" assetsDoNotChange
  TransferToIlliquidCirculationSupply ->
    traceIfFalse "ERROR-RESERVE-11" vtTokensAreMinted
      && traceIfFalse "ERROR-RESERVE-12" noOtherTokensButVtMinted
      && traceIfFalse "ERROR-RESERVE-13" assetsChangeOnlyByCorrectAmountOfReserveTokens
      && traceIfFalse "ERROR-RESERVE-14" datumChangesOnlyByStats
      && traceIfFalse "ERROR-RESERVE-15" correctAmountOfReserveTokensTransferredToICS
  Handover ->
    traceIfFalse "ERROR-RESERVE-24" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-17" oneReserveAuthTokenBurnt
      && traceIfFalse "ERROR-RESERVE-18" noOtherTokensButReserveAuthBurntAndGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-19" allReserveTokensTransferredToICS
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
    !illiquidCirculationSupplyAddress =
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
      Utils.fromSingletonData "ERROR-RESERVE-05"
        $ List.filter carriesAuthToken
        $ List.map txInInfoResolved
        $ txInfoInputs info

    outputReserveUtxo :: TxOut
    outputReserveUtxo =
      let singleOut = Utils.fromSingletonData "ERROR-RESERVE-06" $ getContinuingOutputs ctx
       in if carriesAuthToken singleOut then singleOut else traceError "ERROR-RESERVE-25"

    inputDatum :: VersionedGenericDatum ReserveDatum
    !inputDatum = Utils.fromJust "ERROR-RESERVE-07" (extractReserveUtxoDatumUnsafe inputReserveUtxo)

    outputDatum :: VersionedGenericDatum ReserveDatum
    outputDatum = Utils.fromJust "ERROR-RESERVE-08" (extractReserveUtxoDatumUnsafe outputReserveUtxo)

    isApprovedByGovernance :: Bool
    isApprovedByGovernance = approvedByGovernance voc ctx

    {-# INLINEABLE kindsOfTokenMinted #-}
    kindsOfTokenMinted :: Integer -> Bool
    kindsOfTokenMinted n = (length . flattenValue $ minted) == n

    -- this is valid only if `isApprovedByGovernance` is True
    noOtherTokensButGovernanceMinted :: Bool
    !noOtherTokensButGovernanceMinted = kindsOfTokenMinted 1

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
      Utils.fromSingletonData "ERROR-RESERVE-16"
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

    vtTokensAreMinted :: Bool
    vtTokensAreMinted = numOfVtTokensMinted > 0

    -- this is valid only if `vtTokensAreMinted` is True
    noOtherTokensButVtMinted :: Bool
    noOtherTokensButVtMinted = kindsOfTokenMinted 1

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

    -- this is valid only if `oneReserveAuthTokenBurnt` is True
    -- and if `isApprovedByGovernance` is True
    noOtherTokensButReserveAuthBurntAndGovernanceMinted :: Bool
    noOtherTokensButReserveAuthBurntAndGovernanceMinted = kindsOfTokenMinted 2

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
      Utils.fromSingletonData "ERROR-RESERVE-AUTH-06"
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
