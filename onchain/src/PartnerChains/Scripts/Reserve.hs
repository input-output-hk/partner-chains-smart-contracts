{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- |
Module      : PartnerChains.Scripts.Reserve
Description : Reserve validator and auth token minting policy.
-}
module PartnerChains.Scripts.Reserve (
  -- * Reserve validator
  -- $reserveValidator
  mkReserveValidator,
  mkReserveValidatorUntyped,
  compiledValidator,
  serialisableReserveValidator,

  -- * Reserve Auth token minting policy
  -- $reserveAuthToken
  mkReserveAuthPolicy,
  mkReserveAuthPolicyUntyped,
  compiledReserveAuthPolicy,
  serialisableReserveAuthPolicy,
  reserveAuthTokenTokenName,
) where

import PartnerChains.ScriptId qualified as ScriptId
import PartnerChains.Scripts.Versioning (
  approvedByGovernance,
  getVersionedCurrencySymbol,
  getVersionedValidatorAddress,
 )
import PartnerChains.Types (
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
  VersionOracle (VersionOracle, scriptId),
  VersionOracleConfig,
  VersionedGenericDatum,
  datum,
 )
import PartnerChains.Utils qualified as Utils
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
  AssetClass (unAssetClass),
  CurrencySymbol,
  TokenName (..),
  Value,
  assetClassValueOf,
  valueOf,
 )
import PlutusLedgerApi.V2.Data.Contexts (getContinuingOutputs, ownCurrencySymbol)
import PlutusTx qualified
import PlutusTx.Bool
import PlutusTx.Data.List qualified as List
import PlutusTx.Prelude hiding (fromInteger)

reserveAuthTokenTokenName :: TokenName
reserveAuthTokenTokenName = TokenName emptyByteString

vFunctionTotalAccruedTokenName :: TokenName
vFunctionTotalAccruedTokenName = TokenName emptyByteString

{- $reserveValidator

* All redeemers require a Reserve UTXO input carrying a Reserve Auth token.
* All redeemers except `Handover` require a Reserve UTXO output carrying a Reserve Auth token.
* The Reserve UTXO must have `VersionedGenericDatum ReserveDatum` as its datum.
* The Reserve UTXO carries native tokens, the `AssetClass` of which is specified in `ImmutableReserveSettings` in the datum.

Redeemers:

1. `DepositToReserve` allows depositing native tokens into the Reserve.

    * It is a governance action (see `approvedByGovernance`).
    * Requires the output Reserve UTXO to carry the datum from the input Reserve UTXO unaltered.
    * Requires the output Reserve UTXO to carry more native tokens than the input Reserve UTXO.
    * Required reference inputs:

        * `VersionOracle` for `ScriptId.reserveAuthPolicyId`

2. `TransferToIlliquidCirculationSupply` allows transferring native tokens from the Reserve to the Illiquid Circulation Supply (ICS).

    * Total amount of tokens that can be released at any given time is determined by the so called V-function. This function is encoded
      as a minting policy. A transaction transferring native tokens to the ICS should mint V-function tokens with this minting policy.
      The validator requires that \(pcTokensTransferred = pcTokensTransferredUntilNow - numOfVtTokensMinted\).
    * Required reference inputs:

        * `VersionOracle` UTXO for `ScriptId.reserveAuthPolicyId`
        * `VersionOracle` UTXO for `ScriptId.illiquidCirculationSupplyValidatorId`
        * V-function minting policy referenced by `tokenKind` in the Reserve UTXO datum

3. `UpdateReserve` allows updating `MutableReserveSettings` in the Reserve UTXO.

    * It is a governance action (see `approvedByGovernance`).
    * Assets may not change in the Reserve UTXO.
    * Only `MutableReserveSettings` portion of the Reserve datum may change.
    * Required reference inputs:

        * `VersionOracle` UTXO for `ScriptId.reserveAuthPolicyId`

4. `Handover` allows transferring all remaining native tokens to the ICS, essentially ending the Reserve.

    * It is a governance action (see `approvedByGovernance`).
    * Reserve Auth token must be burned.
    * All native tokens must be transferred to ICS.
    * Required reference inputs:

        * `VersionOracle` UTXO for `ScriptId.reserveAuthPolicyId`
        * `VersionOracle` UTXO for `ScriptId.illiquidCirculationSupplyValidatorId`

Error codes:

* ERROR-RESERVE-01: Governance approval is not present
* ERROR-RESERVE-02: Datum of the propagated reserve utxo changes
* ERROR-RESERVE-03: Assets of the propagated reserve utxo don't increase by native tokens
* ERROR-RESERVE-04: No unique input utxo carrying authentication token
* ERROR-RESERVE-05: No unique output utxo at the reserve address and carrying authentication token
* ERROR-RESERVE-06: Datum of input reserve utxo malformed
* ERROR-RESERVE-07: Datum of output reserve utxo malformed
* ERROR-RESERVE-08: Governance approval is not present
* ERROR-RESERVE-09: Datum of the propagated reserve utxo changes not only by immutable settings
* ERROR-RESERVE-10: Assets of the propagated reserve utxo change
* ERROR-RESERVE-11: Assets of the propagated reserve utxo don't decrease by native tokens in desired way
* ERROR-RESERVE-12: Datum of the propagated reserve utxo changes not only by stats in desired way
* ERROR-RESERVE-13: Incorrect amount of native tokens goes into an illiquid circulation supply
* ERROR-RESERVE-14: No unique output utxo at the illiquid circulation supply address
* ERROR-RESERVE-15: Governance approval is not present
* ERROR-RESERVE-16: An authentication token is not burnt
* ERROR-RESERVE-17: Not all native tokens are transferred to illiquid circulation supply
* ERROR-RESERVE-18: Continuing output exists without an authentication token
-}
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

    changeOfReserveTokens :: Integer
    changeOfReserveTokens = assetClassValueOf diff tokenKind'
      where
        diff = txOutValue outputReserveUtxo - txOutValue inputReserveUtxo

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
    assetsChangeOnlyByPositiveAmountOfReserveTokens = changeOfReserveTokens > 0

    datumChangesOnlyByMutableSettings :: Bool
    datumChangesOnlyByMutableSettings =
      let updatedMutablePart = mutableSettings $ datum outputDatum
       in toBuiltinData inputDatum {datum = (datum inputDatum) {mutableSettings = updatedMutablePart}}
            == toBuiltinData outputDatum

    assetsDoNotChange :: Bool
    assetsDoNotChange = changeOfReserveTokens == 0

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
      changeOfReserveTokens == tokensTransferredUpUntilNow - numOfVtTokensMinted

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
      List.foldr ((+) . reserveTokensOn) zero $ Utils.getInputsAt info illiquidCirculationSupplyAddress

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

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkReserveValidatorUntyped||])

serialisableReserveValidator :: SerialisedScript
serialisableReserveValidator = serialiseCompiledCode compiledValidator

{-# INLINEABLE extractReserveUtxoDatum #-}
extractReserveUtxoDatum :: TxOut -> Maybe (VersionedGenericDatum ReserveDatum)
extractReserveUtxoDatum txOut = case txOutDatum txOut of
  OutputDatum datum -> PlutusTx.fromBuiltinData . getDatum $ datum
  _ -> Nothing

{-# INLINEABLE extractReserveUtxoDatumUnsafe #-}
extractReserveUtxoDatumUnsafe :: TxOut -> Maybe (VersionedGenericDatum ReserveDatum)
extractReserveUtxoDatumUnsafe TxOut {txOutDatum = OutputDatum datum} = PlutusTx.fromBuiltinData . getDatum $ datum
extractReserveUtxoDatumUnsafe _ = Nothing

{- $reserveAuthToken

Minting a Reserve Auth token initializes the Reserve.

* It is a governance action (see `approvedByGovernance`).
* Only a single token may be minted at once.
* Minted token must go to an UTXO at the Reserve validator address, the Reserve UTXO.
* The Reserve UTXO must be unique at the address and carry correct initial datum (where `ReserveStats` is 0).
* Required reference inputs:

    * `VersionOracle` UTXO for `ScriptId.reserveValidatorId`

Burning Reserve Auth token is always allowed by the minting policy.
In practice this can only happen in the Reserve validator `Handover` redeemer.
The burning of the Reserve Auth token signifies the end of life for the Reserve.

Error codes:

* ERROR-RESERVE-AUTH-01: Governance approval is not present
* ERROR-RESERVE-AUTH-02: Single reserve authentication token is not minted
* ERROR-RESERVE-AUTH-03: Output reserve UTxO doesn't carry auth token
* ERROR-RESERVE-AUTH-04: Output reserve UTxO doesn't carry correct initial datum
* ERROR-RESERVE-AUTH-05: No unique output UTxO at the reserve address
* ERROR-RESERVE-AUTH-06: Output reserve UTxO carries no inline datum or malformed datum
-}
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
      Utils.fromSingletonData (\_ -> traceError "ERROR-RESERVE-AUTH-05")
        $ Utils.getOutputsAt info reserveAddress

    reserveUtxoValue :: Value
    reserveUtxoValue = txOutValue reserveUtxo

    reserveUtxoDatum :: VersionedGenericDatum ReserveDatum
    reserveUtxoDatum =
      case extractReserveUtxoDatum reserveUtxo of
        Just d -> d
        Nothing -> traceError "ERROR-RESERVE-AUTH-06"

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

{-# INLINEABLE mkReserveAuthPolicyUntyped #-}
mkReserveAuthPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkReserveAuthPolicyUntyped voc red ctx =
  check
    $ mkReserveAuthPolicy
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData red)
      (PlutusTx.unsafeFromBuiltinData ctx)

compiledReserveAuthPolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledReserveAuthPolicy = $$(PlutusTx.compile [||mkReserveAuthPolicyUntyped||])

serialisableReserveAuthPolicy :: SerialisedScript
serialisableReserveAuthPolicy = serialiseCompiledCode compiledReserveAuthPolicy

{- | Takes a decoded piece of data and turns it into the wrapped `BuiltinData` equivalent
  provided by `asData`, to make it compatible with functions from `PlutusLedgerApi.Vn.Data` modules.
  TODO: Wrap our own types with `asData` and get rid of this function.
-}
toAsData :: (ToData a, UnsafeFromData b) => a -> b
toAsData = unsafeFromBuiltinData . toBuiltinData
