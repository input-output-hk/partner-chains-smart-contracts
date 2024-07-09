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

import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass, unAssetClass),
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
    txOutDatum,
    txOutValue
  ),
  fromCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.AssocMap (lookup, toList)
import PlutusTx.Bool
import TrustlessSidechain.HaskellPrelude (on)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ReserveDatum (mutableSettings, stats),
  ReserveRedeemer (
    DepositToReserve,
    Handover,
    TransferToIlliquidCirculationSupply,
    UpdateReserve
  ),
  ReserveStats (ReserveStats),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
  getVersionedValidatorAddressUnsafe,
  governancePolicyId,
  illiquidCirculationSupplyValidatorId,
  reserveAuthPolicyId,
  reserveValidatorId,
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
--   ERROR-RESERVE-20: Continuing output exists without an authentication token
mkReserveValidator ::
  VersionOracleConfig ->
  BuiltinData ->
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
  TransferToIlliquidCirculationSupply ->
    traceIfFalse "ERROR-RESERVE-11" vtTokensAreMinted
      && traceIfFalse "ERROR-RESERVE-12" noOtherTokensButVtMinted
      && traceIfFalse "ERROR-RESERVE-13" assetsChangeOnlyByCorrectAmountOfReserveTokens
      && traceIfFalse "ERROR-RESERVE-14" datumChangesOnlyByStats
      && traceIfFalse "ERROR-RESERVE-15" correctAmountOfReserveTokensTransferredToICS
  Handover ->
    traceIfFalse "ERROR-RESERVE-01" isApprovedByGovernance
      && traceIfFalse "ERROR-RESERVE-17" oneReserveAuthTokenBurnt
      && traceIfFalse "ERROR-RESERVE-18" noOtherTokensButReserveAuthBurntAndGovernanceMinted
      && traceIfFalse "ERROR-RESERVE-19" allReserveTokensTransferredToICS
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

    illiquidCirculationSupplyAddress :: Address
    !illiquidCirculationSupplyAddress =
      getVersionedValidatorAddressUnsafe
        voc
        (VersionOracle {version = 1, scriptId = illiquidCirculationSupplyValidatorId})
        ctx

    carriesAuthToken :: Unsafe.TxOut -> Bool
    carriesAuthToken txOut =
      valueOf
        (Unsafe.decode . Unsafe.txOutValue $ txOut)
        reserveAuthCurrencySymbol
        reserveAuthTokenTokenName
        == 1

    tokenKind' :: AssetClass
    !tokenKind' = get @"tokenKind" . get @"immutableSettings" $ inputDatum

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
            sortBy ord $ -- sorting to have ADA at the head of the list
              flattenValue $
                (Unsafe.decode . Unsafe.txOutValue $ outputReserveUtxo) - (Unsafe.decode . Unsafe.txOutValue $ inputReserveUtxo)
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

    inputReserveUtxo :: Unsafe.TxOut
    !inputReserveUtxo =
      Utils.fromSingleton "ERROR-RESERVE-05" $
        filter carriesAuthToken $
          Unsafe.txInInfoResolved <$> Unsafe.txInfoInputs info

    outputReserveUtxo :: Unsafe.TxOut
    outputReserveUtxo =
      case Unsafe.getContinuingOutputs ctx of
        [out] | carriesAuthToken out -> out
        [_] -> traceError "ERROR-RESERVE-20"
        _ -> traceError "ERROR-RESERVE-06"

    inputDatum :: ReserveDatum
    !inputDatum = Utils.fromJust "ERROR-RESERVE-07" (extractReserveUtxoDatumUnsafe inputReserveUtxo)

    outputDatum :: ReserveDatum
    outputDatum = Utils.fromJust "ERROR-RESERVE-08" (extractReserveUtxoDatumUnsafe outputReserveUtxo)

    {-# INLINEABLE isApprovedByGovernance #-}
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
      Unsafe.txOutDatum inputReserveUtxo == Unsafe.txOutDatum outputReserveUtxo

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
      changeOfReserveTokens == Just 0

    outputIlliquidCirculationSupplyUtxo :: Unsafe.TxOut
    outputIlliquidCirculationSupplyUtxo =
      Utils.fromSingleton "ERROR-RESERVE-16" $
        Unsafe.getOutputsAt info illiquidCirculationSupplyAddress

    vFunctionTotalAccrued' :: CurrencySymbol
    vFunctionTotalAccrued' =
      get @"vFunctionTotalAccrued" . get @"mutableSettings" $ inputDatum

    incentiveAmount :: Integer
    incentiveAmount =
      get @"incentiveAmount" . get @"mutableSettings" $ inputDatum

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
      get @"tokenTotalAmountTransferred" . get @"stats" $ inputDatum

    assetsChangeOnlyByCorrectAmountOfReserveTokens :: Bool
    assetsChangeOnlyByCorrectAmountOfReserveTokens =
      changeOfReserveTokens == Just (tokensTransferredUpUntilNow - numOfVtTokensMinted)

    datumChangesOnlyByStats :: Bool
    datumChangesOnlyByStats =
      let updatedStats = ReserveStats numOfVtTokensMinted
       in toBuiltinData inputDatum {stats = updatedStats}
            == toBuiltinData outputDatum

    reserveTokensOn :: Unsafe.TxOut -> Integer
    reserveTokensOn txOut =
      uncurry
        (valueOf . Unsafe.decode . Unsafe.txOutValue $ txOut)
        (unAssetClass tokenKind')

    -- It is allowed to claim an incentiveAmount tokens when withdrawing assets
    -- from reserve.  All the remaining funds must be sent to ICS.
    correctAmountOfReserveTokensTransferredToICS :: Bool
    correctAmountOfReserveTokensTransferredToICS =
      reserveTokensOnOutputICSUtxo + incentiveAmount
        == (numOfVtTokensMinted - tokensTransferredUpUntilNow) + reserveTokensOnICSInputUtxos

    -- This check is performed during handover.  At this point claiming any
    -- incentive is disallowed.
    allReserveTokensTransferredToICS :: Bool
    allReserveTokensTransferredToICS =
      reserveTokensOnOutputICSUtxo
        == reserveTokensOn inputReserveUtxo + reserveTokensOnICSInputUtxos

    reserveTokensOnOutputICSUtxo :: Integer
    reserveTokensOnOutputICSUtxo =
      reserveTokensOn outputIlliquidCirculationSupplyUtxo

    reserveTokensOnICSInputUtxos :: Integer
    reserveTokensOnICSInputUtxos =
      sum $ reserveTokensOn <$> Unsafe.getInputsAt info illiquidCirculationSupplyAddress

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

mkReserveValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReserveValidatorUntyped voc rd rr ctx =
  check $
    mkReserveValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      rd
      (PlutusTx.unsafeFromBuiltinData rr)
      (Unsafe.ScriptContext ctx)

serialisableReserveValidator :: Script
serialisableReserveValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveValidatorUntyped||])

{-# INLINEABLE extractReserveUtxoDatum #-}
extractReserveUtxoDatum :: TxOut -> Maybe ReserveDatum
extractReserveUtxoDatum txOut = case txOutDatum txOut of
  OutputDatum datum -> PlutusTx.fromBuiltinData . getDatum $ datum
  _ -> Nothing

{-# INLINEABLE extractReserveUtxoDatumUnsafe #-}
extractReserveUtxoDatumUnsafe :: Unsafe.TxOut -> Maybe ReserveDatum
extractReserveUtxoDatumUnsafe txOut =
  (PlutusTx.fromBuiltinData . getDatum . Unsafe.decode) =<< (Unsafe.getOutputDatum . Unsafe.txOutDatum $ txOut)

-- | This function will be moved to a governance module in the future
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance :: VersionOracleConfig -> Unsafe.ScriptContext -> Bool
approvedByGovernance voc ctx =
  case ofGovernanceCs of
    Just [(_, 1)] -> True -- any token name is allowed
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
  if valueOf minted ownCurrencySymbol reserveAuthTokenTokenName < 0
    then True -- delegating to reserve validator
    else
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
          Unsafe.getOutputsAt info reserveAddress

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
      Utils.oneTokenMinted
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
      red
      (Unsafe.wrap ctx)

serialisableReserveAuthPolicy :: Script
serialisableReserveAuthPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveAuthPolicyUntyped||])
