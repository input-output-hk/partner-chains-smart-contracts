{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | 'TrustlessSidechain.Versioning' module implements script versioning system.
-- It provides VersionOraclePolicy for minting tokens that store versioned
-- scripts, as well as VersionedOracleValidator script for storing the
-- versioning tokens.  Each versioning token stores a reference script and a
-- datum that identifies the script and its version.
module TrustlessSidechain.Versioning (
  serialisableVersionOraclePolicy,
  serialisableVersionOracleValidator,
  mkVersionOraclePolicy,
  mkVersionOracleValidator,
  getVersionedValidatorAddress,
  getVersionedValidatorAddressUnsafe,
  getVersionedCurrencySymbol,
  getVersionedCurrencySymbolUnsafe,
  approvedByGovernance,
  VersionOracle (..),
  VersionOracleDatum (..),
  VersionOracleConfig (..),
  VersionOraclePolicyRedeemer,
) where

import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (getValue, valueOf)
import PlutusLedgerApi.V2 (Address, CurrencySymbol (CurrencySymbol), Datum (Datum), OutputDatum (OutputDatum), ScriptContext (ScriptContext), ScriptHash (ScriptHash), SerialisedScript, TokenName (TokenName), TxInInfo (TxInInfo), TxOut (TxOut), Value, serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts (txInfoInputs, txInfoReferenceInputs)
import PlutusTx qualified
import PlutusTx.AssocMap (lookup, toList)
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (
  fromJust,
  fromSingleton,
 )

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
--
-- @since v5.0.0
newtype VersionOracle = VersionOracle
  { scriptId :: Integer
  -- ^ Unique identifier of the validator.
  -- @since v5.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracle where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracle {scriptId} =
    toBuiltinData scriptId

-- | @since v5.0.0
instance FromData VersionOracle where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracle <$> fromBuiltinData x

-- | @since v5.0.0
instance UnsafeFromData VersionOracle where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x = VersionOracle $ unsafeFromBuiltinData x

-- | @since v5.0.0
instance Eq VersionOracle where
  VersionOracle s == VersionOracle s' = s == s'

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
data VersionOracleDatum = VersionOracleDatum
  { versionOracle :: VersionOracle
  -- ^ VersionOracle which identifies the script.
  -- @since v6.0.0
  , currencySymbol :: CurrencySymbol
  -- ^ Currency Symbol of the VersioningOraclePolicy tokens.
  -- @since v6.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v6.0.0
instance ToData VersionOracleDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleDatum {versionOracle, currencySymbol} =
    productToData2 versionOracle currencySymbol

-- | @since v6.0.0
instance FromData VersionOracleDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 VersionOracleDatum

-- | @since v6.0.0
instance UnsafeFromData VersionOracleDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 VersionOracleDatum

-- | @since v6.0.0
instance Eq VersionOracleDatum where
  VersionOracleDatum vO c == VersionOracleDatum vO' c' = vO == vO' && c == c'

-- | Configuration of the versioning system.  Contains currency symbol of
-- VersionOraclePolicy tokens.  Required to identify versioning tokens that can
-- be trusted.
--
-- @since v5.0.0
newtype VersionOracleConfig = VersionOracleConfig
  { versionOracleCurrencySymbol :: CurrencySymbol
  -- ^ @since v5.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracleConfig where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleConfig {versionOracleCurrencySymbol} =
    toBuiltinData versionOracleCurrencySymbol

-- | @since v5.0.0
instance FromData VersionOracleConfig where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracleConfig <$> fromBuiltinData x

-- | @since v5.0.0
instance UnsafeFromData VersionOracleConfig where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = VersionOracleConfig . unsafeFromBuiltinData

-- | Token name for versioning tokens.  Must match definition in off-chain
-- | module.
{-# INLINEABLE versionOracleTokenName #-}
versionOracleTokenName :: TokenName
versionOracleTokenName = TokenName "Version oracle"

-- | Redeemer for the versioning oracle minting policy that instructs the script
-- whether to mint or burn versioning tokens.
--
-- @since v5.0.0
data VersionOraclePolicyRedeemer
  = -- | Mint versioning tokens from init tokens.  Used during sidechain
    -- initialization.
    -- @since v6.0.0
    InitializeVersionOracle VersionOracle ScriptHash
  | -- | Mint a new versioning token ensuring it contains correct datum and
    -- reference script.
    -- @since v5.0.0
    MintVersionOracle VersionOracle ScriptHash
  | -- | Burn existing versioning token.
    -- @since v5.0.0
    BurnVersionOracle VersionOracle

PlutusTx.makeIsDataIndexed
  ''VersionOraclePolicyRedeemer
  [ ('InitializeVersionOracle, 0)
  , ('MintVersionOracle, 1)
  , ('BurnVersionOracle, 2)
  ]

-- | Manages minting and burning of versioning tokens.  (Note that these are
-- ordinary tokens, not NFTs.)  No restrictions are placed on minting initial
-- versioning tokens during sidechain initialization, other than the usual
-- requirement of burning a genesis UTxO.
--
-- OnChain error descriptions:
--
--   ERROR-VERSION-POLICY-01: Transaction should burn exactly one init token.
--
--   ERROR-VERSION-POLICY-02: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-03: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-04: Script to be invalidated should be present in
--     exactly one transaction input.
--
--   ERROR-VERSION-POLICY-05: Transaction should be signed by the governance.
--
--   ERROR-VERSION-POLICY-06: Transaction should burn all versioning tokens in
--     the input.
--
--   ERROR-VERSION-POLICY-07: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-08: Script can only be used for Minting purpose.
--
--   ERROR-VERSION-POLICY-09: Transaction should be signed by the governance.
--
--   ERROR-VERSION-POLICY-10: Script purpose is not Minting.
mkVersionOraclePolicy ::
  Unsafe.TxOutRef ->
  Address ->
  VersionOraclePolicyRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkVersionOraclePolicy genesisUtxo validatorAddress redeemer ctx =
  case redeemer of
    InitializeVersionOracle versionOracle scriptHash ->
      traceIfFalse "ERROR-VERSION-POLICY-01" isGenesisUtxoUsed
        && fromSingleton "ERROR-VERSION-POLICY-02" (verifyOut versionOracle scriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-03" mintOneVersionToken
    MintVersionOracle newVersionOracle newScriptHash ->
      fromSingleton "ERROR-VERSION-POLICY-04" (verifyOut newVersionOracle newScriptHash)
        && traceIfFalse "ERROR-VERSION-POLICY-05" signedByGovernanceAuthority
        && traceIfFalse "ERROR-VERSION-POLICY-06" mintOneVersionToken
    BurnVersionOracle oldVersion ->
      fromSingleton "ERROR-VERSION-POLICY-07" (versionInputPresent oldVersion)
        && traceIfFalse "ERROR-VERSION-POLICY-08" versionOutputAbsent
        && traceIfFalse "ERROR-VERSION-POLICY-09" signedByGovernanceAuthority
  where
    isGenesisUtxoUsed :: Bool
    isGenesisUtxoUsed =
      genesisUtxo `elem` map Unsafe.txInInfoOutRef (Unsafe.txInfoInputs $ Unsafe.scriptContextTxInfo ctx)

    txInfo = Unsafe.scriptContextTxInfo ctx

    currSym =
      fromJust "ERROR-ORACLE-POLICY-10"
        $ Unsafe.decode
        <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx)

    mintOneVersionToken :: Bool
    mintOneVersionToken =
      valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) currSym versionOracleTokenName == 1

    -- Check that this transaction mints a token with correct datum and script
    -- hash.
    verifyOut :: VersionOracle -> ScriptHash -> [Bool]
    verifyOut versionOracle scriptHash =
      [ True
      | (TxOut address value (OutputDatum (Datum datum)) (Just scriptHash')) <-
          Unsafe.decode <$> Unsafe.txInfoOutputs txInfo
      , address == validatorAddress
      , Just (VersionOracleDatum versionOracle' _) <-
          [PlutusTx.fromBuiltinData datum]
      , -- Check that output contains correct version oracle and a reference
      -- script with correct hash.
      versionOracle' == versionOracle
      , scriptHash' == scriptHash
      , -- Check that datum is attached to a single version token.
      valueOf value currSym versionOracleTokenName == 1
      ]

    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      approvedByGovernance (VersionOracleConfig currSym) ctx

    -- Check that the script version to be invalidated is present in exactly
    -- one transaction input.
    versionInputPresent :: VersionOracle -> [Bool]
    versionInputPresent oldVersion =
      [ True
      | TxInInfo _ (TxOut address value (OutputDatum (Datum datum)) _) <-
          Unsafe.decode <$> Unsafe.txInfoInputs txInfo
      , address == validatorAddress
      , Just (VersionOracleDatum oldVersion' _) <-
          [PlutusTx.fromBuiltinData datum]
      , -- Check we are burning correct token.
      oldVersion' == oldVersion
      , -- Check there is exactly one token in the input that we're going to
      -- burn.
      valueOf value currSym versionOracleTokenName == 1
      ]

    -- Check that the script version to be invalidated is absent from
    -- transaction outputs.
    versionOutputAbsent :: Bool
    versionOutputAbsent =
      null
        [ ()
        | txOut <- Unsafe.txInfoOutputs txInfo
        , valueOf (Unsafe.decode $ Unsafe.txOutValue txOut) currSym versionOracleTokenName > 0
        ]

{-# INLINEABLE mkVersionOraclePolicyUntyped #-}
mkVersionOraclePolicyUntyped ::
  -- | Genesis Utxo
  BuiltinData ->
  -- | Validator address
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  BuiltinUnit
mkVersionOraclePolicyUntyped genesisUtxo validatorAddress redeemer ctx =
  check
    $ mkVersionOraclePolicy
      (Unsafe.wrap genesisUtxo)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableVersionOraclePolicy ::
  SerialisedScript
serialisableVersionOraclePolicy =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkVersionOraclePolicyUntyped||])

-- | Stores VersionOraclePolicy UTxOs, acting both as an oracle of available
-- scripts as well as a script caching system.  UTxOs on the script are managed
-- by governance authority, with VersionOraclePolicy ensuring that tokens are
-- minted and burned correctly.
{-# INLINEABLE mkVersionOracleValidator #-}
mkVersionOracleValidator ::
  BuiltinData ->
  VersionOracleDatum ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  Bool
mkVersionOracleValidator
  _genesisUtxo
  (VersionOracleDatum versionOracle currencySymbol)
  versionOracle'
  ctx =
    traceIfFalse "ERROR-VERSION-ORACLE-01" signedByGovernanceAuthority
      && traceIfFalse "ERROR-VERSION-ORACLE-02" versionOraclesMatch
      && traceIfFalse "ERROR-VERSION-ORACLE-03" versionOutputAbsent
      && traceIfFalse "ERROR-VERSION-ORACLE-04" isSpending
    where
      isSpending = isJust $ Unsafe.getSpending $ Unsafe.scriptContextPurpose ctx

      txInfo = Unsafe.scriptContextTxInfo ctx -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority =
        approvedByGovernance (VersionOracleConfig currencySymbol) ctx

      -- Check that version oracle in the datum matches the redeemer
      versionOraclesMatch = versionOracle == versionOracle'

      ownAddress = case Unsafe.findOwnInput ctx of
        Just txIn -> Unsafe.txOutAddress $ Unsafe.txInInfoResolved txIn
        _ -> traceError "ERROR-VERSION-ORACLE-05"

      -- Check that transaction doesn't output any version tokens.
      versionOutputAbsent =
        all
          (\txOut -> (Unsafe.txOutAddress txOut == ownAddress) || valueOf (Unsafe.decode $ Unsafe.txOutValue txOut) currencySymbol versionOracleTokenName <= 0)
          (Unsafe.txInfoOutputs txInfo)

{-# INLINEABLE mkVersionOracleValidatorUntyped #-}
mkVersionOracleValidatorUntyped ::
  -- | Genesis UTXO
  BuiltinData ->
  -- | Datum
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  BuiltinUnit
mkVersionOracleValidatorUntyped genesisUtxo datum redeemer ctx =
  check
    $ mkVersionOracleValidator
      genesisUtxo
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableVersionOracleValidator ::
  SerialisedScript
serialisableVersionOracleValidator =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkVersionOracleValidatorUntyped||])

-- | Searches for a specified validator script passed as a reference input.
-- Note that if requested script ID corresponds to a minting policy this
-- function will return a result anyway without reporting any errors.
{-# INLINEABLE getVersionedValidatorAddress #-}
getVersionedValidatorAddress ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  Address
getVersionedValidatorAddress voConfig vo =
  scriptHashAddress . ScriptHash . getVersionedScriptHash voConfig vo

{-# INLINEABLE getVersionedValidatorAddressUnsafe #-}
getVersionedValidatorAddressUnsafe ::
  VersionOracleConfig ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  Address
getVersionedValidatorAddressUnsafe voConfig vo =
  scriptHashAddress . ScriptHash . getVersionedScriptHashUnsafe voConfig vo

-- | Searches for a specified minting policy passed as a reference input.  Note
-- that if requested script ID corresponds to a validator this function will
-- return a result anyway without reporting any errors.
{-# INLINEABLE getVersionedCurrencySymbol #-}
getVersionedCurrencySymbol ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  CurrencySymbol
getVersionedCurrencySymbol voConfig vo sc =
  CurrencySymbol (getVersionedScriptHash voConfig vo sc)

{-# INLINEABLE getVersionedCurrencySymbolUnsafe #-}
getVersionedCurrencySymbolUnsafe ::
  VersionOracleConfig ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  CurrencySymbol
getVersionedCurrencySymbolUnsafe voConfig vo sc =
  CurrencySymbol (getVersionedScriptHashUnsafe voConfig vo sc)

-- | Searches script context for a reference input containing specified script
-- as a reference input.  Said reference input must come from
-- VersionOracleValidator script address.  Returns script hash if requested
-- version exists, throws error otherwise.  Note that this function is a worker
-- for 'getVersionedCurrencySymbol' and 'getVersionedValidatorAddress', and it
-- is therefore the callers responsibility to correctly interpret the result as
-- either a validator or a minting policy.
{-# INLINEABLE getVersionedScriptHash #-}
getVersionedScriptHash ::
  VersionOracleConfig ->
  VersionOracle ->
  ScriptContext ->
  BuiltinByteString
getVersionedScriptHash
  VersionOracleConfig {versionOracleCurrencySymbol}
  versionOracle
  (ScriptContext txInfo _) =
    fromSingleton "ERROR-VERSION-CURRENCY-01"
      $ [ hash
        | -- Lookup reference input that:
        TxInInfo
          _
          ( TxOut
              _
              value
              (OutputDatum (Datum datum))
              (Just (ScriptHash hash))
            ) <-
          txInfoReferenceInputs txInfo ++ txInfoInputs txInfo
        , -- 1. Contains datum that matches desired version and scriptId.
        Just (VersionOracleDatum versionOracle' _) <- [PlutusTx.fromBuiltinData datum]
        , versionOracle' == versionOracle
        , -- 2. Contains exactly one VersionOraclePolicy token.
        valueOf value versionOracleCurrencySymbol versionOracleTokenName == 1
        ]

{-# INLINEABLE getVersionedScriptHashUnsafe #-}
getVersionedScriptHashUnsafe ::
  VersionOracleConfig ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  BuiltinByteString
getVersionedScriptHashUnsafe
  VersionOracleConfig {versionOracleCurrencySymbol}
  versionOracle
  sc =
    fromSingleton "ERROR-VERSION-CURRENCY-01"
      $ [ hash
        | -- Lookup reference input that:
        TxInInfo
          _
          ( TxOut
              _
              value
              (OutputDatum (Datum datum))
              (Just (ScriptHash hash))
            ) <-
          PlutusTx.unsafeFromBuiltinData
            . Unsafe.unTxInInfo
            <$> ((\x -> Unsafe.txInfoReferenceInputs x ++ Unsafe.txInfoInputs x) . Unsafe.scriptContextTxInfo $ sc)
        , -- 1. Contains datum that matches desired version and scriptId.
        Just (VersionOracleDatum versionOracle' _) <- [PlutusTx.fromBuiltinData datum]
        , versionOracle' == versionOracle
        , -- 2. Contains exactly one VersionOraclePolicy token.
        valueOf value versionOracleCurrencySymbol versionOracleTokenName == 1
        ]

-- | Check whether a given transaction is approved by sidechain governance.  The
-- actual check is delegated to a governance minting policy stored in the
-- versioning system.  Caller specifies the requested governance version.  The
-- transaction must mint at least one token of the governance minting policy to
-- signify transaction approval.
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance ::
  VersionOracleConfig ->
  Unsafe.ScriptContext ->
  Bool
approvedByGovernance voc ctx =
  case ofGovernanceCs of
    Just [(_, amount)] | amount > 0 -> True -- must mint at least one token, any name
    _ -> False
  where
    ofGovernanceCs :: Maybe [(TokenName, Integer)]
    ofGovernanceCs =
      fmap toList . lookup governanceTokenCurrencySymbol . getValue $ minted

    governanceTokenCurrencySymbol :: CurrencySymbol
    governanceTokenCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        voc
        (VersionOracle {scriptId = ScriptId.governancePolicyId})
        ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx
