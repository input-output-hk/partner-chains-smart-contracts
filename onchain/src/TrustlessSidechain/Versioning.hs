{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
  VersionOracle (..),
  VersionOracleDatum (..),
  VersionOracleConfig (..),
  VersionOraclePolicyRedeemer,
  -- | Script IDs
  fuelMintingPolicyId,
  merkleRootTokenPolicyId,
  merkleRootTokenValidatorId,
  committeeCandidateValidatorId,
  candidatePermissionPolicyId,
  committeeHashValidatorId,
  dsKeyPolicyId,
  dsConfPolicyId,
  dsConfValidatorId,
  dsInsertValidatorId,
  checkpointValidatorId,
  checkpointPolicyId,
  fuelBurningPolicyId,
  versionOraclePolicyId,
  versionOracleValidatorId,
  fuelProxyPolicyId,
  committeeCertificateVerificationPolicyId,
  committeeOraclePolicyId,
  committeePlainEcdsaSecp256k1ATMSPolicyId,
  committeePlainSchnorrSecp256k1ATMSPolicyId,
  dParameterPolicyId,
  dParameterValidatorId,
  permissionedCandidatesPolicyId,
  permissionedCandidatesValidatorId,
  scriptCacheId,
  initTokensPolicyId,
  reserveValidatorId,
  reserveAuthPolicyId,
  illiquidCirculationSupplyValidatorId,
  illiquidCirculationSupplyWithdrawalPolicyId,
  governancePolicyId,
) where

import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (ScriptContext),
  ScriptHash (ScriptHash),
  TokenName (TokenName),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  ValidatorHash (ValidatorHash),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (txInfoReferenceInputs)
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  InitTokenAssetClass,
  SidechainParams,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (
  fromSingleton,
  oneTokenBurned,
 )

-- Note [Versioned script identifiers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Each versioned script has an assigned identifier.  On-chain code must permit
-- addition of new versioned scripts without any changes to existing code, i.e.
-- minting policies and validator hashes must remain stable when new script is
-- being added.  For this purpose we use integers as script identifiers, with
-- each known script being assigned its own number.  In the off-chain code we
-- decode these integers into an ADT using hand-written ToData/FromData
-- instances since it's not a problem to have changes there and it is safer and
-- more descriptive to operate on an ADT.  Integers defined in on-chain code
-- need to match instances in the off-chain portion of the code.

-- See Note [Versioned script identifiers]
fuelMintingPolicyId
  , merkleRootTokenPolicyId
  , merkleRootTokenValidatorId
  , committeeCandidateValidatorId
  , candidatePermissionPolicyId
  , committeeHashValidatorId
  , dsKeyPolicyId
  , dsConfPolicyId
  , dsConfValidatorId
  , dsInsertValidatorId
  , checkpointValidatorId
  , checkpointPolicyId
  , fuelBurningPolicyId
  , versionOraclePolicyId
  , versionOracleValidatorId
  , fuelProxyPolicyId
  , committeeCertificateVerificationPolicyId
  , committeeOraclePolicyId
  , committeePlainEcdsaSecp256k1ATMSPolicyId
  , committeePlainSchnorrSecp256k1ATMSPolicyId
  , dParameterPolicyId
  , dParameterValidatorId
  , permissionedCandidatesPolicyId
  , permissionedCandidatesValidatorId
  , scriptCacheId
  , initTokensPolicyId
  , reserveValidatorId
  , reserveAuthPolicyId
  , illiquidCirculationSupplyValidatorId
  , illiquidCirculationSupplyWithdrawalPolicyId
  , governancePolicyId ::
    Integer
fuelMintingPolicyId = 0
merkleRootTokenPolicyId = 1
merkleRootTokenValidatorId = 2
committeeCandidateValidatorId = 3
candidatePermissionPolicyId = 4
committeeHashValidatorId = 7
dsKeyPolicyId = 8
dsConfPolicyId = 9
dsConfValidatorId = 10
dsInsertValidatorId = 11
checkpointValidatorId = 12
checkpointPolicyId = 13
fuelBurningPolicyId = 14
versionOraclePolicyId = 15
versionOracleValidatorId = 16
fuelProxyPolicyId = 17
committeeCertificateVerificationPolicyId = 18
committeeOraclePolicyId = 19
committeePlainEcdsaSecp256k1ATMSPolicyId = 20
committeePlainSchnorrSecp256k1ATMSPolicyId = 21
dParameterPolicyId = 22
dParameterValidatorId = 23
permissionedCandidatesPolicyId = 24
permissionedCandidatesValidatorId = 25
scriptCacheId = 26
initTokensPolicyId = 27
reserveValidatorId = 28
reserveAuthPolicyId = 29
illiquidCirculationSupplyValidatorId = 30
illiquidCirculationSupplyWithdrawalPolicyId = 31
governancePolicyId = 32

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
--
-- @since v5.0.0
data VersionOracle = VersionOracle
  { -- | Version of the script.
    -- @since v5.0.0
    version :: Integer
  , -- | Unique identifier of the validator.
    -- @since v5.0.0
    scriptId :: Integer
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracle where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionOracle {..}) =
    productToData2 version scriptId

-- | @since v5.0.0
instance FromData VersionOracle where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 VersionOracle

-- | @since v5.0.0
instance UnsafeFromData VersionOracle where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 VersionOracle

-- | @since v5.0.0
instance Eq VersionOracle where
  VersionOracle v s == VersionOracle v' s' = v == v' && s == s'

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
data VersionOracleDatum = VersionOracleDatum
  { -- | VersionOracle which identifies the script.
    -- @since v6.0.0
    versionOracle :: VersionOracle
  , -- | Currency Symbol of the VersioningOraclePolicy tokens.
    -- @since v6.0.0
    currencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v6.0.0
instance ToData VersionOracleDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionOracleDatum {..}) =
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
  { -- | @since v5.0.0
    versionOracleCurrencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracleConfig where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionOracleConfig {..}) =
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
--   ERROR-VERSION-POLICY-04: Transaction should be signed by the governance.
--
--   ERROR-VERSION-POLICY-05: Script to be invalidated should be present in
--     exactly one transaction input.
--
--   ERROR-VERSION-POLICY-06: Transaction should burn all versioning tokens in
--     the input.
--
--   ERROR-VERSION-POLICY-07: Transaction should attach datum and reference
--     script to output containing one versioning token.
--
--   ERROR-VERSION-POLICY-08: Script can only be used for Minting purpose.
mkVersionOraclePolicy ::
  SidechainParams ->
  InitTokenAssetClass ->
  Address ->
  VersionOraclePolicyRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkVersionOraclePolicy
  _
  itac
  validatorAddress
  (InitializeVersionOracle versionOracle scriptHash)
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let txInfo = Unsafe.scriptContextTxInfo ctx

          mintOneVersionToken :: Bool
          mintOneVersionToken =
            valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) currSym versionOracleTokenName == 1
          -- Did we burn exactly one init token?  Again, we prevent initializing
          -- multiple scripts in a single transaction
          initTokenBurned :: Bool
          initTokenBurned =
            oneTokenBurned
              (Unsafe.decode $ Unsafe.txInfoMint txInfo)
              (get @"initTokenCurrencySymbol" itac)
              (get @"initTokenName" itac)

          -- Check that this transaction mints a token with correct datum and script
          -- hash.
          verifyOut :: [Bool]
          verifyOut =
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
       in traceIfFalse "ERROR-VERSION-POLICY-01" initTokenBurned
            && fromSingleton "ERROR-VERSION-POLICY-02" verifyOut
            && traceIfFalse "ERROR-VERSION-POLICY-03" mintOneVersionToken
mkVersionOraclePolicy
  sp
  _
  validatorAddress
  (MintVersionOracle newVersionOracle newScriptHash)
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let txInfo = Unsafe.scriptContextTxInfo ctx

          mintOneVersionToken :: Bool
          mintOneVersionToken =
            valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) currSym versionOracleTokenName == 1

          -- Check that transaction was approved by governance authority
          signedByGovernanceAuthority :: Bool
          signedByGovernanceAuthority =
            txInfo `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

          -- Check that this transaction mints a token with correct datum and script
          -- hash.
          verifyOut :: [Bool]
          verifyOut =
            [ True
            | (TxOut address value (OutputDatum (Datum datum)) (Just scriptHash')) <-
                Unsafe.decode <$> Unsafe.txInfoOutputs txInfo
            , address == validatorAddress
            , Just (VersionOracleDatum versionOracle' _) <-
                [PlutusTx.fromBuiltinData datum]
            , -- Check that output contains correct version oracle and a reference
            -- script with correct hash.
            versionOracle' == newVersionOracle
            , scriptHash' == newScriptHash
            , -- Check that datum is attached to a single version token.
            valueOf value currSym versionOracleTokenName == 1
            ]
       in fromSingleton "ERROR-VERSION-POLICY-04" verifyOut
            && traceIfFalse "ERROR-VERSION-POLICY-05" signedByGovernanceAuthority
            && traceIfFalse "ERROR-VERSION-POLICY-06" mintOneVersionToken
mkVersionOraclePolicy
  sp
  _
  validatorAddress
  (BurnVersionOracle oldVersion)
  ctx
    | Just currSym <-
        Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let txInfo = Unsafe.scriptContextTxInfo ctx
          -- Check that transaction was approved by governance authority
          signedByGovernanceAuthority :: Bool
          signedByGovernanceAuthority =
            txInfo `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

          -- Check that the script version to be invalidated is present in exactly
          -- one transaction input.
          versionInputPresent :: [Bool]
          versionInputPresent =
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
       in fromSingleton "ERROR-VERSION-POLICY-07" versionInputPresent
            && traceIfFalse "ERROR-VERSION-POLICY-08" versionOutputAbsent
            && traceIfFalse "ERROR-VERSION-POLICY-09" signedByGovernanceAuthority
mkVersionOraclePolicy _ _ _ _ _ =
  trace "ERROR-ORACLE-POLICY-10" False

{-# INLINEABLE mkVersionOraclePolicyUntyped #-}
mkVersionOraclePolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | InitToken asset class
  BuiltinData ->
  -- | Validator address
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkVersionOraclePolicyUntyped sp itac validatorAddress redeemer ctx =
  check $
    mkVersionOraclePolicy
      (unsafeFromBuiltinData sp)
      (unsafeFromBuiltinData itac)
      (unsafeFromBuiltinData validatorAddress)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableVersionOraclePolicy ::
  Script
serialisableVersionOraclePolicy =
  fromCompiledCode
    $$(PlutusTx.compile [||mkVersionOraclePolicyUntyped||])

-- | Stores VersionOraclePolicy UTxOs, acting both as an oracle of available
-- scripts as well as a script caching system.  UTxOs on the script are managed
-- by governance authority, with VersionOraclePolicy ensuring that tokens are
-- minted and burned correctly.
{-# INLINEABLE mkVersionOracleValidator #-}
mkVersionOracleValidator ::
  SidechainParams ->
  VersionOracleDatum ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  Bool
mkVersionOracleValidator
  sp
  (VersionOracleDatum versionOracle currencySymbol)
  versionOracle'
  ctx
    | isJust $ Unsafe.getSpending $ Unsafe.scriptContextPurpose ctx =
      traceIfFalse "ERROR-VERSION-ORACLE-01" signedByGovernanceAuthority
        && traceIfFalse "ERROR-VERSION-ORACLE-02" versionOraclesMatch
        && traceIfFalse "ERROR-VERSION-ORACLE-03" versionOutputAbsent
    where
      txInfo = Unsafe.scriptContextTxInfo ctx
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority =
        txInfo `Governance.isApprovedByUnsafe` get @"governanceAuthority" sp

      -- Check that version oracle in the datum matches the redeemer
      versionOraclesMatch = versionOracle == versionOracle'

      -- Check that transaction doesn't output any version tokens.
      versionOutputAbsent =
        -- TODO replace with all after Plutus version upgrade
        null
          [ ()
          | txOut <- Unsafe.txInfoOutputs txInfo
          , valueOf (Unsafe.decode $ Unsafe.txOutValue txOut) currencySymbol versionOracleTokenName > 0
          ]
mkVersionOracleValidator _ _ _ _ =
  trace "ERROR-VERSION-ORACLE-04" False

{-# INLINEABLE mkVersionOracleValidatorUntyped #-}
mkVersionOracleValidatorUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Datum
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkVersionOracleValidatorUntyped params datum redeemer ctx =
  check $
    mkVersionOracleValidator
      (PlutusTx.unsafeFromBuiltinData params)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableVersionOracleValidator ::
  Script
serialisableVersionOracleValidator =
  fromCompiledCode
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
  scriptHashAddress . ValidatorHash . getVersionedScriptHash voConfig vo

{-# INLINEABLE getVersionedValidatorAddressUnsafe #-}
getVersionedValidatorAddressUnsafe ::
  VersionOracleConfig ->
  VersionOracle ->
  Unsafe.ScriptContext ->
  Address
getVersionedValidatorAddressUnsafe voConfig vo =
  scriptHashAddress . ValidatorHash . getVersionedScriptHashUnsafe voConfig vo

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
  (VersionOracleConfig {..})
  versionOracle
  (ScriptContext txInfo _) =
    fromSingleton "ERROR-VERSION-CURRENCY-01" $
      [ hash
      | -- Lookup reference input that:
      TxInInfo
        _
        ( TxOut
            _
            value
            (OutputDatum (Datum datum))
            (Just (ScriptHash hash))
          ) <-
        txInfoReferenceInputs txInfo
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
  (VersionOracleConfig {..})
  versionOracle
  sc =
    fromSingleton "ERROR-VERSION-CURRENCY-01" $
      [ hash
      | -- Lookup reference input that:
      TxInInfo
        _
        ( TxOut
            _
            value
            (OutputDatum (Datum datum))
            (Just (ScriptHash hash))
          ) <-
        PlutusTx.unsafeFromBuiltinData . Unsafe.unTxInInfo
          <$> (Unsafe.txInfoReferenceInputs . Unsafe.scriptContextTxInfo $ sc)
      , -- 1. Contains datum that matches desired version and scriptId.
      Just (VersionOracleDatum versionOracle' _) <- [PlutusTx.fromBuiltinData datum]
      , versionOracle' == versionOracle
      , -- 2. Contains exactly one VersionOraclePolicy token.
      valueOf value versionOracleCurrencySymbol versionOracleTokenName == 1
      ]
