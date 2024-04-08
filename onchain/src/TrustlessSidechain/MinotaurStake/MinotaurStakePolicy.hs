{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'TrustlessSidechain.Versioning' module implements script versioning system.
-- It provides VersionOraclePolicy for minting tokens that store versioned
-- scripts, as well as VersionedOracleValidator script for storing the
-- versioning tokens.  Each versioning token stores a reference script and a
-- datum that identifies the script and its version.
module TrustlessSidechain.MinotaurStake.MinotaurStakePolicy (serialisableMinotaurStakePolicy) where

import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf)
import Plutus.V2.Ledger.Api (
  Address,
  Datum (Datum),
  OutputDatum (OutputDatum),
  PubKeyHash,
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TokenName (TokenName),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  fromCompiledCode,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
 )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  fromSingleton,
  mkUntypedMintingPolicy,
 )

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
data MinotaurStakeDatum = MinotaurStakeDatum
  { -- | reward address on partner chain
    -- @since Unreleased
    partnerChainRewardAddress :: BuiltinByteString
  , -- | pub key hash of stake key of the delegator
    -- @since Unreleased
    stakePubKeyHash :: PubKeyHash
  , -- | ID of the stake pool
    -- @since Unreleased
    stakePoolId :: BuiltinByteString
  , -- | currency symbol of the minotaur stake delegation token
    -- @since Unreleased
    stakeCurrencySymbol :: Currencysymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since Unreleased
instance ToData MinotaurStakeDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MinotaurStakeDatum {..}) =
    productToData4
      partnerChainRewardAddress
      stakePubKeyHash
      stakePoolId
      stakeCurrencySymbol

-- | @since Unreleased
instance FromData MinotaurStakeDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 MinotaurStakeDatum

-- | @since Unreleased
instance UnsafeFromData MinotaurStakeDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 MinotaurStakeDatum

-- | module.
minotaurStakeTokenName :: TokenName
minotaurStakeTokenName = TokenName "Minotaur Stake"

-- | MinotaurStakeRedeemer is used to mint and burn MinotaurStake tokens.
--
-- @since v5.0.0
data MinotaurStakeRedeemer
  = -- | Mint a new MinotaurStake token
    -- @since Unreleased
    MintMinotaurStake
  | -- | Burn a MinotaurStake token
    -- @since Unreleased
    BurnMinotaurStake

PlutusTx.makeIsDataIndexed
  ''MinotaurStakeRedeemer
  [ ('MintMinotaurStake, 0)
  , ('BurnMinotaurStake, 1)
  ]

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
