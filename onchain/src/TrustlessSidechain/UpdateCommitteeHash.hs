{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TrustlessSidechain.UpdateCommitteeHash (
  initCommitteeOracleTn,
  initCommitteeOracleMintAmount,
  mkCommitteeOraclePolicy,
  mkUpdateCommitteeHashValidator,
  serialisableCommitteeOraclePolicy,
  serialisableCommitteeHashValidator,
) where

import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  LedgerBytes (LedgerBytes),
  OutputDatum (OutputDatum),
  Script,
  TokenName (TokenName),
  Value (getValue),
  fromCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  InitTokenAssetClass,
  SidechainParams,
  UpdateCommitteeDatum,
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    newAggregateCommitteePubKeys,
    previousMerkleRoot,
    sidechainEpoch,
    sidechainParams,
    validatorHash
  ),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (
  oneTokenBurned,
 )
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  committeeCertificateVerificationPolicyId,
  committeeOraclePolicyId,
  getVersionedCurrencySymbolUnsafe,
  merkleRootTokenPolicyId,
 )

-- * Updating the committee hash

-- | 'serialiseUchm' serialises an 'UpdateCommitteeHashMessage' via converting
-- to the Plutus data representation, then encoding it to cbor via the builtin.
serialiseUchm :: ToData aggregatePubKeys => UpdateCommitteeHashMessage aggregatePubKeys -> BuiltinByteString
serialiseUchm = Builtins.serialiseData . IsData.toBuiltinData

-- | 'initCommitteeOracleTn'  is the token name of the NFT which identifies
-- the utxo which contains the committee hash. We use an empty bytestring for
-- this because the name really doesn't matter, so we mighaswell save a few
-- bytes by giving it the empty name.
{-# INLINEABLE initCommitteeOracleTn #-}
initCommitteeOracleTn :: TokenName
initCommitteeOracleTn = TokenName Builtins.emptyByteString

-- | 'initCommitteeOracleMintAmount' is the amount of the currency to mint which
-- is 1.
{-# INLINEABLE initCommitteeOracleMintAmount #-}
initCommitteeOracleMintAmount :: Integer
initCommitteeOracleMintAmount = 1

-- | 'mkUpdateCommitteeHashValidator' is the on-chain validator.
-- See the specification for what is verified, but as a summary: we verify that
-- the transaction corresponds to the signed update committee message in a
-- reasonable sense.
--
-- OnChain error descriptions:
--
-- ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-01: invalid committee output
--
-- ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-02: tx doesn't reference previous merkle
-- root
--
-- ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-03: output address for utxo containing
-- committeeOracleCurrencySymbol must be a script address
--
-- ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-04: tx not signed by committee
--
-- ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-05: sidechain epoch is not strictly
-- increasing
{-# INLINEABLE mkUpdateCommitteeHashValidator #-}
mkUpdateCommitteeHashValidator ::
  SidechainParams ->
  VersionOracleConfig ->
  UpdateCommitteeDatum BuiltinData ->
  Unsafe.UpdateCommitteeHashRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkUpdateCommitteeHashValidator sp versioningConfig dat red ctx =
  traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-01" committeeOutputIsValid
    && traceIfFalse
      "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-02"
      referencesPreviousMerkleRoot
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    committeeOracleCurrencySymbol :: CurrencySymbol
    committeeOracleCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeOraclePolicyId})
        ctx

    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
    committeeCertificateVerificationCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeCertificateVerificationPolicyId})
        ctx

    mptRootTokenCurrencySymbol :: CurrencySymbol
    mptRootTokenCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version = 1, scriptId = merkleRootTokenPolicyId})
        ctx

    committeeOutputIsValid :: Bool
    committeeOutputIsValid =
      let go :: [Unsafe.TxOut] -> Bool
          go [] = False
          go (o : os)
            | -- recall that 'committeeOracleCurrencySymbol' should be
              -- an NFT, so  (> 0) ==> exactly one.
              Value.valueOf (Unsafe.decode $ Unsafe.txOutValue o) committeeOracleCurrencySymbol initCommitteeOracleTn > 0
              , OutputDatum d <- Unsafe.decode $ Unsafe.txOutDatum o
              , ucd :: UpdateCommitteeDatum BuiltinData <- PlutusTx.unsafeFromBuiltinData (getDatum d) =
              -- Note that we build the @msg@ that we check is signed
              -- with the data in this transaction directly... so in a sense,
              -- checking if this message is signed is checking if the
              -- transaction corresponds to the message

              let validatorHash' =
                    case Unsafe.getScriptCredential $ Unsafe.addressCredential $ Unsafe.txOutAddress o of
                      Just vh -> Unsafe.decode vh
                      _ -> traceError "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-03"

                  msg =
                    UpdateCommitteeHashMessage
                      { sidechainParams = sp
                      , newAggregateCommitteePubKeys = get @"aggregateCommitteePubKeys" ucd
                      , previousMerkleRoot = Unsafe.decode <$> Unsafe.previousMerkleRoot red
                      , sidechainEpoch = get @"sidechainEpoch" ucd
                      , validatorHash = validatorHash'
                      }
               in traceIfFalse
                    "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-04"
                    ( Value.valueOf
                        (Unsafe.decode $ Unsafe.txInfoMint info)
                        committeeCertificateVerificationCurrencySymbol
                        (TokenName (Builtins.blake2b_256 (serialiseUchm msg)))
                        > 0
                    )
                    && traceIfFalse
                      "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-05"
                      (get @"sidechainEpoch" dat < get @"sidechainEpoch" ucd)
            | otherwise = go os
       in go (Unsafe.txInfoOutputs info)

    referencesPreviousMerkleRoot :: Bool
    referencesPreviousMerkleRoot =
      -- Either we want to reference the previous merkle root or we don't (note
      -- that this is signed by the committee -- this is where the security
      -- guarantees come from).
      -- If we do want to reference the previous merkle root, we need to verify
      -- that there exists at least one input with a nonzero amount of the
      -- merkle root tokens.
      case Unsafe.previousMerkleRoot red of
        Nothing -> True
        Just unsafeLedgerBytes ->
          -- TODO replace with any
          let LedgerBytes tn = Unsafe.decode unsafeLedgerBytes
              go :: [Unsafe.TxInInfo] -> Bool
              go (txInInfo : rest) =
                ( (Value.valueOf (Unsafe.decode $ Unsafe.txOutValue (Unsafe.txInInfoResolved txInInfo)) mptRootTokenCurrencySymbol (TokenName tn) > 0)
                    || go rest
                )
              go [] = False
           in go (Unsafe.txInfoReferenceInputs info)

-- * Initializing the committee hash

-- | 'mkCommitteeOraclePolicy' is the minting policy for the NFT which identifies
-- the committee hash.
--
-- OnChain error descriptions:
--
-- ERROR-UPDATE-COMMITTEE-HASH-POLICY-01: The transaction does not spend init
-- token.
--
-- ERROR-UPDATE-COMMITTEE-HASH-POLICY-02: wrong amount minted
-- increasing
{-# INLINEABLE mkCommitteeOraclePolicy #-}
mkCommitteeOraclePolicy :: InitTokenAssetClass -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkCommitteeOraclePolicy itac _red ctx =
  traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-POLICY-01" initTokenBurned
    && traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-POLICY-02" checkMintedAmount
  where
    mint :: Value
    mint = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

    -- Assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount =
      case fmap AssocMap.toList $ AssocMap.lookup (Unsafe.ownCurrencySymbol ctx) $ getValue mint of
        Just [(tn', amt)] -> tn' == initCommitteeOracleTn && amt == initCommitteeOracleMintAmount
        _ -> False

mkCommitteeOraclePolicyUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCommitteeOraclePolicyUntyped itac red ctx =
  check $
    mkCommitteeOraclePolicy
      (PlutusTx.unsafeFromBuiltinData itac)
      red
      (Unsafe.wrap ctx)

serialisableCommitteeOraclePolicy :: Script
serialisableCommitteeOraclePolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCommitteeOraclePolicyUntyped||])

mkCommitteeHashValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkCommitteeHashValidatorUntyped uch versioningConfig dat red ctx =
  check $
    mkUpdateCommitteeHashValidator
      (PlutusTx.unsafeFromBuiltinData uch)
      (PlutusTx.unsafeFromBuiltinData versioningConfig)
      (PlutusTx.unsafeFromBuiltinData dat)
      (Unsafe.wrap red)
      (Unsafe.wrap ctx)

serialisableCommitteeHashValidator :: Script
serialisableCommitteeHashValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkCommitteeHashValidatorUntyped||])
