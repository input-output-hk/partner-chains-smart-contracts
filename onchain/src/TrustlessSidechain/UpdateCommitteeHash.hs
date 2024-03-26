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
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (getDatum),
  LedgerBytes (LedgerBytes),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  Value (getValue),
  addressCredential,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
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
  UpdateCommitteeHashRedeemer,
 )
import TrustlessSidechain.Utils (
  mkUntypedMintingPolicy,
  mkUntypedValidator,
  oneTokenBurned,
 )
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  committeeCertificateVerificationPolicyId,
  committeeOraclePolicyId,
  getVersionedCurrencySymbol,
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
  UpdateCommitteeHashRedeemer ->
  ScriptContext ->
  Bool
mkUpdateCommitteeHashValidator sp versioningConfig dat red ctx =
  traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-01" committeeOutputIsValid
    && traceIfFalse
      "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-02"
      referencesPreviousMerkleRoot
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    committeeOracleCurrencySymbol :: CurrencySymbol
    committeeOracleCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeOraclePolicyId})
        ctx

    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
    committeeCertificateVerificationCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeCertificateVerificationPolicyId})
        ctx

    mptRootTokenCurrencySymbol :: CurrencySymbol
    mptRootTokenCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = merkleRootTokenPolicyId})
        ctx

    committeeOutputIsValid :: Bool
    committeeOutputIsValid =
      let go :: [TxOut] -> Bool
          go [] = False
          go (o : os)
            | -- recall that 'committeeOracleCurrencySymbol' should be
              -- an NFT, so  (> 0) ==> exactly one.
              Value.valueOf (txOutValue o) committeeOracleCurrencySymbol initCommitteeOracleTn > 0
              , OutputDatum d <- txOutDatum o
              , ucd :: UpdateCommitteeDatum BuiltinData <- PlutusTx.unsafeFromBuiltinData (getDatum d) =
              -- Note that we build the @msg@ that we check is signed
              -- with the data in this transaction directly... so in a sense,
              -- checking if this message is signed is checking if the
              -- transaction corresponds to the message

              let validatorHash' =
                    case addressCredential $ txOutAddress o of
                      ScriptCredential vh -> vh
                      _ -> traceError "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-03"

                  msg =
                    UpdateCommitteeHashMessage
                      { sidechainParams = sp
                      , newAggregateCommitteePubKeys = get @"aggregateCommitteePubKeys" ucd
                      , previousMerkleRoot = get @"previousMerkleRoot" red
                      , sidechainEpoch = get @"sidechainEpoch" ucd
                      , validatorHash = validatorHash'
                      }
               in traceIfFalse
                    "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-04"
                    ( Value.valueOf
                        (txInfoMint info)
                        committeeCertificateVerificationCurrencySymbol
                        (TokenName (Builtins.blake2b_256 (serialiseUchm msg)))
                        > 0
                    )
                    && traceIfFalse
                      "ERROR-UPDATE-COMMITTEE-HASH-VALIDATOR-05"
                      (get @"sidechainEpoch" dat < get @"sidechainEpoch" ucd)
            | otherwise = go os
       in go (txInfoOutputs info)

    referencesPreviousMerkleRoot :: Bool
    referencesPreviousMerkleRoot =
      -- Either we want to reference the previous merkle root or we don't (note
      -- that this is signed by the committee -- this is where the security
      -- guarantees come from).
      -- If we do want to reference the previous merkle root, we need to verify
      -- that there exists at least one input with a nonzero amount of the
      -- merkle root tokens.
      case get @"previousMerkleRoot" red of
        Nothing -> True
        Just (LedgerBytes tn) ->
          let go :: [TxInInfo] -> Bool
              go (txInInfo : rest) =
                ( (Value.valueOf (txOutValue (txInInfoResolved txInInfo)) mptRootTokenCurrencySymbol (TokenName tn) > 0)
                    || go rest
                )
              go [] = False
           in go (txInfoReferenceInputs info)

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
mkCommitteeOraclePolicy :: InitTokenAssetClass -> () -> ScriptContext -> Bool
mkCommitteeOraclePolicy itac _red ctx =
  traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-POLICY-01" initTokenBurned
    && traceIfFalse "ERROR-UPDATE-COMMITTEE-HASH-POLICY-02" checkMintedAmount
  where
    mint :: Value
    mint = txInfoMint . scriptContextTxInfo $ ctx

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

    -- Assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount =
      case fmap AssocMap.toList $ AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue mint of
        Just [(tn', amt)] -> tn' == initCommitteeOracleTn && amt == initCommitteeOracleMintAmount
        _ -> False

mkCommitteeOraclePolicyUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCommitteeOraclePolicyUntyped =
  mkUntypedMintingPolicy
    . mkCommitteeOraclePolicy
    . PlutusTx.unsafeFromBuiltinData

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
mkCommitteeHashValidatorUntyped uch versioningConfig =
  mkUntypedValidator $
    mkUpdateCommitteeHashValidator
      (PlutusTx.unsafeFromBuiltinData uch)
      (PlutusTx.unsafeFromBuiltinData versioningConfig)

serialisableCommitteeHashValidator :: Script
serialisableCommitteeHashValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkCommitteeHashValidatorUntyped||])
