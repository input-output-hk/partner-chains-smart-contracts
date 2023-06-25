{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TrustlessSidechain.UpdateCommitteeHash where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  TokenName (TokenName),
  Value (getValue),
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import Plutus.V2.Ledger.Tx (OutputDatum (OutputDatum))
import PlutusTx (ToData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  UpdateCommitteeDatum (aggregateCommitteePubKeys, sidechainEpoch),
  UpdateCommitteeHash (
    cCommitteeCertificateVerificationCurrencySymbol,
    cCommitteeOracleCurrencySymbol,
    cMptRootTokenCurrencySymbol,
    cSidechainParams
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    uchmNewAggregateCommitteePubKeys,
    uchmPreviousMerkleRoot,
    uchmSidechainEpoch,
    uchmSidechainParams,
    uchmValidatorAddress
  ),
  UpdateCommitteeHashRedeemer (uchrPreviousMerkleRoot),
 )

-- * Updating the committee hash

{- | 'serialiseUchm' serialises an 'UpdateCommitteeHashMessage' via converting
 to the Plutus data representation, then encoding it to cbor via the builtin.
-}
serialiseUchm :: ToData aggregatePubKeys => UpdateCommitteeHashMessage aggregatePubKeys -> BuiltinByteString
serialiseUchm = Builtins.serialiseData . IsData.toBuiltinData

{- | 'initCommitteeHashMintTn'  is the token name of the NFT which identifies
 the utxo which contains the committee hash. We use an empty bytestring for
 this because the name really doesn't matter, so we mighaswell save a few
 bytes by giving it the empty name.
-}
{-# INLINEABLE initCommitteeHashMintTn #-}
initCommitteeHashMintTn :: TokenName
initCommitteeHashMintTn = TokenName Builtins.emptyByteString

{- | 'initCommitteeHashMintAmount' is the amount of the currency to mint which
 is 1.
-}
{-# INLINEABLE initCommitteeHashMintAmount #-}
initCommitteeHashMintAmount :: Integer
initCommitteeHashMintAmount = 1

{- | 'mkUpdateCommitteeHashValidator' is the on-chain validator. We test for the following conditions
  1. The redeemer is signed by the current committee

  2. We reference the last merkle root (as understood by the signed redeemer)

  3. the sidechain epoch (as understood by the redeemer) is strictly increasing
  from the current sidechain epoch in the datum

  4. There is an output which satisfies

    - it has the new aggregated committee public keys

    - it has the new sidechain epoch

    - it is at the new address (as understood from the redeemer)

    - the Sidechain parameters are the same? TODO
-}
{-# INLINEABLE mkUpdateCommitteeHashValidator #-}
mkUpdateCommitteeHashValidator ::
  UpdateCommitteeHash ->
  UpdateCommitteeDatum BuiltinData ->
  UpdateCommitteeHashRedeemer ->
  ScriptContext ->
  Bool
mkUpdateCommitteeHashValidator uch dat red ctx =
  traceIfFalse "error 'mkUpdateCommitteeHashValidator': invalid committee output" committeeOutputIsValid
    && traceIfFalse
      "error 'mkUpdateCommitteeHashValidator': tx doesn't reference previous merkle root"
      referencesPreviousMerkleRoot
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    committeeOutputIsValid :: Bool
    committeeOutputIsValid =
      let go :: [TxOut] -> Bool
          go [] = False
          go (o : os)
            | -- recall that 'cCommitteeOracleCurrencySymbol' should be
              -- an NFT, so  (> 0) ==> exactly one.
              Value.valueOf (txOutValue o) (cCommitteeOracleCurrencySymbol uch) initCommitteeHashMintTn > 0
              , OutputDatum d <- txOutDatum o
              , ucd :: UpdateCommitteeDatum BuiltinData <- PlutusTx.unsafeFromBuiltinData (getDatum d) =
              -- Note that we build the @msg@ that we check is signed
              -- with the data in this transaction directly... so in a sense,
              -- checking if this message is signed is checking if the
              -- transaction corresponds to the message
              let msg =
                    UpdateCommitteeHashMessage
                      { uchmSidechainParams = cSidechainParams uch
                      , uchmNewAggregateCommitteePubKeys = aggregateCommitteePubKeys ucd
                      , uchmPreviousMerkleRoot = uchrPreviousMerkleRoot red
                      , uchmSidechainEpoch = sidechainEpoch ucd
                      , uchmValidatorAddress = txOutAddress o
                      }
               in traceIfFalse
                    "error 'mkUpdateCommitteeHashValidator': tx not signed by committee"
                    ( Value.valueOf
                        (txInfoMint info)
                        (cCommitteeCertificateVerificationCurrencySymbol uch)
                        (TokenName (Builtins.blake2b_256 (serialiseUchm msg)))
                        > 0
                    )
                    && traceIfFalse
                      "error 'mkUpdateCommitteeHashValidator': sidechain epoch is not strictly increasing"
                      (sidechainEpoch dat < sidechainEpoch ucd)
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
      case uchrPreviousMerkleRoot red of
        Nothing -> True
        Just tn ->
          let go :: [TxInInfo] -> Bool
              go (txInInfo : rest) =
                ( (Value.valueOf (txOutValue (txInInfoResolved txInInfo)) (cMptRootTokenCurrencySymbol uch) (TokenName tn) > 0)
                    || go rest
                )
              go [] = False
           in go (txInfoReferenceInputs info)

-- * Initializing the committee hash

-- | 'InitCommitteeHashMint' is used as the parameter for the minting policy
newtype InitCommitteeHashMint = InitCommitteeHashMint
  { -- | 'TxOutRef' is the output reference to mint the NFT initially.
    icTxOutRef :: TxOutRef
  }
  deriving newtype
    ( TSPrelude.Show
    , TSPrelude.Eq
    , TSPrelude.Ord
    , PlutusTx.UnsafeFromData
    )

PlutusTx.makeLift ''InitCommitteeHashMint

{- | 'mkCommitteeOraclePolicy' is the minting policy for the NFT which identifies
 the committee hash.
-}
{-# INLINEABLE mkCommitteeOraclePolicy #-}
mkCommitteeOraclePolicy :: InitCommitteeHashMint -> () -> ScriptContext -> Bool
mkCommitteeOraclePolicy ichm _red ctx =
  traceIfFalse "error 'mkCommitteeOraclePolicy' UTxO not consumed" hasUtxo
    && traceIfFalse "error 'mkCommitteeOraclePolicy' wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    oref :: TxOutRef
    oref = icTxOutRef ichm
    hasUtxo :: Bool
    hasUtxo = any ((oref ==) . txInInfoOutRef) $ txInfoInputs info
    -- Assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount =
      case fmap AssocMap.toList $ AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue $ txInfoMint info of
        Just [(tn', amt)] -> tn' == initCommitteeHashMintTn && amt == initCommitteeHashMintAmount
        _ -> False

-- CTL hack
mkCommitteeOraclePolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCommitteeOraclePolicyUntyped =
  ScriptUtils.mkUntypedMintingPolicy . mkCommitteeOraclePolicy . PlutusTx.unsafeFromBuiltinData

serialisableCommitteeOraclePolicy :: Versioned Ledger.Script
serialisableCommitteeOraclePolicy =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCommitteeOraclePolicyUntyped||])) PlutusV2

mkCommitteeHashValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCommitteeHashValidatorUntyped =
  ScriptUtils.mkUntypedValidator . mkUpdateCommitteeHashValidator . PlutusTx.unsafeFromBuiltinData

serialisableCommitteeHashValidator :: Versioned Ledger.Script
serialisableCommitteeHashValidator =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCommitteeHashValidatorUntyped||])) PlutusV2
