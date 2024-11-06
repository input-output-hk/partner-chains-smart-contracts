{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-- Needed for Arbitrary instances for Plutus types
{-# OPTIONS_GHC -Wno-orphans #-}
-- Some top binds are used by TH generated code only
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TrustlessSidechain.Types.Unsafe (
  Packable (..),
  Codable (..),
  -- | From TrustlessSidechain.Types
  BlockProducerRegistration (..),
  auraKey,
  grandpaKey,
  inputUtxo,
  ownPkh,
  sidechainPubKey,
  sidechainSignature,
  stakeOwnership,
  Signature (..),
  StakeOwnership (..),
  -- | From Plutus.V2.Ledger.Api
  Address (..),
  addressCredential,
  Credential (..),
  getPubKeyCredential,
  getScriptCredential,
  CurrencySymbol (..),
  Datum (..),
  DatumHash (..),
  DCert (..),
  LedgerBytes (..),
  OutputDatum (..),
  isNoOutputDatum,
  getOutputDatumHash,
  getOutputDatum,
  POSIXTimeRange (..),
  PubKeyHash (..),
  ScriptContext (..),
  scriptContextPurpose,
  scriptContextTxInfo,
  ScriptHash (..),
  ScriptPurpose (..),
  getMinting,
  getSpending,
  getCertifying,
  getRewarding,
  StakingCredential (..),
  TxId (..),
  TxInfo (..),
  txInfoDCert,
  txInfoFee,
  txInfoId,
  txInfoMint,
  txInfoWdrl,
  txInfoReferenceInputs,
  txInfoValidRange,
  txInfoInputs,
  txInfoOutputs,
  txInfoSignatories,
  TxInInfo (..),
  txInInfoOutRef,
  txInInfoResolved,
  TxOut (..),
  txOutAddress,
  txOutDatum,
  txOutValue,
  TxOutRef (..),
  Value (..),
  -- | Unsafe versions of ledger API functions
  ownCurrencySymbol,
  getContinuingOutputs,
  findOwnInput,
  txSignedBy,
  getOutputsAt,
  getInputsAt,
) where

-- This module is intended to be imported qualified as `Unsafe`.
-- This module provides wrappers and extractor functions to allow fine-grained
-- and manual extraction of fields from script arguments.
--
-- The reason we want to do this is that calling PlutusTx.unsafeFromBuiltinData
-- on the whole argument has a large impact on performance:
-- - it causes the entire conversion code to be included in the script, and
-- - it causes the entire structure gets decoded strictly, regardless of
--   how many fields are used.

import PlutusLedgerApi.V2 qualified as V2
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.PlutusPrelude hiding (Integer)
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude
import TrustlessSidechain.Types qualified as Types

class Packable a where
  wrap :: BuiltinData -> a
  unwrap :: a -> BuiltinData

class (Packable unsafe, UnsafeFromData safe, ToData safe) => Codable safe unsafe | unsafe -> safe, safe -> unsafe where
  {-# INLINE decode #-}
  decode :: unsafe -> safe
  decode = PlutusTx.unsafeFromBuiltinData . unwrap
  {-# INLINE encode #-}
  encode :: safe -> unsafe
  encode = wrap . PlutusTx.toBuiltinData

-- helpers

{-# INLINEABLE nthFieldOf #-}
nthFieldOf :: PTPrelude.Integer -> BuiltinData -> BuiltinData
n `nthFieldOf` bd = snd (Builtins.unsafeDataAsConstr bd) !! n

{-# INLINEABLE nthCtorOf #-}
nthCtorOf :: PTPrelude.Integer -> BuiltinData -> Maybe BuiltinData
n `nthCtorOf` bd = case Builtins.unsafeDataAsConstr bd of
  (ix, [x]) | n == ix -> Just x
  _ -> Nothing

{-# INLINEABLE isNthCtorOf #-}
isNthCtorOf :: PTPrelude.Integer -> BuiltinData -> Bool
n `isNthCtorOf` bd = case Builtins.unsafeDataAsConstr bd of
  (ix, _) | n == ix -> True
  _ -> False

{-# INLINEABLE unsafeDataAsMaybe #-}
unsafeDataAsMaybe :: BuiltinData -> Maybe BuiltinData
unsafeDataAsMaybe bd = case Builtins.unsafeDataAsConstr bd of
  (0, [x]) -> Just x
  (1, []) -> Nothing
  _ -> traceError "unsafeDataAsMaybe: unreachable"

makeUnsafeNewtypes ''V2.StakingCredential
makeUnsafeNewtypes ''Types.BlockProducerRegistration
makeUnsafeNewtypes ''Types.Signature
makeUnsafeNewtypes ''Types.StakeOwnership
makeUnsafeNewtypes ''V2.Address
makeUnsafeNewtypes ''V2.Credential
makeUnsafeNewtypes ''V2.CurrencySymbol
makeUnsafeNewtypes ''V2.Datum
makeUnsafeNewtypes ''V2.DatumHash
makeUnsafeNewtypes ''V2.DCert
makeUnsafeNewtypes ''V2.LedgerBytes
makeUnsafeNewtypes ''V2.OutputDatum
makeUnsafeNewtypes ''V2.POSIXTimeRange
makeUnsafeNewtypes ''V2.PubKeyHash
makeUnsafeNewtypes ''V2.ScriptContext
makeUnsafeNewtypes ''V2.ScriptHash
makeUnsafeNewtypes ''V2.ScriptPurpose
makeUnsafeNewtypes ''V2.TxId
makeUnsafeNewtypes ''V2.TxInfo
makeUnsafeNewtypes ''V2.TxInInfo
makeUnsafeNewtypes ''V2.TxOut
makeUnsafeNewtypes ''V2.TxOutRef
makeUnsafeNewtypes ''V2.Value
makeUnsafeNewtypes ''V2.Redeemer
makeUnsafeNewtypes ''PTPrelude.Integer

makeUnsafeGetters ''Types.BlockProducerRegistration
makeUnsafeGetters ''V2.Address
makeUnsafeGetters ''V2.Credential
makeUnsafeGetters ''V2.OutputDatum
makeUnsafeGetters ''V2.ScriptContext
makeUnsafeGetters ''V2.ScriptPurpose
makeUnsafeGetters ''V2.TxInfo
makeUnsafeGetters ''V2.TxInInfo
makeUnsafeGetters ''V2.TxOut

-- Unsafe versions of plutus-ledger-api functions

-- | The 'CurrencySymbol' of the current validator script.
--   Adapted from Plutus.V2.Ledger.Contexts.ownCurrencySymbol
{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: ScriptContext -> V2.CurrencySymbol
ownCurrencySymbol bd = case getMinting $ scriptContextPurpose bd of
  Just cs -> decode cs
  Nothing -> traceError "Lh" -- "Can't get currency symbol of the current validator script"

-- | Get all the outputs that pay to the same script address we are currently spending from, if any.
--   Adapted from Plutus.V2.Ledger.Contexts.getContinuingOutputs
{-# INLINEABLE getContinuingOutputs #-}
getContinuingOutputs :: ScriptContext -> [TxOut]
getContinuingOutputs ctx
  | Just inInfo <- findOwnInput ctx =
      filter (f (txOutAddress . txInInfoResolved $ inInfo)) (txInfoOutputs $ scriptContextTxInfo ctx)
  where
    f addr out = addr == txOutAddress out
getContinuingOutputs _ = traceError "Lf" -- "Can't get any continuing outputs"

-- | Find the input currently being validated.
--   Adapted from Plutus.V2.Ledger.Contexts.findOwnInput
{-# INLINEABLE findOwnInput #-}
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput sc
  | Just txOutRef <- getSpending $ scriptContextPurpose sc =
      find
        (\inInfo -> (unTxOutRef . txInInfoOutRef $ inInfo) == unTxOutRef txOutRef)
        (txInfoInputs . scriptContextTxInfo $ sc)
findOwnInput _ = Nothing

-- | Check if a transaction was signed by the given public key.
--   Adapted from Plutus.V2.Ledger.Contexts.txSignedBy
{-# INLINEABLE txSignedBy #-}
txSignedBy :: TxInfo -> V2.PubKeyHash -> Bool
txSignedBy info k = any ((k ==) . decode) (txInfoSignatories info)

{-# INLINEABLE getOutputsAt #-}
getOutputsAt :: TxInfo -> V2.Address -> [TxOut]
getOutputsAt txInfo address =
  ((== address) . decode . txOutAddress) `filter` txInfoOutputs txInfo

{-# INLINEABLE getInputsAt #-}
getInputsAt :: TxInfo -> V2.Address -> [TxOut]
getInputsAt txInfo address =
  ((== address) . decode . txOutAddress)
    `filter` (txInInfoResolved <$> txInfoInputs txInfo)
