module TrustlessSidechain.TypesRaw where -- TODO get a better name

-- This module is intended to be imported qualified as ``.
-- This module provides wrappers and extractor functions to allow fine-grained
-- and manual extraction of fields from script arguments.
--
-- The reason we want to do this is that calling PlutusTx.unsafeFromBuiltinData
-- on the whole argument has a large impact on performance:
-- - it causes the entire conversion code to be included in the script, and
-- - it causes the entire structure gets decoded strictly, regardless of
--   how many fields are used.

import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.PlutusPrelude

-- import Plutus.V1.Ledger.Value qualified as V1
import PlutusTx qualified

-- newtype SidechainParams = SidechainParams {unSidechainParams :: BuiltinData}
newtype ScriptContext = ScriptContext {unScriptContext :: BuiltinData}
newtype TxInfo = TxInfo {unTxInfo :: BuiltinData}
newtype TxInInfo = TxInInfo {unTxInInfo :: BuiltinData}
newtype TxOut = TxOut {unTxOut :: BuiltinData}
newtype TxOutRef = TxOutRef {unTxOutRef :: BuiltinData}
newtype TxInfoMint = TxInfoMint {unTxInfoMint :: BuiltinData}
newtype ScriptPurpose = ScriptPurpose {unScriptPurpose :: BuiltinData}

-- ScriptContext

{-# INLINE scriptContextTxInfo #-}
scriptContextTxInfo :: ScriptContext -> TxInfo
-- 0. field of ScriptContext is TxInfo
scriptContextTxInfo (ScriptContext bd) = TxInfo $ 0 `nthFieldOf` bd

{-# INLINE scriptContextPurpose #-}
scriptContextPurpose :: ScriptContext -> V2.ScriptPurpose
-- 1. field of ScriptContext is ScriptPurpose
scriptContextPurpose (ScriptContext bd) = PlutusTx.unsafeFromBuiltinData $ 1 `nthFieldOf` bd

-- TxOut

{-# INLINE txOutAddress #-}
txOutAddress :: TxOut -> V2.Address
-- 0. field of TxOut is Address
txOutAddress (TxOut bd) = PlutusTx.unsafeFromBuiltinData $ 0 `nthFieldOf` bd

{-# INLINE txOutValue #-}
txOutValue :: TxOut -> V2.Value
-- 1. field of TxOut is Datum
txOutValue (TxOut bd) = PlutusTx.unsafeFromBuiltinData $ 1 `nthFieldOf` bd

{-# INLINE txOutDatum #-}
txOutDatum :: TxOut -> V2.OutputDatum
-- 2. field of TxOut is Datum
txOutDatum (TxOut bd) = PlutusTx.unsafeFromBuiltinData $ 2 `nthFieldOf` bd

-- TxInInfo

{-# INLINE txInInfoOutRef #-}
txInInfoOutRef :: TxInInfo -> TxOutRef
-- 0. field of TxInInfo is OutRef
txInInfoOutRef (TxInInfo bd) = TxOutRef $ 0 `nthFieldOf` bd

{-# INLINE txInInfoResolved #-}
txInInfoResolved :: TxInInfo -> TxOut
-- 1. field of TxInInfo is InfoResolved
txInInfoResolved (TxInInfo bd) = TxOut $ 1 `nthFieldOf` bd

-- TxInfo

{-# INLINE txInfoInputs #-}
txInfoInputs :: TxInfo -> [TxInInfo]
-- 0. field of TxInfo is txInfoInputs
txInfoInputs (TxInfo bd) = TxInInfo <$> (Builtins.unsafeDataAsList $ 0 `nthFieldOf` bd)

{-# INLINE txInfoReferenceInputs #-}
txInfoReferenceInputs :: TxInfo -> [TxInInfo]
-- 1. field of TxInfo is txInfoReferenceInputs
txInfoReferenceInputs (TxInfo bd) = TxInInfo <$> (Builtins.unsafeDataAsList $ 1 `nthFieldOf` bd)

{-# INLINE txInfoOutputs #-}
txInfoOutputs :: TxInfo -> [TxOut]
-- 2. field of TxInfo is txInfoReferenceInputs
txInfoOutputs (TxInfo bd) = TxOut <$> (Builtins.unsafeDataAsList $ 2 `nthFieldOf` bd)

{-# INLINE txInfoMint #-}
txInfoMint :: TxInfo -> V2.Value
-- 4. field of TxInfo is txInfoMint
txInfoMint (TxInfo bd) = PlutusTx.unsafeFromBuiltinData $ 4 `nthFieldOf` bd

{-# INLINE txInfoSignatories #-}
txInfoSignatories :: TxInfo -> [V2.PubKeyHash]
-- 8. field of TxInfo is txInfoSignatories
txInfoSignatories (TxInfo bd) = PlutusTx.unsafeFromBuiltinData $ 8 `nthFieldOf` bd

-- {-# INLINE getMinting #-}
-- getMinting :: ScriptPurpose -> Maybe V2.CurrencySymbol
-- -- 1st ctor of ScriptPurpose is Minting
-- getMinting (ScriptPurpose bd) = case Builtins.unsafeDataAsConstr bd of
--   (0, [cs]) -> Just (PlutusTx.unsafeFromBuiltinData cs)
--   _ -> Nothing

-- | Check if a transaction was signed by the given public key.
{-# INLINEABLE txSignedBy #-}
txSignedBy :: TxInfo -> V2.PubKeyHash -> Bool
-- TODO replace with `any` when we have newer Plutus vesion
txSignedBy info k = case find ((==) k) (txInfoSignatories info) of
  Just _ -> True
  Nothing -> False

{-# INLINEABLE nthFieldOf #-}
nthFieldOf :: Integer -> BuiltinData -> BuiltinData
nthFieldOf n bd = snd (Builtins.unsafeDataAsConstr bd) !! n

-- Raw versions of plutus-ledger-api functions

-- | The 'CurrencySymbol' of the current validator script.
--   Adapted from Plutus.V2.Ledger.Contexts.ownCurrencySymbol
{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: ScriptContext -> V2.CurrencySymbol
ownCurrencySymbol bd = case scriptContextPurpose bd of
  V2.Minting cs -> cs
  _ -> traceError "Lh" -- "Can't get currency symbol of the current validator script"

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
  | V2.Spending txOutRef <- scriptContextPurpose sc =
    find
      (\inInfo -> (PlutusTx.unsafeFromBuiltinData . unTxOutRef . txInInfoOutRef $ inInfo) == txOutRef)
      (txInfoInputs . scriptContextTxInfo $ sc)
findOwnInput _ = Nothing
