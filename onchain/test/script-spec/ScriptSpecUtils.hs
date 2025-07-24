{-# LANGUAGE TemplateHaskell #-}

module ScriptSpecUtils where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Either
import Data.String
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

emptyScriptContext :: V2.ScriptContext
emptyScriptContext =
  V2.ScriptContext
    { V2.scriptContextTxInfo = emptyTxInfo
    , V2.scriptContextPurpose = undefined
    }

emptyTxInfo :: V2.TxInfo
emptyTxInfo =
  V2.TxInfo
    { V2.txInfoInputs = []
    , V2.txInfoReferenceInputs = []
    , V2.txInfoOutputs = []
    , V2.txInfoFee = emptyValue
    , V2.txInfoMint = emptyValue
    , V2.txInfoDCert = []
    , V2.txInfoWdrl = Map.empty
    , V2.txInfoValidRange = interval (V2.POSIXTime 0) (V2.POSIXTime 0)
    , V2.txInfoSignatories = []
    , V2.txInfoRedeemers = Map.empty
    , V2.txInfoData = Map.empty
    , V2.txInfoId = V2.TxId ""
    }

emptyTxInInfo :: V2.TxInInfo
emptyTxInInfo =
  V2.TxInInfo
    { V2.txInInfoOutRef = emptyTxOutRef
    , V2.txInInfoResolved = emptyTxOut
    }

emptyTxOutRef :: V2.TxOutRef
emptyTxOutRef = V2.TxOutRef "" 0

emptyTxOut :: V2.TxOut
emptyTxOut =
  V2.TxOut
    { V2.txOutAddress = V2.Address (V2.PubKeyCredential "") Nothing
    , V2.txOutValue = emptyValue
    , V2.txOutDatum = V2.NoOutputDatum
    , V2.txOutReferenceScript = Nothing
    }

emptyValue :: V2.Value
emptyValue = V2.Value Map.empty

mkMintingPurpose :: V2.BuiltinByteString -> V2.ScriptPurpose
mkMintingPurpose currSym = V2.Minting $ V2.CurrencySymbol currSym

mkTxOut :: (ToData d) => V2.Address -> V2.Value -> d -> V2.ScriptHash -> V2.TxOut
mkTxOut txOutAddress txOutValue datum scriptHash =
  V2.TxOut
    { V2.txOutAddress = txOutAddress
    , V2.txOutValue = txOutValue
    , V2.txOutDatum = V2.OutputDatum (V2.Datum $ toBuiltinData datum)
    , V2.txOutReferenceScript = Just scriptHash
    }

builtinDummy :: BuiltinData
builtinDummy = toBuiltinData ()

-- test functions

expectFail :: TestName -> BuiltinUnit -> TestTree
expectFail str a = testCase str do
  res <- catch (Right <$> evaluate a) \(SomeException _) -> return $ Left ()
  case res of
    Right _ -> assertFailure ("expected fail")
    _ -> return ()

expectSuccess :: TestName -> BuiltinUnit -> TestTree
expectSuccess str a = testCase str do
  res <- catch (Right <$> evaluate a) \(SomeException _) -> return $ Left ()
  case res of
    Left _ -> assertFailure ("expected pass")
    _ -> return ()

-- lens

_scriptContextTxInfo :: Lens' V2.ScriptContext V2.TxInfo
_scriptContextTxInfo f a@V2.ScriptContext {..} =
  fmap
    (\scriptContextTxInfo' -> a {V2.scriptContextTxInfo = scriptContextTxInfo'})
    (f scriptContextTxInfo)

_scriptContextPurpose :: Lens' V2.ScriptContext V2.ScriptPurpose
_scriptContextPurpose f a@V2.ScriptContext {..} =
  fmap
    (\scriptContextPurpose' -> a {V2.scriptContextPurpose = scriptContextPurpose'})
    (f scriptContextPurpose)

_txInfoInputs :: Lens' V2.TxInfo [V2.TxInInfo]
_txInfoInputs f a@V2.TxInfo {..} =
  fmap
    (\txInfoInputs' -> a {V2.txInfoInputs = txInfoInputs'})
    (f txInfoInputs)

_txInfoOutputs :: Lens' V2.TxInfo [V2.TxOut]
_txInfoOutputs f a@V2.TxInfo {..} =
  fmap
    (\txInfoOutputs' -> a {V2.txInfoOutputs = txInfoOutputs'})
    (f txInfoOutputs)

_txInfoMint :: Lens' V2.TxInfo V2.Value
_txInfoMint f a@V2.TxInfo {..} =
  fmap
    (\txInfoMint' -> a {V2.txInfoMint = txInfoMint'})
    (f txInfoMint)

_txInInfoOutRef :: Lens' V2.TxInInfo V2.TxOutRef
_txInInfoOutRef f a@V2.TxInInfo {..} =
  fmap
    (\txInInfoOutRef' -> a {V2.txInInfoOutRef = txInInfoOutRef'})
    (f txInInfoOutRef)

_txInInfoResolved :: Lens' V2.TxInInfo V2.TxOut
_txInInfoResolved f a@V2.TxInInfo {..} =
  fmap
    (\txInInfoResolved' -> a {V2.txInInfoResolved = txInInfoResolved'})
    (f txInInfoResolved)
