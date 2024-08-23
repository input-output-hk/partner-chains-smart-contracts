{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TrustlessSidechain.CustomScriptContext where

import Data.Eq qualified as Haskell
import PlutusLedgerApi.V2 hiding (TxInfo, txInfoMint, txInfoReferenceInputs)
import PlutusPrelude (Generic)
import PlutusTx
import TrustlessSidechain.PlutusPrelude

data CustomScriptContext a = CustomScriptContext {scriptContextTxInfo :: a, scriptContextPurpose :: ScriptPurpose}
  deriving stock (Generic, Haskell.Eq)

makeLift ''CustomScriptContext
makeIsDataIndexed ''CustomScriptContext [('CustomScriptContext, 0)]

{-# INLINEABLE ownCurrencySymbol #-}

-- | The 'CurrencySymbol' of the current validator script.
ownCurrencySymbol :: CustomScriptContext a -> CurrencySymbol
ownCurrencySymbol CustomScriptContext {scriptContextPurpose = Minting cs} = cs
ownCurrencySymbol _ = traceError "Lh" -- "Can't get currency symbol of the current validator script"

{-# INLINEABLE txSignedBy #-}

-- | Check if a transaction was signed by the given public key.
txSignedBy :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy signatories k = case find (k ==) signatories of
  Just _ -> True
  Nothing -> False
