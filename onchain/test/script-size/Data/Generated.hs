{-# LANGUAGE TemplateHaskell #-}

module Data.Generated (Foo (..)) where

import Ledger.Value (CurrencySymbol)
import PlutusTx (makeIsDataIndexed)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (SidechainParams)

data Foo = Foo
  { tcs :: CurrencySymbol
  , sp :: SidechainParams
  , kcs :: CurrencySymbol
  }

makeIsDataIndexed ''Foo [('Foo, 0)]
