{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator (
  Mpt (Mpt, mptCurrencySymbol, mptSidechainParams),
  validator,
  hash,
  address,
) where

import Ledger qualified
import Ledger.Typed.Scripts (Validator)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Typed.Scripts.Validators (DatumType, RedeemerType, ValidatorTypes)
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusPrelude qualified
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import Prelude qualified

{- | Merkle proof token (abbr. 'Mpt') is the parameter for the
 'mkMptRootTokenValidator'. We're going to get rid of this when we get
 reference inputs but for now it contains the 'CurrencySymbol' of the actual
 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy'.
-}
data Mpt = Mpt
  { mptCurrencySymbol :: CurrencySymbol
  , mptSidechainParams :: SidechainParams
  }
  deriving stock (Prelude.Show, PlutusPrelude.Generic)

PlutusTx.makeLift ''Mpt

instance ValidatorTypes Mpt where
  type RedeemerType Mpt = ()
  type DatumType Mpt = ()

-- | 'mkMptRootTokenValidator' always fails.
{-# INLINEABLE mkMptRootTokenValidator #-}
mkMptRootTokenValidator :: Mpt -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMptRootTokenValidator _mpt _dat _red _ctx = ()

-- This should be the following when we get reference inputs
-- > mkMptRootTokenValidator :: Mpt -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- > mkMptRootTokenValidator mpt _dat _red _ctx = Builtins.error ()

validator :: Mpt -> Validator
validator mpt =
  Scripts.mkValidatorScript
    ( $$(PlutusTx.compile [||mkMptRootTokenValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode mpt
    )

hash :: Mpt -> ValidatorHash
hash = Scripts.validatorHash . Scripts.unsafeMkTypedValidator . validator

address :: Mpt -> Address
address = Ledger.scriptHashAddress . hash
