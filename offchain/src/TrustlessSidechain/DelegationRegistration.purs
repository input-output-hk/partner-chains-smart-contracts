module TrustlessSidechain.DelegationRegistration where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(Datum)
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

newtype DelegatorWalletEntry = DelegatorWalletEntry
  { stakePubKeyHash ∷ ByteArray
  , partnerChainWallet ∷ ByteArray
  }

instance FromData DelegatorWalletEntry where
  fromData (Constr n [ a, b ]) | n == BigNum.fromInt 0 = ado
    stakePubKeyHash ← fromData a
    partnerChainWallet ← fromData b
    in DelegatorWalletEntry { stakePubKeyHash, partnerChainWallet }
  fromData _ = Nothing

derive instance Generic DelegatorWalletEntry _

derive instance Newtype DelegatorWalletEntry _

derive newtype instance Eq DelegatorWalletEntry

instance ToData DelegatorWalletEntry where
  toData
    ( DelegatorWalletEntry
        { stakePubKeyHash, partnerChainWallet }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData stakePubKeyHash
      , toData partnerChainWallet
      ]

instance Show DelegatorWalletEntry where
  show = genericShow

getDelegationRegistration ∷
  ∀ r.
  ByteArray →
  ByteArray →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
getDelegationRegistration stakePubKeyHash partnerChainWallet = do
  let
    datum = Datum $ PlutusData.toData $ DelegatorWalletEntry
      { stakePubKeyHash, partnerChainWallet }
    constraints = TxConstraints.mustIncludeDatum datum
  pure
    { lookups: mempty
    , constraints
    }
