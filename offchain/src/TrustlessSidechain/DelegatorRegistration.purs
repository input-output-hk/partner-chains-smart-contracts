module TrustlessSidechain.DelegatorRegistration
  ( DelegatorWalletEntry(..)
  , delegatorRegistration
  ) where

import Contract.Prelude

import Contract.Address (StakePubKeyHash)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( getOwnWalletAddress
  , toAddress
  )
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(DelegatorRegistrationValidator)
  )
import Type.Row (type (+))

newtype DelegatorWalletEntry = DelegatorWalletEntry
  { stakePubKeyHash ∷ StakePubKeyHash
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

delegatorRegistration ∷
  ∀ r.
  SidechainParams →
  StakePubKeyHash →
  ByteArray →
  Run (APP + r) TransactionHash
delegatorRegistration sp stakePubKeyHash partnerChainWallet = do
  let datum = DelegatorWalletEntry { stakePubKeyHash, partnerChainWallet }
  delegatorRegistrationValidator ← mkValidatorWithParams
    DelegatorRegistrationValidator
    [ toData sp, toData datum ]
  ownAddr ← getOwnWalletAddress
  let
    val = Value.lovelaceValueOf (BigNum.fromInt 1)
    valHash = Scripts.validatorHash delegatorRegistrationValidator
  valAddr ← toAddress valHash
  ownUtxos ← Effect.utxosAt ownAddr
  valUtxos ← Effect.utxosAt valAddr

  pure
    { lookups: Lookups.unspentOutputs ownUtxos
        <> Lookups.validator delegatorRegistrationValidator
        <> Lookups.unspentOutputs valUtxos
    , constraints: TxConstraints.mustPayToScript valHash (toData datum)
        TxConstraints.DatumInline
        val
    }
    >>= balanceSignAndSubmit "DelegatorRegistration"
