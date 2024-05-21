module TrustlessSidechain.DelegationRegistration
  ( DelegatorWalletEntry(..)
  , delegationRegistration
  ) where

import Contract.Prelude

import Contract.Address (StakePubKeyHash)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(Datum)
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
import Data.BigInt as BigInt
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
  ( ScriptId(DelegationRegistrationValidator)
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

delegationRegistration ∷
  ∀ r.
  SidechainParams →
  StakePubKeyHash →
  ByteArray →
  Run (APP + r) TransactionHash
delegationRegistration sp stakePubKeyHash partnerChainWallet = do
  let datum = DelegatorWalletEntry { stakePubKeyHash, partnerChainWallet }
  delegationRegistrationValidator ← mkValidatorWithParams
    DelegationRegistrationValidator
    [ toData sp, toData datum ]
  ownAddr ← getOwnWalletAddress
  let
    val = Value.lovelaceValueOf (BigInt.fromInt 1)
    valHash = Scripts.validatorHash delegationRegistrationValidator
  valAddr ← toAddress valHash
  ownUtxos ← Effect.utxosAt ownAddr
  valUtxos ← Effect.utxosAt valAddr

  pure
    { lookups: Lookups.unspentOutputs ownUtxos
        <> Lookups.validator delegationRegistrationValidator
        <> Lookups.unspentOutputs valUtxos
    , constraints: TxConstraints.mustPayToScript valHash (Datum $ toData datum)
        TxConstraints.DatumInline
        val
    }
    >>= balanceSignAndSubmit "DelegationRegistration"
