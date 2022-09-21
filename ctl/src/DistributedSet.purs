-- | This module implements the required offchain functionality of the
-- distributed set.
module DistributedSet
  ( Ds(Ds)
  , DsDatum(DsDatum)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)


  , insertValidator
  ) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.Monad (Contract)
import Contract.PlutusData (PlutusData(..))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (Validator, ValidatorHash)
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction (Language(PlutusV2), TransactionInput)
import FromData (class FromData, fromData)
-- import Types.Scripts as Scripts
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import RawScripts as RawScripts
import ToData (class ToData, toData)

-- * Types

-- | 'Ds' is the type which parameterizes the validator script for insertion in
-- the distributed set.
newtype Ds = Ds { dsConf ∷ CurrencySymbol }

-- | 'DsDatum' is the datum for the validator script for insertion in the
-- distributed set.
newtype DsDatum = DsDatum { dsNext ∷ ByteArray }

-- | 'DsConfDatum' is the datum for the validator script which holds the
-- configuration of the distributed set on chain.
newtype DsConfDatum = DsConfDatum
  { dscKeyPolicy ∷ CurrencySymbol
  , dscFUELPolicy ∷ CurrencySymbol
  }

-- | 'DsConfMint' is the type which paramaterizes the minting policy of the NFT
-- which initializes the distribted set.
newtype DsConfMint = DsConfMint { dscmTxOutRef ∷ TransactionInput }

-- | 'DsKeyMint' is the type which paramterizes the minting policy of the
-- tokens which are keys in the distributed set.
newtype DsKeyMint = DsKeyMint
  { dskmValidatorHash ∷ ValidatorHash
  , dskmCurrencySymbol ∷ CurrencySymbol
  }

-- * Validator / minting policies

-- | 'insertValidator' gets the validator
insertValidator ∷ Ds → Contract () Validator
insertValidator _ds = do
  validatorBytes ← TextEnvelope.textEnvelopeBytes RawScripts.rawInsertValidator
    PlutusScriptV2
  let validatorUnapplied = wrap $ wrap $ validatorBytes /\ PlutusV2 ∷ Validator
  -- TODO: apply 'validatorUnapplied' to the validator..
  pure validatorUnapplied

-- * ToData / FromData instances.
-- These should correspond to the on-chain Haskell types.

instance FromData Ds where
  fromData (Constr n [ a ]) | n == zero = Ds <<< { dsConf: _ } <$> fromData a
  fromData _ = Nothing

instance ToData Ds where
  toData (Ds { dsConf }) = Constr zero [ toData dsConf ]

instance FromData DsDatum where
  fromData (Constr n [ a ]) | n == zero = DsDatum <<< { dsNext: _ } <$> fromData
    a
  fromData _ = Nothing

instance ToData DsDatum where
  toData (DsDatum { dsNext }) = Constr zero [ toData dsNext ]

instance FromData DsConfDatum where
  fromData (Constr n [ a, b ]) | n == zero =
    DsConfDatum <$>
      ({ dscKeyPolicy: _, dscFUELPolicy: _ } <$> fromData a <*> fromData b)
  fromData _ = Nothing

instance ToData DsConfDatum where
  toData (DsConfDatum { dscKeyPolicy, dscFUELPolicy }) = Constr zero
    [ toData dscKeyPolicy, toData dscFUELPolicy ]

instance FromData DsConfMint where
  fromData (Constr n [ a ])
    | n == zero = DsConfMint <<< { dscmTxOutRef: _ } <$> fromData a
  fromData _ = Nothing

instance ToData DsConfMint where
  toData (DsConfMint { dscmTxOutRef }) = Constr zero [ toData dscmTxOutRef ]

instance FromData DsKeyMint where
  fromData (Constr n [ a, b ])
    | n == zero = DsKeyMint <$>
        ( { dskmValidatorHash: _, dskmCurrencySymbol: _ } <$> fromData a <*>
            fromData b
        )
  fromData _ = Nothing

instance ToData DsKeyMint where
  toData (DsKeyMint { dskmValidatorHash, dskmCurrencySymbol }) = Constr zero
    [ toData dskmValidatorHash, toData dskmCurrencySymbol ]
