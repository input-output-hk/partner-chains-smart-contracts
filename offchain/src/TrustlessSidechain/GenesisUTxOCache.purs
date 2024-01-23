module TrustlessSidechain.GenesisUTxOCache
  ( prepareGenesisUTxO
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator
  , validatorHash
  )
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  )
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( GenesisUTxOCache
      )
  )

genesisUTxOCacheValidator ∷ BigInt → Contract Validator
genesisUTxOCacheValidator chainId =
  mkValidatorWithParams GenesisUTxOCache [ toData chainId ]

-- | `prepareGenesisUTxO` takes a minimal amount of Ada from the wallet and
-- | sends it to a dedicated script, where it is stored to be used as a genesis
-- | UTxO.
prepareGenesisUTxO ∷
  BigInt →
  Contract
    { transactionId ∷ TransactionHash
    , outputIndex ∷ Int
    }
prepareGenesisUTxO chainId = do
  ownPkh ← getOwnPaymentPubKeyHash
  ownAddr ← getOwnWalletAddress
  ownUtxos ← utxosAt ownAddr

  validator ← genesisUTxOCacheValidator chainId

  let -- Take one Lovelace.  Balancing will ensure to convert this into minAda.
    someAda = lovelaceValueOf (BigInt.fromInt 1)
    genesisUTxOCacheValidatorHash = validatorHash validator

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs ownUtxos
      <> Lookups.validator validator

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript genesisUTxOCacheValidatorHash
        (Datum $ toData ownPkh)
        DatumInline
        someAda

  transactionId ← balanceSignAndSubmit "Prepare genesis UTxO"
    { lookups, constraints }

  pure { transactionId, outputIndex: 0 }
