-- | `UpdateCommitteeHash.Utils` contains utility functions relating to the
-- | update committee hash endpoint including:
-- |
-- |      - Creating the data for onchain validators / minting policies
-- |
-- |      - Mirroring functionality of onchain code
-- |
-- |      - Querying utxos regarding the update committee hash
-- |
-- | Note: the reason for the existence of this module is because there are some
-- | cyclic dependencies between `MerkleRoot` and `UpdateCommitteeHash` without
-- | this.
module TrustlessSidechain.UpdateCommitteeHash.Utils
  ( updateCommitteeHashValidator
  , findUpdateCommitteeHashUtxo
  , serialiseUchmHash
  , getUpdateCommitteeHashValidator
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.PlutusData
  ( class ToData
  , serializeData
  , toData
  )
import Contract.Scripts
  ( Validator
  , ValidatorHash
  )
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Value as Value
import Data.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHashMessage
  )
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Utxos as Utils.Utxos
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteeHashValidator)
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(CommitteeOraclePolicy)
  , VersionOracle(VersionOracle)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

updateCommitteeHashValidator ∷
  ∀ r.
  SidechainParams →
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
updateCommitteeHashValidator sp versionOracleConfig =
  mkValidatorWithParams CommitteeHashValidator
    [ toData sp, toData versionOracleConfig ]

-- | `getUpdateCommitteeHashValidator` wraps `updateCommitteeHashValidator` but
-- | also returns the hash and address
getUpdateCommitteeHashValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { validator ∷ Validator
    , validatorHash ∷ ValidatorHash
    , address ∷ Address
    }
getUpdateCommitteeHashValidator sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  validator ← updateCommitteeHashValidator sp versionOracleConfig
  let validatorHash = Scripts.validatorHash validator
  address ← toAddress validatorHash
  pure { validator, validatorHash, address }

-- | `serialiseUchmHash` is an alias for (ignoring the `Maybe`)
-- | ```
-- | Contract.Hashing.blake2b256Hash <<< PlutusData.serializeData
-- | ```
-- | The result of this function is what is signed by the committee members.
serialiseUchmHash ∷
  ∀ aggregatePubKeys.
  ToData aggregatePubKeys ⇒
  UpdateCommitteeHashMessage aggregatePubKeys →
  Maybe EcdsaSecp256k1Message
serialiseUchmHash = Utils.Crypto.ecdsaSecp256k1Message
  <<< Hashing.blake2b256Hash
  <<< cborBytesToByteArray
  <<< serializeData

-- | `findUpdateCommitteeHashUtxo` returns the (unique) utxo which hold the token which
-- | identifies the committee hash.
--
-- Time complexity: bad, it looks at all utxos at the update committee hash
-- validator, then linearly scans through each utxo to determine which has token
findUpdateCommitteeHashUtxo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUpdateCommitteeHashUtxo sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  validator ← updateCommitteeHashValidator sp versionOracleConfig
  validatorAddress ← toAddress (Scripts.validatorHash validator)

  committeeOracleCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sp
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this committee hash nft.
    Value.valueOf value committeeOracleCurrencySymbol
      CommitteeOraclePolicy.committeeOracleTn
      /= zero
