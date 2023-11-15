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
import Contract.Address as Address
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (class ToData, serializeData, toData)
import Contract.Scripts
  ( Validator
  , ValidatorHash
  )
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value as Value
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.RawScripts (rawCommitteeHashValidator)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash
  , UpdateCommitteeHashMessage
  )
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1Message)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Utxos as Utils.Utxos

updateCommitteeHashValidator ∷ UpdateCommitteeHash → Contract Validator
updateCommitteeHashValidator sidechainParams =
  mkValidatorWithParams rawCommitteeHashValidator [ toData sidechainParams ]

-- | `getUpdateCommitteeHashValidator` wraps `updateCommitteeHashValidator` but
-- | also returns the hash and address
getUpdateCommitteeHashValidator ∷
  UpdateCommitteeHash →
  Contract
    { validator ∷ Validator
    , validatorHash ∷ ValidatorHash
    , address ∷ Address
    }
getUpdateCommitteeHashValidator uch = do
  netId ← Address.getNetworkId
  validator ← updateCommitteeHashValidator uch
  let validatorHash = Scripts.validatorHash validator

  address ← Monad.liftContractM
    "error 'getUpdateCommitteeHashValidator': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)
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
  UpdateCommitteeHash →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUpdateCommitteeHashUtxo uch = do
  netId ← Address.getNetworkId
  validator ← updateCommitteeHashValidator uch
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← Monad.liftContractM
    "error 'findUpdateCommitteeHashUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this committee hash nft.
    Value.valueOf value ((unwrap uch).committeeOracleCurrencySymbol)
      CommitteeOraclePolicy.committeeOracleTn
      /= zero
