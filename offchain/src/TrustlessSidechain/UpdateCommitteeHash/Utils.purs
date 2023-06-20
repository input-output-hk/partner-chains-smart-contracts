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
import Contract.PlutusData
  ( class ToData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  , ValidatorHash
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Value as Value
import Data.Array as Array
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.CommitteeOraclePolicy (committeeOracleTn)
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass, assetClass)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage
  )
import TrustlessSidechain.Utils.Crypto (SidechainMessage)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Utxos as Utils.Utxos

updateCommitteeHashValidator ∷ UpdateCommitteeHash → Contract Validator
updateCommitteeHashValidator sp = do
  let
    script = decodeTextEnvelope RawScripts.rawCommitteeHashValidator
      >>= plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
    [ PlutusData.toData sp ]
  pure $ Validator applied

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
  Maybe SidechainMessage
serialiseUchmHash = Utils.Crypto.sidechainMessage
  <<< Hashing.blake2b256Hash
  <<< cborBytesToByteArray
  <<< PlutusData.serializeData

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
      committeeOracleTn
      /= zero
