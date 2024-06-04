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

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.Value as Value
import Contract.Address (Address)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , toData
  )
import Contract.Transaction
  ( TransactionInput
  )
import Partial.Unsafe (unsafePartial)
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
  Run (EXCEPT OffchainError + r) PlutusScript
updateCommitteeHashValidator sp versionOracleConfig =
  mkValidatorWithParams CommitteeHashValidator
    [ toData sp, toData versionOracleConfig ]

-- | `getUpdateCommitteeHashValidator` wraps `updateCommitteeHashValidator` but
-- | also returns the hash and address
getUpdateCommitteeHashValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { validator ∷ PlutusScript
    , validatorHash ∷ ScriptHash
    , address ∷ Address
    }
getUpdateCommitteeHashValidator sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  validator ← updateCommitteeHashValidator sp versionOracleConfig
  let validatorHash = PlutusScript.hash validator
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
serialiseUchmHash keys = unsafePartial
  ( Utils.Crypto.ecdsaSecp256k1Message
      $ Utils.Crypto.blake2b256Hash
      $ unwrap
      $ encodeCbor
      $ toData keys
  )

-- | `findUpdateCommitteeHashUtxo` returns the (unique) utxo which hold the token which
-- | identifies the committee hash.
--
-- Time complexity: bad, it looks at all utxos at the update committee hash
-- validator, then linearly scans through each utxo to determine which has token
findUpdateCommitteeHashUtxo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findUpdateCommitteeHashUtxo sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  validator ← updateCommitteeHashValidator sp versionOracleConfig
  validatorAddress ← toAddress (PlutusScript.hash validator)

  committeeOracleCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sp
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this committee hash nft.
    Value.valueOf
      ( Asset committeeOracleCurrencySymbol
          CommitteeOraclePolicy.committeeOracleTn
      )
      value
      /= BigNum.fromInt 0
