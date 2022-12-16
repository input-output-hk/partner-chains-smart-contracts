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
-- | cyclic dependencies between `MPTRoot` and `UpdateCommitteeHash` without
-- | this.
module UpdateCommitteeHash.Utils
  ( committeeHashPolicy
  , updateCommitteeHashValidator
  , initCommitteeHashMintTn
  , committeeHashAssetClass
  , aggregateKeys
  , findUpdateCommitteeHashUtxo
  , serialiseUchmHash
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Transaction as Transaction
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import RawScripts as RawScripts
import Types (AssetClass, assetClass)
import UpdateCommitteeHash.Types
  ( InitCommitteeHashMint
  , UpdateCommitteeHash
  , UpdateCommitteeHashMessage
  )
import Utils.SerialiseData as Utils.SerialiseData
import Utils.Utxos as Utils.Utxos

committeeHashPolicy ∷ InitCommitteeHashMint → Contract () MintingPolicy
committeeHashPolicy sp = do
  policyUnapplied ← (Transaction.plutusV2Script >>> MintingPolicy) <$>
    TextEnvelope.textEnvelopeBytes
      RawScripts.rawCommitteeHashPolicy
      PlutusScriptV2
  Monad.liftedE (Scripts.applyArgs policyUnapplied [ PlutusData.toData sp ])

updateCommitteeHashValidator ∷ UpdateCommitteeHash → Contract () Validator
updateCommitteeHashValidator sp = do
  validatorUnapplied ← (Transaction.plutusV2Script >>> Validator) <$>
    TextEnvelope.textEnvelopeBytes
      RawScripts.rawCommitteeHashValidator
      PlutusScriptV2
  Monad.liftedE (Scripts.applyArgs validatorUnapplied [ PlutusData.toData sp ])

-- | `initCommitteeHashMintTn` is the token name of the NFT which identifies
-- | the utxo which contains the committee hash. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
initCommitteeHashMintTn ∷ Value.TokenName
initCommitteeHashMintTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

-- | `committeeHashCurSymbol` is the asset class. See `initCommitteeHashMintTn`
-- | for details on the token name
{-# INLINEABLE committeeHashAssetClass #-}
committeeHashAssetClass ∷ InitCommitteeHashMint → Contract () AssetClass
committeeHashAssetClass ichm = do
  cp ← committeeHashPolicy ichm
  curSym ← Monad.liftContractM "Couldn't get committeeHash currency symbol"
    (Value.scriptCurrencySymbol cp)

  pure $ assetClass curSym initCommitteeHashMintTn

-- | `aggregateKeys` aggregates a list of keys s.t. the resulting `ByteArray`
-- | may be stored in the `UpdateCommitteeHashDatum` in an onchain compatible way.
-- | For this to be truly compatible with the onchain function, you need to ensure
-- | that the input list is sorted.
aggregateKeys ∷ Array ByteArray → ByteArray
aggregateKeys = Hashing.blake2b256Hash <<< mconcat

-- | `serialiseUchmHash` is an alias for (ignoring the `Maybe`)
-- | ```
-- | Contract.Hashing.blake2b256Hash <<< Utils.SerialiseData.serialiseToData
-- | ```
-- | The result of this function is what is signed by the committee members.
serialiseUchmHash ∷ UpdateCommitteeHashMessage → Maybe ByteArray
serialiseUchmHash = (Hashing.blake2b256Hash <$> _) <<<
  Utils.SerialiseData.serialiseToData

-- | `findUpdateCommitteeHashUtxo` returns the (unique) utxo which hold the token which
-- | identifies the committee hash.
--
-- Time complexity: bad, it looks at all utxos at the update committee hash
-- validator, then linearly scans through each utxo to determine which has token
findUpdateCommitteeHashUtxo ∷
  UpdateCommitteeHash →
  Contract ()
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
    Value.valueOf value (fst (unwrap uch).uchAssetClass) initCommitteeHashMintTn
      /= zero
