-- | 'MPTRoot.Utils' contains utility functions relating to the
-- update committee hash endpoint including:
--
--      - Creating the data for onchain validators / minting policies
--
--      - Querying utxos regarding the update committee hash
--
-- Note: the reason for the existence of this module is because there are some
-- cyclic dependencies between 'MPTRoot' and 'UpdateCommitteeHash' without
-- this.
module MPTRoot.Utils
  ( mptRootTokenMintingPolicy
  , mptRootTokenValidator
  , findMptRootTokenUtxo
  , findLastMptRootTokenUtxo
  , serialiseMrimHash
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
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
import Contract.Value (TokenName)
import Contract.Value as Value
import MPTRoot.Types (MerkleRootInsertionMessage, SignedMerkleRootMint)
import RawScripts as RawScripts
import SidechainParams (SidechainParams)
import Utils.SerialiseData as Utils.SerialiseData
import Utils.Utxos as Utils.Utxos

-- | 'mptRootTokenMintingPolicy' gets the minting policy corresponding to
-- 'RawScripts.rawMPTRootTokenMintingPolicy' paramaterized by the given
-- 'SignedMerkleRootMint'.
mptRootTokenMintingPolicy ∷ SignedMerkleRootMint → Contract () MintingPolicy
mptRootTokenMintingPolicy sp = do
  mptRootMP ← (Transaction.plutusV2Script >>> MintingPolicy) <$>
    TextEnvelope.textEnvelopeBytes
      RawScripts.rawMPTRootTokenMintingPolicy
      PlutusScriptV2
  Monad.liftedE (Scripts.applyArgs mptRootMP [ PlutusData.toData sp ])

-- | 'mptRootTokenValidator' gets the validator corresponding to
-- 'RawScripts.rawMPTRootTokenValidator' paramaterized by 'SidechainParams'.
mptRootTokenValidator ∷ SidechainParams → Contract () Validator
mptRootTokenValidator sp = do
  mptRootVal ← (Transaction.plutusV2Script >>> Validator) <$>
    TextEnvelope.textEnvelopeBytes
      RawScripts.rawMPTRootTokenValidator
      PlutusScriptV2
  Monad.liftedE (Scripts.applyArgs mptRootVal [ PlutusData.toData sp ])

-- | @'findMptRootTokenUtxo' merkleRoot smrm@ locates a utxo which
--
--      1. is sitting at the @'mptRootTokenValidator' smrm.sidechainParams@
--      utxo
--
--      2. contains a token with 'CurrencySymbol' @'mptRootTokenMintingPolicy' smrm@
--      and 'TokenName' as @merkleRoot@.
--
-- Note: in the case that there is more than such utxo, this returns the first
-- such utxo it finds that satisifies the aforementioned properties.
findMptRootTokenUtxo ∷
  TokenName →
  SignedMerkleRootMint →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMptRootTokenUtxo merkleRoot smrm = do
  netId ← Address.getNetworkId
  validator ← mptRootTokenValidator (unwrap smrm).sidechainParams
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← Monad.liftContractM
    "error 'findMptRootTokenUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  mintingPolicy ← mptRootTokenMintingPolicy smrm
  currencySymbol ←
    Monad.liftContractM
      "error 'findMptRootTokenUtxo': failed to get currency symbol for minting policy"
      $ Value.scriptCurrencySymbol mintingPolicy

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: we just need the existence of the token i.e., there is a nonzero
    -- amount
    Value.valueOf value currencySymbol merkleRoot /= zero

-- | @'findLastMptRootTokenUtxo' maybeLastMerkleRoot smrm@ returns 'Nothing' in
-- the case that 'maybeLastMerkleRoot' is 'Nothing', and 'Just' the result of
-- @'findMptRootTokenUtxo' lastMerkleRoot smrm@ provided that @Just lastMerkleRoot = maybeLastMerkleRoot@
-- and there are no other errors.
-- Note: the 'Maybe' return type does NOT denote the absense or existence of
-- finding the utxo... rather it reflects the 'Maybe' in the last merkle root
-- of whether it exists or not.
findLastMptRootTokenUtxo ∷
  Maybe ByteArray →
  SignedMerkleRootMint →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findLastMptRootTokenUtxo maybeLastMerkleRoot smrm = case maybeLastMerkleRoot of
  Nothing → pure Nothing
  Just lastMerkleRoot' → do
    lastMerkleRootTokenName ← Monad.liftContractM
      "error 'saveRoot': invalid lastMerkleRoot token name"
      (Value.mkTokenName lastMerkleRoot')
    lkup ← findMptRootTokenUtxo lastMerkleRootTokenName smrm
    lkup' ←
      Monad.liftContractM
        "error 'findLastMptRootTokenUtxo': failed to find last merkle root" $ lkup
    pure $ Just lkup'

-- | 'serialiseMrimHash' is an alias for  (ignoring the 'Maybe')
-- > 'Contract.Hashing.blake2b256Hash' <<< 'Utils.SerialiseData.serialiseToData'
serialiseMrimHash ∷ MerkleRootInsertionMessage → Maybe ByteArray
serialiseMrimHash = (Hashing.blake2b256Hash <$> _) <<<
  Utils.SerialiseData.serialiseToData
