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
  , findMptRootTokenUtxoByRootHash
  , findPreviousMptRootTokenUtxo
  , serialiseMrimHash
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (MintingPolicy(..), Validator(..))
import Contract.Scripts as Scripts
import Contract.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Transaction as Transaction
import Contract.Value (TokenName)
import Contract.Value as Value
import MPTRoot (SignedMerkleRootMint(..))
import MPTRoot.Types (MerkleRootInsertionMessage, SignedMerkleRootMint)
import MerkleTree (RootHash(..))
import RawScripts as RawScripts
import SidechainParams (SidechainParams(..))
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

findMptRootTokenUtxoByRootHash ∷
  SidechainParams →
  RootHash →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMptRootTokenUtxoByRootHash sidechainParams rootHash = do
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy sidechainParams

  -- Then, we get the mpt root token minting policy..
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  merkleRootTokenName ←
    liftContractM
      (msg "Invalid merkle root TokenName for mptRootTokenMintingPolicy")
      $ Value.mkTokenName merkleRoot
  findMptRootTokenUtxo merkleRootTokenName smrm

-- | @'findPreviousMptRootTokenUtxo' maybeLastMerkleRoot smrm@ returns 'Nothing' in
-- the case that 'maybeLastMerkleRoot' is 'Nothing', and 'Just' the result of
-- @'findMptRootTokenUtxo' lastMerkleRoot smrm@ provided that @Just lastMerkleRoot = maybeLastMerkleRoot@
-- and there are no other errors.
-- Note: the 'Maybe' return type does NOT denote the absense or existence of
-- finding the utxo... rather it reflects the 'Maybe' in the last merkle root
-- of whether it exists or not.
findPreviousMptRootTokenUtxo ∷
  Maybe ByteArray →
  SignedMerkleRootMint →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findPreviousMptRootTokenUtxo maybeLastMerkleRoot smrm =
  case maybeLastMerkleRoot of
    Nothing → pure Nothing
    Just lastMerkleRoot' → do
      lastMerkleRootTokenName ← Monad.liftContractM
        "error 'saveRoot': invalid lastMerkleRoot token name"
        (Value.mkTokenName lastMerkleRoot')
      lkup ← findMptRootTokenUtxo lastMerkleRootTokenName smrm
      lkup' ←
        Monad.liftContractM
          "error 'findPreviousMptRootTokenUtxo': failed to find last merkle root"
          $ lkup
      pure $ Just lkup'

-- | 'serialiseMrimHash' is an alias for (ignoring the 'Maybe')
-- | ```purescript
-- | Contract.Hashing.blake2b256Hash <<< Utils.SerialiseData.serialiseToData
-- | ```
serialiseMrimHash ∷ MerkleRootInsertionMessage → Maybe ByteArray
serialiseMrimHash = (Hashing.blake2b256Hash <$> _) <<<
  Utils.SerialiseData.serialiseToData

-- | 'getCommitteeHashPolicy' grabs the committee hash policy and currency symbol
-- (potentially throwing an error in the case that it is not possible).
getCommitteeHashPolicy ∷
  SidechainParams →
  Contract ()
    { committeeHashPolicy ∷ MintingPolicy
    , committeeHashCurrencySymbol ∷ CurrencySymbol
    }
getCommitteeHashPolicy (SidechainParams sp) = do
  let
    msg = Logging.mkReport
      { mod: "FUELMintingPolicy", fun: "getCommitteeHashPolicy" }
  committeeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: sp.initUtxo }
  committeeHashCurrencySymbol ← liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)
  pure { committeeHashPolicy, committeeHashCurrencySymbol }

-- | 'getMptRootTokenPolicy' grabs the mpt root token policy and currency
-- symbol (potentially throwing an error if this is not possible).
getMptRootTokenPolicy ∷
  SidechainParams →
  Contract
    ()
    { mptRootTokenMintingPolicy ∷ MintingPolicy
    , mptRootTokenMintingPolicyCurrencySymbol ∷ CurrencySymbol
    }
getMptRootTokenPolicy sidechainParams = do
  let
    msg = Logging.mkReport
      { mod: "FUELMintingPolicy", fun: "getMptRootTokenPolicy" }

  -- some awkwardness that we need the committee hash policy first.
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy sidechainParams

  -- Then, we get the mpt root token minting policy..
  mptRootTokenMintingPolicy ← MPTRoot.mptRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  mptRootTokenMintingPolicyCurrencySymbol ←
    liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol mptRootTokenMintingPolicy

  pure { mptRootTokenMintingPolicy, mptRootTokenMintingPolicyCurrencySymbol }
