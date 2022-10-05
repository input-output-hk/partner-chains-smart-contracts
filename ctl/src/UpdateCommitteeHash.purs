module UpdateCommitteeHash
  ( UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , committeeHashPolicy
  , updateCommitteeHashValidator
  , initCommitteeHashMintTn
  , committeeHashAssetClass
  , findUpdateCommitteeHashUtxo
  , updateCommitteeHash
  , aggregateKeys
  ) where

import Contract.Prelude

import BalanceTx.Extra (reattachDatumsInline)
import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractE
  , liftContractM
  , liftedE
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray
  ( ByteArray
  , hexToByteArrayUnsafe
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array (toUnfoldable)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(..))
import MerkleTree as MT
import Partial.Unsafe (unsafePartial)
import RawScripts (rawCommitteeHashPolicy, rawCommitteeHashValidator)
import SidechainParams (SidechainParams(..))
import Types
  ( AssetClass
  , PubKey
  , Signature
  , assetClass
  , assetClassValue
  )
import Types.Datum (Datum(..))
import Types.OutputDatum (outputDatumDatum)
import Types.Redeemer (Redeemer(..))
import Types.Scripts (plutusV2Script)
import Utils.Crypto as Utils.Crypto
import Utils.Utxos as Utils.Utxos

newtype UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash ∷ ByteArray }

derive instance Generic UpdateCommitteeHashDatum _
derive instance Newtype UpdateCommitteeHashDatum _
instance ToData UpdateCommitteeHashDatum where
  toData (UpdateCommitteeHashDatum { committeeHash }) = Constr zero
    [ toData committeeHash ]

instance FromData UpdateCommitteeHashDatum where
  fromData (Constr n [ a ])
    | n == zero = UpdateCommitteeHashDatum <$> ({ committeeHash: _ }) <$>
        fromData a
  fromData _ = Nothing

-- plutus script is parameterised on AssetClass, which CTL doesn't have
-- the toData instance uses the underlying tuple so we do the same
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { uchAssetClass ∷ AssetClass }

derive instance Generic UpdateCommitteeHash _
derive instance Newtype UpdateCommitteeHash _
instance ToData UpdateCommitteeHash where
  toData (UpdateCommitteeHash { uchAssetClass }) = Constr zero
    [ toData uchAssetClass ]

newtype InitCommitteeHashMint = InitCommitteeHashMint
  { icTxOutRef ∷ TransactionInput }

derive instance Generic InitCommitteeHashMint _
derive instance Newtype InitCommitteeHashMint _
instance ToData InitCommitteeHashMint where
  toData (InitCommitteeHashMint { icTxOutRef }) =
    toData icTxOutRef

data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { committeeSignatures ∷ Array Signature
  , committeePubKeys ∷ Array PubKey
  , newCommitteeHash ∷ ByteArray
  }

derive instance Generic UpdateCommitteeHashRedeemer _
instance ToData UpdateCommitteeHashRedeemer where
  toData
    ( UpdateCommitteeHashRedeemer
        { committeeSignatures, committeePubKeys, newCommitteeHash }
    ) = Constr zero
    [ toData committeeSignatures
    , toData committeePubKeys
    , toData newCommitteeHash
    ]

-- | 'UpdateCommitteeHashParams' is the parameter for the update committee hash endpoint.
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { sidechainParams ∷ SidechainParams
  , newCommitteePubKeys ∷ Array PubKey
  , committeeSignatures ∷ Array (PubKey /\ Maybe Signature)
  , sidechainEpoch ∷ BigInt
  , lastMerkleRoot ∷ Maybe ByteArray
  }

committeeHashPolicy ∷ InitCommitteeHashMint → Contract () MintingPolicy
committeeHashPolicy sp = do
  policyUnapplied ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawCommitteeHashPolicy
    PlutusScriptV2
  liftedE (Scripts.applyArgs policyUnapplied [ toData sp ])

updateCommitteeHashValidator ∷ UpdateCommitteeHash → Contract () Validator
updateCommitteeHashValidator sp = do
  validatorUnapplied ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawCommitteeHashValidator
    PlutusScriptV2
  liftedE (Scripts.applyArgs validatorUnapplied [ toData sp ])

{- | 'initCommitteeHashMintTn'  is the token name of the NFT which identifies
 the utxo which contains the committee hash. We use an empty bytestring for
 this because the name really doesn't matter, so we mighaswell save a few
 bytes by giving it the empty name.
-}
initCommitteeHashMintTn ∷ Value.TokenName
initCommitteeHashMintTn = unsafePartial $ fromJust $ Value.mkTokenName $
  hexToByteArrayUnsafe ""

{- | 'committeeHashCurSymbol' is the asset class. See 'initCommitteeHashMintTn'
 for details on the token name
-}
{-# INLINEABLE committeeHashAssetClass #-}
committeeHashAssetClass ∷ InitCommitteeHashMint → Contract () AssetClass
committeeHashAssetClass ichm = do
  cp ← committeeHashPolicy ichm
  curSym ← liftContractM "Couldn't get committeeHash currency symbol"
    (Value.scriptCurrencySymbol cp)

  pure $ assetClass curSym initCommitteeHashMintTn

-- | 'updateCommitteeHashUtxos' returns the (unique) utxo which hold the token which
-- identifies the committee hash.
--
-- Time complexity: bad, it looks at all utxos at the update committee hash
-- validator, then linearly scans through each utxo to determine which has token
findUpdateCommitteeHashUtxo ∷
  UpdateCommitteeHash →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUpdateCommitteeHashUtxo uch = do
  netId ← getNetworkId
  validator ← updateCommitteeHashValidator uch
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← liftContractM
    "error 'findUpdateCommitteeHashUtxo': failed to get validator address"
    (validatorHashEnterpriseAddress netId validatorHash)

  Utils.Utxos.findUtxoByValueAt validatorAddress \value →
    -- Note: there should either be 0 or 1 tokens of this committee hash nft.
    Value.valueOf value (fst (unwrap uch).uchAssetClass) initCommitteeHashMintTn
      /= zero

-- N.B. on-chain code verifies the datum is contained in the output -- see Note [Committee hash in output datum]
-- | 'updateCommitteeHash' is the endpoint to submit the transaction to update the committee hash.
-- check if we have the right committee. This gets checked on chain also
updateCommitteeHash ∷ UpdateCommitteeHashParams → Contract () Unit
updateCommitteeHash (UpdateCommitteeHashParams uchp) = do
  pol ← committeeHashPolicy
    ( InitCommitteeHashMint
        { icTxOutRef: (\(SidechainParams x) → x.genesisUtxo) uchp.sidechainParams
        }
    )

  cs ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol pol)

  let tn = initCommitteeHashMintTn

  when (null uchp.committeeSignatures) (throwContractError "Empty Committee")
  let uch = UpdateCommitteeHash { uchAssetClass: assetClass cs tn }

  newCommitteeHash ← liftContractE $ aggregateKeys $ Array.sort
    uchp.newCommitteePubKeys

  let
    curCommitteePubKeys /\ committeeSignatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures uchp.committeeSignatures
  curCommitteeHash ← liftContractE $ aggregateKeys curCommitteePubKeys

  updateValidator ← updateCommitteeHashValidator uch
  let valHash = Scripts.validatorHash updateValidator

  lkup ← findUpdateCommitteeHashUtxo uch
  { index: oref
  , value: (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    liftContractM "error 'updateCommitteeHash': failed to find token" $ lkup

  rawDatum ← liftContractM "No inline datum found" (outputDatumDatum tOut.datum)
  UpdateCommitteeHashDatum datum ← liftContractM "cannot get datum"
    (fromData $ unwrap rawDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError "incorrect committee provided")
  let
    newDatum = Datum $ toData
      (UpdateCommitteeHashDatum { committeeHash: newCommitteeHash })
    value = assetClassValue (unwrap uch).uchAssetClass (BigInt.fromInt 1)
    redeemer = Redeemer $ toData
      ( UpdateCommitteeHashRedeemer
          { committeeSignatures
          , committeePubKeys: curCommitteePubKeys
          , newCommitteeHash: newCommitteeHash
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton oref
            ( TransactionOutputWithRefScript
                { output: TransactionOutput tOut, scriptRef: Nothing }
            )
        )
        <> Lookups.validator updateValidator
    constraints = Constraints.mustSpendScriptOutput oref redeemer
      <> Constraints.mustPayToScript valHash newDatum DatumWitness value

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx"
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' "Submitted updateCommitteeHash transaction!"
  awaitTxConfirmed txId
  logInfo' "updateCommitteeHash transaction submitted successfully!"

-- | 'aggregateKeys' aggregates a list of keys s.t. the resulting 'ByteArray'
-- may be stored in the 'UpdateCommitteeHashDatum' in an onchain compatible way.
aggregateKeys ∷ Array ByteArray → Either String ByteArray
aggregateKeys ls =
  (MT.fromList (toUnfoldable ls) <#> MT.rootHash >>> MT.unRootHash)
