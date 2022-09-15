module UpdateCommitteeHash where

import Contract.Prelude

import BalanceTx.Extra (reattachDatumsInline)
import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
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
  , byteArrayFromAscii
  , hexToByteArrayUnsafe
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(..)
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (toUnfoldable)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.BigInt as BigInt
import Data.Foldable (find)
import Data.List (List, (:))
import Data.List as List
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
  , assetClassValueOf
  )
import Types.Datum (Datum(..))
import Types.OutputDatum (outputDatumDatum)
import Types.Redeemer (Redeemer(..))
import Types.Scripts (plutusV2Script)
import Utils.Crypto (verifyEd25519Signature)

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

data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { sidechainParams ∷ SidechainParams
  , newCommitteePubKeys ∷ Array PubKey
  , committeeSignatures ∷ Array Signature
  , committeePubKeys ∷ Array PubKey
  }

derive instance Generic UpdateCommitteeHashParams _
instance ToData UpdateCommitteeHashParams where
  toData
    ( UpdateCommitteeHashParams
        { sidechainParams
        , newCommitteePubKeys
        , committeeSignatures
        , committeePubKeys
        }
    ) = Constr zero
    [ toData sidechainParams
    , toData newCommitteePubKeys
    , toData committeeSignatures
    , toData committeePubKeys
    ]

committeeHashPolicy ∷ InitCommitteeHashMint → Contract () MintingPolicy
committeeHashPolicy sp = do
  policyUnapplied ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawCommitteeHashPolicy
    PlutusScriptV2
  liftedE (applyArgs policyUnapplied [ toData sp ])

updateCommitteeHashValidator ∷ UpdateCommitteeHash → Contract () Validator
updateCommitteeHashValidator sp = do
  validatorUnapplied ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawCommitteeHashValidator
    PlutusScriptV2
  liftedE (applyArgs validatorUnapplied [ toData sp ])

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

  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName =<< byteArrayFromAscii "") -- TODO init token name?

  when (null uchp.committeePubKeys) (throwContractError "Empty Committee")
  let uch = { uchAssetClass: assetClass cs tn }

  let newCommitteePubKeys = uchp.newCommitteePubKeys
  unless (isSorted newCommitteePubKeys) $ throwContractError
    "New committee member public keys must be sorted"

  newCommitteeHash ← aggregateKeys newCommitteePubKeys

  let
    curCommitteePubKeys /\ committeeSignatures = sortPubKeysAndSigs
      newCommitteeHash
      uchp.committeePubKeys
      uchp.committeeSignatures
  curCommitteeHash ← aggregateKeys curCommitteePubKeys

  updateValidator ← updateCommitteeHashValidator (UpdateCommitteeHash uch)
  let valHash = validatorHash updateValidator

  netId ← getNetworkId
  valAddr ← liftContractM "updateCommitteeHash: get validator address"
    (validatorHashEnterpriseAddress netId valHash)
  scriptUtxos ← unwrap <$> liftedM "Cannot get script utxos" (utxosAt valAddr)
  let
    findOwnValue (_tIN /\ tOUT) =
      assetClassValueOf ((unwrap tOUT).amount) uch.uchAssetClass ==
        BigInt.fromInt 1
    found = find findOwnValue
      ( Map.toUnfoldable scriptUtxos ∷
          (Array (TransactionInput /\ TransactionOutput))
      )
  (oref /\ (TransactionOutput tOut)) ← liftContractM
    "updateCommittee hash output not found"
    found

  rawDatum ← liftContractM "No inline datum found" (outputDatumDatum tOut.datum)
  UpdateCommitteeHashDatum datum ← liftContractM "cannot get datum"
    (fromData $ unwrap rawDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError "incorrect committee provided")
  let
    newDatum = Datum $ toData
      (UpdateCommitteeHashDatum { committeeHash: newCommitteeHash })
    value = assetClassValue uch.uchAssetClass (BigInt.fromInt 1)
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
        (Map.singleton oref (TransactionOutput tOut))
        <> Lookups.validator updateValidator
    constraints = Constraints.mustSpendScriptOutput oref redeemer
      <> Constraints.mustPayToScript valHash newDatum value

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx"
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' "Submitted updateCommitteeHash transaction!"
  awaitTxConfirmed txId
  logInfo' "updateCommitteeHash transaction submitted successfully!"

aggregateKeys ∷ Array ByteArray → Contract () ByteArray
aggregateKeys ls = liftAff
  (MT.fromList (toUnfoldable ls) <#> MT.rootHash >>> MT.unRootHash)

{-| Sorting public key and signatures pairs by public keys to be able to efficiently nub them on-chain -}
sortPubKeysAndSigs ∷
  ByteArray →
  Array PubKey →
  Array Signature →
  Tuple (Array PubKey) (Array Signature)
sortPubKeysAndSigs msg pks sigs =
  pairSigs (Array.toUnfoldable pks) (Array.toUnfoldable sigs)
    # List.sortBy (\x y → fst x `compare` fst y)
    # Array.fromFoldable
    # Array.unzip
    # rmap Array.catMaybes
  where
  pairSigs ∷ List PubKey → List Signature → List (Tuple PubKey (Maybe Signature))
  pairSigs List.Nil _ = List.Nil
  pairSigs pks' List.Nil = map (_ /\ Nothing) pks'
  pairSigs (pk : pks') (sig : sigs')
    | verifyEd25519Signature pk msg sig =
        (pk /\ Just sig) : pairSigs pks' sigs'
    | otherwise =
        (pk /\ Nothing) : pairSigs pks' (sig : sigs')

{- | Verifies that the non empty array is sorted -}
isSorted ∷ ∀ a. Ord a ⇒ Array a → Boolean
isSorted xss = case Array.tail xss of
  Just xs → and (Array.zipWith (<) xss xs) -- insert (<) between all elements
  Nothing → false
