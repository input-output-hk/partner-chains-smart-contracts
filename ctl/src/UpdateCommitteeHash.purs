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
import Data.BigInt as BigInt
import Data.Foldable (find)
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
  let
    uch = { uchAssetClass: assetClass cs tn }
    -- show is our version of (getLedgerBytes . getPubKey)
    newCommitteeHash = aggregateKeys uchp.newCommitteePubKeys
    curCommitteeHash = aggregateKeys uchp.committeePubKeys
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
          { committeeSignatures: uchp.committeeSignatures
          , committeePubKeys: uchp.committeePubKeys
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

  logInfo' (show tOut)
  logInfo' (show value)
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx"
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' "Submitted updateCommitteeHash transaction!"
  awaitTxConfirmed txId
  logInfo' "updateCommitteeHash transaction submitted successfully!"

aggregateKeys ∷ Array ByteArray → ByteArray
aggregateKeys ls = MT.unRootHash $ MT.rootHash
  (MT.fromList (toUnfoldable ls))
