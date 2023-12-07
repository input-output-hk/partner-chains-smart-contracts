module TrustlessSidechain.PermissionedCandidates
  ( mkUpdatePermissionedCandidatesLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( OutputDatum(OutputDatum)
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , mkTxUnspentOut
  )
import Contract.Transaction as Transaction
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Array ((\\))
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.PermissionedCandidates.Types
  ( PermissionedCandidateKeys(PermissionedCandidateKeys)
  , PermissionedCandidatesPolicyRedeemer
      ( PermissionedCandidatesMint
      )
  , PermissionedCandidatesValidatorDatum(PermissionedCandidatesValidatorDatum)
  , PermissionedCandidatesValidatorRedeemer
      ( UpdatePermissionedCandidates
      )
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils
import TrustlessSidechain.Utils.Error
  ( InternalError(InvalidData)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(PermissionedCandidatesPolicy, PermissionedCandidatesValidator)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils
  ( getVersionedCurrencySymbol
  , getVersionedScriptRefUtxo
  , getVersionedValidatorAddress
  ) as Versioning

permissionedCandidatesTokenName ∷ TokenName
permissionedCandidatesTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii ""

mkUpdatePermissionedCandidatesLookupsAndConstraints ∷
  SidechainParams →
  { permissionedCandidatesToAdd ∷
      Array
        { mainchainKey ∷ ByteArray
        , sidechainKey ∷ ByteArray
        , auraKey ∷ ByteArray
        , grandpaKey ∷ ByteArray
        }
  , permissionedCandidatesToRemove ∷
      Maybe
        ( Array
            { mainchainKey ∷ ByteArray
            , sidechainKey ∷ ByteArray
            , auraKey ∷ ByteArray
            , grandpaKey ∷ ByteArray
            }
        )
  } →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkUpdatePermissionedCandidatesLookupsAndConstraints
  sidechainParams
  { permissionedCandidatesToAdd, permissionedCandidatesToRemove } = do
  permissionedCandidatesCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesPolicy }
      )

  let
    permissionedCandidatesMintingPolicyHash =
      Value.currencyMPSHash permissionedCandidatesCurrencySymbol

  permissionedCandidatesValidatorAddress ←
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesValidator }
      )

  permissionedCandidatesValidatorHash ←
    Utils.toValidatorHash permissionedCandidatesValidatorAddress

  -- find the permissioned candidates UTxO
  maybePermissionedCandidatesUTxO ←
    ( Array.find
        ( \( _ /\ TransactionOutputWithRefScript
               { output: (TransactionOutput { amount, datum: outputDatum }) }
           ) → fromMaybe false $ do
            d ← case outputDatum of
              OutputDatum (Datum d) → pure d
              _ → Nothing
            _ ← (fromData d ∷ Maybe PermissionedCandidatesValidatorDatum)
            pure
              ( Value.valueOf amount permissionedCandidatesCurrencySymbol
                  permissionedCandidatesTokenName > BigInt.fromInt 0
              )
        )
        <<< Map.toUnfoldable
    ) <$> utxosAt permissionedCandidatesValidatorAddress

  ( permissionedCandidatesPolicyRefTxInput /\
      permissionedCandidatesPolicyRefTxOutput
  ) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesPolicy }
      )

  ( permissionedCandidatesValidatorScriptRefTxInput /\
      permissionedCandidatesValidatorScriptRefTxOutput
  ) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesValidator }
      )

  { lookups: governanceLookups, constraints: governanceConstraints } ←
    Governance.governanceAuthorityLookupsAndConstraints
      (unwrap sidechainParams).governanceAuthority

  oldCandidates ← case maybePermissionedCandidatesUTxO of
    Nothing → pure []
    Just
      ( _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput { datum: outputDatum } }
      ) →
      maybe
        ( throwContractError $ show $ InternalError $ InvalidData
            "could not decode PermissionedCandidatesValidatorDatum"
        )
        pure $ do
        d ← case outputDatum of
          OutputDatum (Datum d) → pure d
          _ → Nothing
        PermissionedCandidatesValidatorDatum { candidates } ← fromData d
        pure candidates

  let
    filteredCandidates ∷ Array PermissionedCandidateKeys
    filteredCandidates = case permissionedCandidatesToRemove of
      Nothing → []
      Just candidatesToRemove → oldCandidates \\
        (map PermissionedCandidateKeys candidatesToRemove)

    newCandidates ∷ Array PermissionedCandidateKeys
    newCandidates = filteredCandidates <>
      (map PermissionedCandidateKeys permissionedCandidatesToAdd)

    value ∷ Value
    value = Value.singleton
      permissionedCandidatesCurrencySymbol
      permissionedCandidatesTokenName
      one

    permissionedCandidatesDatum ∷ Datum
    permissionedCandidatesDatum = Datum $ toData $
      PermissionedCandidatesValidatorDatum
        { candidates: newCandidates }

    oldUtxoLookups ∷ ScriptLookups Void
    oldUtxoLookups = case maybePermissionedCandidatesUTxO of
      Nothing → mempty
      Just (txInput /\ txOutput) →
        Lookups.unspentOutputs $ Map.singleton txInput txOutput

    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton permissionedCandidatesValidatorScriptRefTxInput
            permissionedCandidatesValidatorScriptRefTxOutput
        )
        <> Lookups.unspentOutputs
          ( Map.singleton permissionedCandidatesPolicyRefTxInput
              permissionedCandidatesPolicyRefTxOutput
          )
        <> oldUtxoLookups
        <> governanceLookups

    validatorRef = RefInput
      ( Transaction.mkTxUnspentOut permissionedCandidatesValidatorScriptRefTxInput
          permissionedCandidatesValidatorScriptRefTxOutput
      )

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints = case maybePermissionedCandidatesUTxO of
      Nothing → mempty
      Just (txInput /\ _) → Constraints.mustSpendScriptOutputUsingScriptRef
        txInput
        (Redeemer $ toData UpdatePermissionedCandidates)
        validatorRef

    mintTokenConstraint = case maybePermissionedCandidatesUTxO of
      Just _ → mempty
      Nothing → Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        permissionedCandidatesMintingPolicyHash
        (Redeemer $ toData PermissionedCandidatesMint)
        permissionedCandidatesTokenName
        (BigInt.fromInt 1)
        ( RefInput $ mkTxUnspentOut permissionedCandidatesPolicyRefTxInput
            permissionedCandidatesPolicyRefTxOutput
        )

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript permissionedCandidatesValidatorHash
        permissionedCandidatesDatum
        DatumInline
        value
        <> Constraints.mustReferenceOutput
          permissionedCandidatesValidatorScriptRefTxInput
        <> Constraints.mustReferenceOutput permissionedCandidatesPolicyRefTxInput
        <> spendScriptOutputConstraints
        <> mintTokenConstraint
        <> governanceConstraints

  pure { lookups, constraints }
