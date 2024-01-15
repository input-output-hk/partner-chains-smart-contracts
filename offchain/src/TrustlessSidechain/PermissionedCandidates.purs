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
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Array (nub, sort, (\\))
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.Error
  ( InternalError(InvalidData)
  , OffchainError(InternalError, InvalidInputError)
  )
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
import TrustlessSidechain.PermissionedCandidates.Utils as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils

permissionedCandidatesTokenName ∷ TokenName
permissionedCandidatesTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii ""

mkUpdatePermissionedCandidatesLookupsAndConstraints ∷
  SidechainParams →
  { permissionedCandidatesToAdd ∷
      Array
        { sidechainKey ∷ ByteArray
        , auraKey ∷ ByteArray
        , grandpaKey ∷ ByteArray
        }
  , permissionedCandidatesToRemove ∷
      Maybe
        ( Array
            { sidechainKey ∷ ByteArray
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
  { permissionedCandidatesCurrencySymbol, permissionedCandidatesMintingPolicy } ←
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      sidechainParams
  let
    permissionedCandidatesMintingPolicyHash =
      Value.currencyMPSHash permissionedCandidatesCurrencySymbol

  { permissionedCandidatesValidatorAddress, permissionedCandidatesValidator } ←
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress
      sidechainParams

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
    newCandidates = nub
      ( filteredCandidates <>
          (map PermissionedCandidateKeys permissionedCandidatesToAdd)
      )

  when (sort newCandidates == sort oldCandidates)
    $ throwContractError
    $
      ( InvalidInputError
          "New candidates list is the same as the currently stored list."
      )

  let
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
    lookups = Lookups.validator permissionedCandidatesValidator
      <> Lookups.mintingPolicy permissionedCandidatesMintingPolicy
      <> oldUtxoLookups
      <> governanceLookups

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints = case maybePermissionedCandidatesUTxO of
      Nothing → mempty
      Just (txInput /\ _) → Constraints.mustSpendScriptOutput
        txInput
        (Redeemer $ toData UpdatePermissionedCandidates)

    mintTokenConstraint = case maybePermissionedCandidatesUTxO of
      Just _ → mempty
      Nothing → Constraints.mustMintCurrencyWithRedeemer
        permissionedCandidatesMintingPolicyHash
        (Redeemer $ toData PermissionedCandidatesMint)
        permissionedCandidatesTokenName
        (BigInt.fromInt 1)

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript permissionedCandidatesValidatorHash
        permissionedCandidatesDatum
        DatumInline
        value
        <> spendScriptOutputConstraints
        <> mintTokenConstraint
        <> governanceConstraints

  pure { lookups, constraints }
