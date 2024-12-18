module TrustlessSidechain.PermissionedCandidates
  ( mkUpdatePermissionedCandidatesLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.ToData (toData)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.BigInt as BigInt
import Cardano.Types.Int as Int
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Array (nub, sort, (\\))
import Data.Array as Array
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Except as Run
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(InvalidData, InvalidCLIParams)
  )
import TrustlessSidechain.Governance.Utils as Governance
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
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import TrustlessSidechain.Utils.Data
  ( VersionedGenericDatum(VersionedGenericDatum)
  )
import Type.Row (type (+))

permissionedCandidatesTokenName :: TokenName
permissionedCandidatesTokenName = emptyAssetName

mkUpdatePermissionedCandidatesLookupsAndConstraints ::
  forall r.
  TransactionInput ->
  { permissionedCandidatesToAdd ::
      Array
        { sidechainKey :: ByteArray
        , auraKey :: ByteArray
        , grandpaKey :: ByteArray
        }
  , permissionedCandidatesToRemove ::
      Maybe
        ( Array
            { sidechainKey :: ByteArray
            , auraKey :: ByteArray
            , grandpaKey :: ByteArray
            }
        )
  } ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
mkUpdatePermissionedCandidatesLookupsAndConstraints
  genesisUtxo
  { permissionedCandidatesToAdd, permissionedCandidatesToRemove } = do
  { permissionedCandidatesCurrencySymbol, permissionedCandidatesMintingPolicy } <-
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      genesisUtxo

  { permissionedCandidatesValidatorAddress, permissionedCandidatesValidator } <-
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress
      genesisUtxo

  let
    permissionedCandidatesValidatorHash = PlutusScript.hash
      permissionedCandidatesValidator

  -- find the permissioned candidates UTxO
  maybePermissionedCandidatesUTxO <-
    ( Array.find
        ( \(_ /\ TransactionOutput { amount, datum: outputDatum }) ->
            fromMaybe false $ do
              d <- case outputDatum of
                Just (OutputDatum d) -> pure d
                _ -> Nothing
              VersionedGenericDatum { builtinData } :: VersionedGenericDatum Unit <-
                fromData d
              _ :: PermissionedCandidatesValidatorDatum <- fromData builtinData
              pure
                ( Value.valueOf
                    ( Asset
                        permissionedCandidatesCurrencySymbol
                        permissionedCandidatesTokenName
                    )
                    amount
                    > BigNum.fromInt 0
                )
        )
        <<< Map.toUnfoldable
    ) <$> Effect.utxosAt permissionedCandidatesValidatorAddress

  { lookups: governanceLookups, constraints: governanceConstraints } <-
    Governance.approvedByGovernanceLookupsAndConstraints genesisUtxo

  oldCandidates <- case maybePermissionedCandidatesUTxO of
    Nothing -> pure []
    Just
      ( _ /\ TransactionOutput { datum: outputDatum }
      ) ->
      Run.note
        ( InvalidData
            "could not decode PermissionedCandidatesValidatorDatum"
        )
        $ do
            d <- case outputDatum of
              Just (OutputDatum d) -> pure d
              _ -> Nothing
            VersionedGenericDatum { builtinData } :: VersionedGenericDatum Unit <-
              fromData d
            PermissionedCandidatesValidatorDatum { candidates } <- fromData
              builtinData
            pure candidates

  let
    filteredCandidates :: Array PermissionedCandidateKeys
    filteredCandidates = case permissionedCandidatesToRemove of
      Nothing -> []
      Just candidatesToRemove -> oldCandidates \\
        (map PermissionedCandidateKeys candidatesToRemove)

    newCandidates :: Array PermissionedCandidateKeys
    newCandidates = nub
      ( filteredCandidates <>
          (map PermissionedCandidateKeys permissionedCandidatesToAdd)
      )

  when (sort newCandidates == sort oldCandidates)
    $ throw
        ( InvalidCLIParams
            "New candidates list is the same as the currently stored list."
        )

  let
    value :: Value
    value = Value.singleton
      permissionedCandidatesCurrencySymbol
      permissionedCandidatesTokenName
      (BigNum.fromInt 1)

    permissionedCandidatesDatum :: PlutusData
    permissionedCandidatesDatum = toData $
      PermissionedCandidatesValidatorDatum
        { candidates: newCandidates }

    datum = toData $ VersionedGenericDatum
      { datum: unit
      , builtinData: permissionedCandidatesDatum
      , version: BigInt.fromInt 0
      }

    oldUtxoLookups :: ScriptLookups
    oldUtxoLookups = case maybePermissionedCandidatesUTxO of
      Nothing -> mempty
      Just (txInput /\ txOutput) ->
        Lookups.unspentOutputs $ Map.singleton txInput txOutput

    lookups :: ScriptLookups
    lookups = Lookups.validator permissionedCandidatesValidator
      <> Lookups.plutusMintingPolicy permissionedCandidatesMintingPolicy
      <> oldUtxoLookups
      <> governanceLookups

    spendScriptOutputConstraints :: TxConstraints
    spendScriptOutputConstraints = case maybePermissionedCandidatesUTxO of
      Nothing -> mempty
      Just (txInput /\ _) -> Constraints.mustSpendScriptOutput
        txInput
        (RedeemerDatum $ toData UpdatePermissionedCandidates)

    mintTokenConstraint = case maybePermissionedCandidatesUTxO of
      Just _ -> mempty
      Nothing -> Constraints.mustMintCurrencyWithRedeemer
        permissionedCandidatesCurrencySymbol
        (RedeemerDatum $ toData PermissionedCandidatesMint)
        permissionedCandidatesTokenName
        (Int.fromInt 1)

    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript permissionedCandidatesValidatorHash
        datum
        DatumInline
        value
        <> spendScriptOutputConstraints
        <> mintTokenConstraint
        <> governanceConstraints

  pure $ { lookups, constraints }
