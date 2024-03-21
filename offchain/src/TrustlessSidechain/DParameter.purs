module TrustlessSidechain.DParameter
  ( mkUpdateDParameterLookupsAndConstraints
  , mkInsertDParameterLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( Datum(Datum)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
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
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Except as Run
import TrustlessSidechain.DParameter.Types
  ( DParameterValidatorDatum(DParameterValidatorDatum)
  )
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, InvalidCLIParams, GenericInternalError)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils
import Type.Row (type (+))

dParameterTokenName ∷ TokenName
dParameterTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii ""

mkInsertDParameterLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  } →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkInsertDParameterLookupsAndConstraints
  sidechainParams
  { permissionedCandidatesCount, registeredCandidatesCount } = do
  { dParameterCurrencySymbol, dParameterMintingPolicy } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sidechainParams

  let
    dParameterMintingPolicyHash =
      Value.currencyMPSHash dParameterCurrencySymbol

  { dParameterValidatorAddress } ←
    DParameter.getDParameterValidatorAndAddress sidechainParams

  dParameterValidatorHash ← Utils.toValidatorHash dParameterValidatorAddress

  let
    { lookups: governanceLookups, constraints: governanceConstraints } =
      Governance.governanceAuthorityLookupsAndConstraints
        (unwrap sidechainParams).governanceAuthority

  let
    value ∷ Value
    value = Value.singleton
      dParameterCurrencySymbol
      dParameterTokenName
      (BigInt.fromInt 1)

    dParameterDatum ∷ Datum
    dParameterDatum = Datum $ toData $ DParameterValidatorDatum
      { permissionedCandidatesCount, registeredCandidatesCount }

    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy dParameterMintingPolicy
      <> governanceLookups

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemer
        dParameterMintingPolicyHash
        unitRedeemer
        dParameterTokenName
        (BigInt.fromInt 1)
        <> Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
          DatumInline
          value
        <> governanceConstraints
  pure { lookups, constraints }

mkUpdateDParameterLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkUpdateDParameterLookupsAndConstraints
  sidechainParams
  { permissionedCandidatesCount, registeredCandidatesCount } = do
  { dParameterCurrencySymbol } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sidechainParams

  { dParameterValidatorAddress, dParameterValidator } ←
    DParameter.getDParameterValidatorAndAddress sidechainParams

  dParameterValidatorHash ← Utils.toValidatorHash dParameterValidatorAddress

  -- find one UTxO at DParameterValidator address that contain DParameterToken

  mOldDParameter ←
    ( Array.head
        <<< Map.toUnfoldable
        <<< Map.filter
          ( \( TransactionOutputWithRefScript
                 { output: (TransactionOutput { amount }) }
             ) → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName
              >
                BigInt.fromInt 0
          )
    )
      <$> Effect.utxosAt dParameterValidatorAddress

  (oldDParameterInput /\ oldDParameterOutput) ← Run.note
    (NotFoundUtxo "Old D parameter not found")
    mOldDParameter

  -- check how much DParameterToken is stored in UTxOs that we're trying to
  -- update
  let
    dParameterTokenAmount = case oldDParameterOutput of
      TransactionOutputWithRefScript
        { output: TransactionOutput { amount }
        } → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName

  -- if the old D Parameter is exactly the same as the new one, throw an error
  case oldDParameterOutput of
    TransactionOutputWithRefScript
      { output: TransactionOutput { datum: OutputDatum (Datum d) }
      } → case fromData d of
      Just (DParameterValidatorDatum dParameter)
        | dParameter.permissionedCandidatesCount == permissionedCandidatesCount
            && dParameter.registeredCandidatesCount
            == registeredCandidatesCount → throw
            ( InvalidCLIParams
                "Provided values have already been set. Please check."
            )
      _ → pure unit
    _ → pure unit

  when (dParameterTokenAmount <= BigInt.fromInt 0)
    $ throw
    $ GenericInternalError
        "No previous DParameter tokens were found. Please insert a new DParameter before trying to update."

  let
    { lookups: governanceLookups, constraints: governanceConstraints } =
      Governance.governanceAuthorityLookupsAndConstraints
        (unwrap sidechainParams).governanceAuthority

  let
    value ∷ Value
    value = Value.singleton
      dParameterCurrencySymbol
      dParameterTokenName
      dParameterTokenAmount

    dParameterDatum ∷ Datum
    dParameterDatum = Datum $ toData $ DParameterValidatorDatum
      { permissionedCandidatesCount, registeredCandidatesCount }

    lookups ∷ ScriptLookups Void
    lookups = Lookups.validator dParameterValidator
      <> Lookups.unspentOutputs
        (Map.singleton oldDParameterInput oldDParameterOutput)
      <> governanceLookups

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints = Constraints.mustSpendScriptOutput
      oldDParameterInput
      unitRedeemer

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
        DatumInline
        value
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }
