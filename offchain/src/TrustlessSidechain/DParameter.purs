module TrustlessSidechain.DParameter
  ( mkRemoveDParameterLookupsAndConstraints
  , mkUpdateDParameterLookupsAndConstraints
  , mkInsertDParameterLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , fromData
  , toData
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
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.DParameter.Types
  ( DParameterPolicyRedeemer(DParameterBurn, DParameterMint)
  , DParameterValidatorDatum(DParameterValidatorDatum)
  , DParameterValidatorRedeemer(RemoveDParameter, UpdateDParameter)

  )
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, InvalidCLIParams)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils

dParameterTokenName ∷ TokenName
dParameterTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii ""

mkInsertDParameterLookupsAndConstraints ∷
  SidechainParams →
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  } →
  Contract
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

  { lookups: governanceLookups, constraints: governanceConstraints } ←
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
        (Redeemer $ toData DParameterMint)
        dParameterTokenName
        (BigInt.fromInt 1)
        <> Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
          DatumInline
          value
        <> governanceConstraints
  pure { lookups, constraints }

mkRemoveDParameterLookupsAndConstraints ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkRemoveDParameterLookupsAndConstraints sidechainParams = do
  { dParameterCurrencySymbol, dParameterMintingPolicy } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sidechainParams

  let
    dParameterMintingPolicyHash =
      Value.currencyMPSHash dParameterCurrencySymbol

  { dParameterValidatorAddress, dParameterValidator } ←
    DParameter.getDParameterValidatorAndAddress sidechainParams

  -- find all UTxOs at DParameterValidator address that contain DParameterToken
  dParameterUTxOs ←
    Map.filter
      ( \( TransactionOutputWithRefScript
             { output: (TransactionOutput { amount }) }
         ) → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName >
          BigInt.fromInt 0
      )
      <$> utxosAt dParameterValidatorAddress

  -- check how much DParameterToken is stored in UTxOs that we're trying to remove
  let
    amountToBurn = sum
      $ map
          ( \( TransactionOutputWithRefScript
                 { output: (TransactionOutput { amount }) }
             ) → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName
          )
      $ Map.values dParameterUTxOs

  when (amountToBurn == zero)
    $ throwContractError
    $ show
    $ NotFoundUtxo "Unable to remove non-existent d-param"

  { lookups: governanceLookups, constraints: governanceConstraints } ←
    Governance.governanceAuthorityLookupsAndConstraints
      (unwrap sidechainParams).governanceAuthority

  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.validator dParameterValidator
      <> Lookups.mintingPolicy dParameterMintingPolicy
      <> Lookups.unspentOutputs dParameterUTxOs
      <> governanceLookups

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints =
      foldMap
        ( \txInput → Constraints.mustSpendScriptOutput txInput
            (Redeemer $ toData RemoveDParameter)
        ) $ Map.keys dParameterUTxOs

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemer
        dParameterMintingPolicyHash
        (Redeemer $ toData DParameterBurn)
        dParameterTokenName
        (-amountToBurn)
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }

mkUpdateDParameterLookupsAndConstraints ∷
  SidechainParams →
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  } →
  Contract
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
      <$> utxosAt dParameterValidatorAddress

  (oldDParameterInput /\ oldDParameterOutput) ← liftContractM
    ( show
        (NotFoundUtxo "Old D parameter not found")
    )
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
            == registeredCandidatesCount → throwContractError
            ( show
                ( InvalidCLIParams
                    "Provided values have already been set. Please check."
                )
            )
      _ → pure unit
    _ → pure unit

  when (dParameterTokenAmount <= BigInt.fromInt 0) $
    throwContractError
      "No previous DParameter tokens were found. Please insert a new DParameter before trying to update."

  { lookups: governanceLookups, constraints: governanceConstraints } ←
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
      (Redeemer $ toData UpdateDParameter)

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
        DatumInline
        value
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }
