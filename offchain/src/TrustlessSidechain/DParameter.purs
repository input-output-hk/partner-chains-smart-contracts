module TrustlessSidechain.DParameter
  ( mkUpdateDParameterLookupsAndConstraints
  , mkInsertDParameterLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.ToData (toData)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value as Value
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Data.Array as Array
import Data.Map as Map
import JS.BigInt (BigInt)
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
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import Type.Row (type (+))

dParameterTokenName :: AssetName
dParameterTokenName = emptyAssetName

mkInsertDParameterLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  { permissionedCandidatesCount :: BigInt
  , registeredCandidatesCount :: BigInt
  } ->
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
mkInsertDParameterLookupsAndConstraints
  sidechainParams
  { permissionedCandidatesCount, registeredCandidatesCount } = do
  { dParameterCurrencySymbol, dParameterMintingPolicy } <-
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sidechainParams

  let
    dParameterMintingPolicyHash = dParameterCurrencySymbol

  { dParameterValidator } <-
    DParameter.getDParameterValidatorAndAddress sidechainParams

  let dParameterValidatorHash = PlutusScript.hash dParameterValidator

  let
    { lookups: governanceLookups, constraints: governanceConstraints } =
      Governance.governanceAuthorityLookupsAndConstraints
        (unwrap sidechainParams).governanceAuthority

  let
    value :: Value.Value
    value = Value.singleton
      dParameterCurrencySymbol
      dParameterTokenName
      (BigNum.fromInt 1)

    dParameterDatum :: PlutusData.PlutusData
    dParameterDatum = toData $ DParameterValidatorDatum
      { permissionedCandidatesCount, registeredCandidatesCount }

    lookups :: ScriptLookups
    lookups = Lookups.plutusMintingPolicy dParameterMintingPolicy
      <> governanceLookups

    constraints :: TxConstraints
    constraints =
      Constraints.mustMintCurrencyWithRedeemer
        dParameterMintingPolicyHash
        (RedeemerDatum $ PlutusData.unit)
        dParameterTokenName
        (Int.fromInt 1)
        <> Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
          DatumInline
          value
        <> governanceConstraints
  pure { lookups, constraints }

mkUpdateDParameterLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  { permissionedCandidatesCount :: BigInt
  , registeredCandidatesCount :: BigInt
  } ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
mkUpdateDParameterLookupsAndConstraints
  sidechainParams
  { permissionedCandidatesCount, registeredCandidatesCount } = do
  { dParameterCurrencySymbol } <-
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sidechainParams

  { dParameterValidatorAddress, dParameterValidator } <-
    DParameter.getDParameterValidatorAndAddress sidechainParams

  let dParameterValidatorHash = PlutusScript.hash dParameterValidator

  -- find one UTxO at DParameterValidator address that contain DParameterToken

  mOldDParameter <-
    ( Array.head
        <<< Map.toUnfoldable
        <<< Map.filter
          ( \(TransactionOutput { amount }) ->
              Value.valueOf (Asset dParameterCurrencySymbol dParameterTokenName)
                amount
                > BigNum.fromInt 0
          )
    )
      <$> Effect.utxosAt dParameterValidatorAddress

  (oldDParameterInput /\ oldDParameterOutput) <- Run.note
    (NotFoundUtxo "Old D parameter not found")
    mOldDParameter

  -- check how much DParameterToken is stored in UTxOs that we're trying to
  -- update
  let
    dParameterTokenAmount = case oldDParameterOutput of
      TransactionOutput { amount }
      -> Value.valueOf (Asset dParameterCurrencySymbol dParameterTokenName) amount

  -- if the old D Parameter is exactly the same as the new one, throw an error
  case oldDParameterOutput of
    TransactionOutput { datum: Just (OutputDatum d) }
    -> case fromData d of
      Just (DParameterValidatorDatum dParameter)
        | dParameter.permissionedCandidatesCount == permissionedCandidatesCount
            && dParameter.registeredCandidatesCount
            == registeredCandidatesCount -> throw
            ( InvalidCLIParams
                "Provided values have already been set. Please check."
            )
      _ -> pure unit
    _ -> pure unit

  when (dParameterTokenAmount <= BigNum.fromInt 0)
    $ throw
    $ GenericInternalError
        "No previous DParameter tokens were found. Please insert a new DParameter before trying to update."

  let
    { lookups: governanceLookups, constraints: governanceConstraints } =
      Governance.governanceAuthorityLookupsAndConstraints
        (unwrap sidechainParams).governanceAuthority

  let
    value :: Value.Value
    value = Value.singleton
      dParameterCurrencySymbol
      dParameterTokenName
      dParameterTokenAmount

    dParameterDatum :: PlutusData.PlutusData
    dParameterDatum = toData $ DParameterValidatorDatum
      { permissionedCandidatesCount, registeredCandidatesCount }

    lookups :: ScriptLookups
    lookups = Lookups.validator dParameterValidator
      <> Lookups.unspentOutputs
        (Map.singleton oldDParameterInput oldDParameterOutput)
      <> governanceLookups

    spendScriptOutputConstraints :: TxConstraints
    spendScriptOutputConstraints = Constraints.mustSpendScriptOutput
      oldDParameterInput
      (RedeemerDatum $ PlutusData.unit)

    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
        DatumInline
        value
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }
