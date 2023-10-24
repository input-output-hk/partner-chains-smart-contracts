module TrustlessSidechain.DParameter
  ( mkRemoveDParameterLookupsAndConstraints
  , mkUpdateDParameterLookupsAndConstraints
  , mkInsertDParameterLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionOutput(TransactionOutput)
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
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils
import TrustlessSidechain.Versioning.Types
  ( ScriptId(DParameterPolicy, DParameterValidator)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils
  ( getVersionedCurrencySymbol
  , getVersionedScriptRefUtxo
  , getVersionedValidatorAddress
  ) as Versioning

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
  dParameterCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
      )

  let
    dParameterMintingPolicyHash =
      Value.currencyMPSHash dParameterCurrencySymbol

  dParameterValidatorAddress ←
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

  dParameterValidatorHash ← Utils.toValidatorHash dParameterValidatorAddress

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
    )

  (dParameterValidatorScriptRefTxInput /\ dParameterValidatorScriptRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

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
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton dParameterValidatorScriptRefTxInput
            dParameterValidatorScriptRefTxOutput
        )
        <> Lookups.unspentOutputs
          (Map.singleton scriptRefTxInput scriptRefTxOutput)
        <> governanceLookups

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        dParameterMintingPolicyHash
        (Redeemer $ toData DParameterMint)
        dParameterTokenName
        (BigInt.fromInt 1)
        (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
        <> Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
          DatumInline
          value
        <> Constraints.mustReferenceOutput dParameterValidatorScriptRefTxInput
        <> governanceConstraints
  pure { lookups, constraints }

mkRemoveDParameterLookupsAndConstraints ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkRemoveDParameterLookupsAndConstraints sidechainParams = do
  dParameterCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
      )

  let
    dParameterMintingPolicyHash =
      Value.currencyMPSHash dParameterCurrencySymbol

  dParameterValidatorAddress ←
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

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

  (dParameterPolicyRefTxInput /\ dParameterPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
      )

  (dParameterValidatorScriptRefTxInput /\ dParameterValidatorScriptRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

  { lookups: governanceLookups, constraints: governanceConstraints } ←
    Governance.governanceAuthorityLookupsAndConstraints
      (unwrap sidechainParams).governanceAuthority

  let
    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton dParameterValidatorScriptRefTxInput
            dParameterValidatorScriptRefTxOutput
        )
        <> Lookups.unspentOutputs
          (Map.singleton dParameterPolicyRefTxInput dParameterPolicyRefTxOutput)
        <> Lookups.unspentOutputs dParameterUTxOs
        <> governanceLookups

    validatorRef = RefInput
      ( Transaction.mkTxUnspentOut dParameterValidatorScriptRefTxInput
          dParameterValidatorScriptRefTxOutput
      )

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints =
      foldMap
        ( \txInput → Constraints.mustSpendScriptOutputUsingScriptRef txInput
            (Redeemer $ toData RemoveDParameter)
            validatorRef
        ) $ Map.keys dParameterUTxOs

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        dParameterMintingPolicyHash
        (Redeemer $ toData DParameterBurn)
        dParameterTokenName
        (-amountToBurn)
        ( RefInput $ mkTxUnspentOut dParameterPolicyRefTxInput
            dParameterPolicyRefTxOutput
        )
        <> Constraints.mustReferenceOutput dParameterValidatorScriptRefTxInput
        <> Constraints.mustReferenceOutput dParameterPolicyRefTxInput
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
  dParameterCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
      )

  dParameterValidatorAddress ←
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

  dParameterValidatorHash ← Utils.toValidatorHash dParameterValidatorAddress

  -- find all UTxOs at DParameterValidator address that contain DParameterToken
  dParameterUTxOs ←
    Map.filter
      ( \( TransactionOutputWithRefScript
             { output: (TransactionOutput { amount }) }
         ) → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName >
          BigInt.fromInt 0
      )
      <$> utxosAt dParameterValidatorAddress

  -- check how much DParameterToken is stored in UTxOs that we're trying to
  -- update
  let
    dParameterTokenAmount = sum
      $ map
          ( \( TransactionOutputWithRefScript
                 { output: (TransactionOutput { amount }) }
             ) → Value.valueOf amount dParameterCurrencySymbol dParameterTokenName
          )
      $ Map.values dParameterUTxOs

  (dParameterPolicyRefTxInput /\ dParameterPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterPolicy }
      )

  (dParameterValidatorScriptRefTxInput /\ dParameterValidatorScriptRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: DParameterValidator }
      )

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
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton dParameterValidatorScriptRefTxInput
            dParameterValidatorScriptRefTxOutput
        )
        <> Lookups.unspentOutputs
          (Map.singleton dParameterPolicyRefTxInput dParameterPolicyRefTxOutput)
        <> Lookups.unspentOutputs dParameterUTxOs
        <> governanceLookups

    validatorRef = RefInput
      ( Transaction.mkTxUnspentOut dParameterValidatorScriptRefTxInput
          dParameterValidatorScriptRefTxOutput
      )

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints =
      foldMap
        ( \txInput → Constraints.mustSpendScriptOutputUsingScriptRef txInput
            (Redeemer $ toData UpdateDParameter)
            validatorRef
        ) $ Map.keys dParameterUTxOs

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript dParameterValidatorHash dParameterDatum
        DatumInline
        value
        <> Constraints.mustReferenceOutput dParameterValidatorScriptRefTxInput
        <> Constraints.mustReferenceOutput dParameterPolicyRefTxInput
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }
