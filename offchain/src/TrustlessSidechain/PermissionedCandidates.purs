module TrustlessSidechain.PermissionedCandidates
  ( mkInsertPermissionedCandidatesLookupsAndConstraints
  , mkRemovePermissionedCandidatesLookupsAndConstraints
  , mkUpdatePermissionedCandidatesLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)

  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
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
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.PermissionedCandidates.Types
  ( PermissionedCandidateKeys(PermissionedCandidateKeys)
  , PermissionedCandidatesPolicyRedeemer
      ( PermissionedCandidatesMint
      , PermissionedCandidatesBurn
      )
  , PermissionedCandidatesValidatorDatum(PermissionedCandidatesValidatorDatum)
  , PermissionedCandidatesValidatorRedeemer
      ( RemovePermissionedCandidates
      , UpdatePermissionedCandidates
      )
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toValidatorHash) as Utils
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

mkInsertPermissionedCandidatesLookupsAndConstraints ∷
  SidechainParams →
  { candidates ∷
      Array
        { mainchainKey ∷ ByteArray
        , sidechainKey ∷ ByteArray
        , authorityDiscoveryKey ∷ ByteArray
        , grandpaKey ∷ ByteArray
        }

  } →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkInsertPermissionedCandidatesLookupsAndConstraints
  sidechainParams
  { candidates } = do
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

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
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

  let
    value ∷ Value
    value = Value.singleton
      permissionedCandidatesCurrencySymbol
      permissionedCandidatesTokenName
      (BigInt.fromInt 1)

    permissionedCandidatesDatum ∷ Datum
    permissionedCandidatesDatum = Datum $ toData $
      PermissionedCandidatesValidatorDatum
        { candidates: map PermissionedCandidateKeys candidates }

    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton permissionedCandidatesValidatorScriptRefTxInput
            permissionedCandidatesValidatorScriptRefTxOutput
        )
        <> Lookups.unspentOutputs
          (Map.singleton scriptRefTxInput scriptRefTxOutput)
        <> governanceLookups

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        permissionedCandidatesMintingPolicyHash
        (Redeemer $ toData PermissionedCandidatesMint)
        permissionedCandidatesTokenName
        (BigInt.fromInt 1)
        (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
        <> Constraints.mustPayToScript permissionedCandidatesValidatorHash
          permissionedCandidatesDatum
          DatumInline
          value
        <> Constraints.mustReferenceOutput
          permissionedCandidatesValidatorScriptRefTxInput
        <> governanceConstraints
  pure { lookups, constraints }

mkRemovePermissionedCandidatesLookupsAndConstraints ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkRemovePermissionedCandidatesLookupsAndConstraints sidechainParams = do
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

  -- find all UTxOs at permissionedCandidatesValidator address that contain permissionedCandidatesToken
  permissionedCandidatesUTxOs ←
    Map.filter
      ( \( TransactionOutputWithRefScript
             { output: (TransactionOutput { amount }) }
         ) →
          Value.valueOf amount permissionedCandidatesCurrencySymbol
            permissionedCandidatesTokenName >
            BigInt.fromInt 0
      )
      <$> utxosAt permissionedCandidatesValidatorAddress

  -- check how much permissionedCandidatesToken is stored in UTxOs that we're trying to remove
  let
    amountToBurn = sum
      $ map
          ( \( TransactionOutputWithRefScript
                 { output: (TransactionOutput { amount }) }
             ) → Value.valueOf amount permissionedCandidatesCurrencySymbol
              permissionedCandidatesTokenName
          )
      $ Map.values permissionedCandidatesUTxOs

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

  let
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
        <> Lookups.unspentOutputs permissionedCandidatesUTxOs
        <> governanceLookups

    validatorRef = RefInput
      ( Transaction.mkTxUnspentOut permissionedCandidatesValidatorScriptRefTxInput
          permissionedCandidatesValidatorScriptRefTxOutput
      )

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints =
      foldMap
        ( \txInput → Constraints.mustSpendScriptOutputUsingScriptRef txInput
            (Redeemer $ toData RemovePermissionedCandidates)
            validatorRef
        ) $ Map.keys permissionedCandidatesUTxOs

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        permissionedCandidatesMintingPolicyHash
        (Redeemer $ toData PermissionedCandidatesBurn)
        permissionedCandidatesTokenName
        (-amountToBurn)
        ( RefInput $ mkTxUnspentOut permissionedCandidatesPolicyRefTxInput
            permissionedCandidatesPolicyRefTxOutput
        )
        <> Constraints.mustReferenceOutput
          permissionedCandidatesValidatorScriptRefTxInput
        <> Constraints.mustReferenceOutput permissionedCandidatesPolicyRefTxInput
        <> spendScriptOutputConstraints
        <> governanceConstraints

  pure { lookups, constraints }

mkUpdatePermissionedCandidatesLookupsAndConstraints ∷
  SidechainParams →
  { candidates ∷
      Array
        { mainchainKey ∷ ByteArray
        , sidechainKey ∷ ByteArray
        , authorityDiscoveryKey ∷ ByteArray
        , grandpaKey ∷ ByteArray
        }

  } →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkUpdatePermissionedCandidatesLookupsAndConstraints
  sidechainParams
  { candidates } = do
  permissionedCandidatesCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesPolicy }
      )

  permissionedCandidatesValidatorAddress ←
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: PermissionedCandidatesValidator }
      )

  permissionedCandidatesValidatorHash ←
    Utils.toValidatorHash permissionedCandidatesValidatorAddress

  -- find all UTxOs at PermissionedCandidatesValidator address that contain PermissionedCandidatesToken
  permissionedCandidatesUTxOs ←
    Map.filter
      ( \( TransactionOutputWithRefScript
             { output: (TransactionOutput { amount }) }
         ) →
          Value.valueOf amount permissionedCandidatesCurrencySymbol
            permissionedCandidatesTokenName >
            BigInt.fromInt 0
      )
      <$> utxosAt permissionedCandidatesValidatorAddress

  -- check how much PermissionedCandidatesToken is stored in UTxOs that we're trying to update
  let
    permissionedCandidatesTokenAmount = sum
      $ map
          ( \( TransactionOutputWithRefScript
                 { output: (TransactionOutput { amount }) }
             ) → Value.valueOf amount permissionedCandidatesCurrencySymbol
              permissionedCandidatesTokenName
          )
      $ Map.values permissionedCandidatesUTxOs

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

  let
    value ∷ Value
    value = Value.singleton
      permissionedCandidatesCurrencySymbol
      permissionedCandidatesTokenName
      permissionedCandidatesTokenAmount

    permissionedCandidatesDatum ∷ Datum
    permissionedCandidatesDatum = Datum $ toData $
      PermissionedCandidatesValidatorDatum
        { candidates: map PermissionedCandidateKeys candidates }

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
        <> Lookups.unspentOutputs permissionedCandidatesUTxOs
        <> governanceLookups

    validatorRef = RefInput
      ( Transaction.mkTxUnspentOut permissionedCandidatesValidatorScriptRefTxInput
          permissionedCandidatesValidatorScriptRefTxOutput
      )

    spendScriptOutputConstraints ∷ TxConstraints Void Void
    spendScriptOutputConstraints =
      foldMap
        ( \txInput → Constraints.mustSpendScriptOutputUsingScriptRef txInput
            (Redeemer $ toData UpdatePermissionedCandidates)
            validatorRef
        ) $ Map.keys permissionedCandidatesUTxOs

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
        <> governanceConstraints

  pure { lookups, constraints }
