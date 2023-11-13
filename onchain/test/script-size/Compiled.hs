{-# LANGUAGE TemplateHaskell #-}

module Compiled (
  newVerify,
  mkCPMPCode,
  mkCVCode,
  mkCCVCode,
  mkMPFuelCode,
  mkMPMerkleRootCode,
  mkUPCVCode,
  mkCommitteeOraclePolicyCode,
  mkCPCode,
  mkInsertValidatorCode,
  mkDsConfPolicyCode,
  mkDsKeyPolicyCode,
  mkCommitteePlainEcdsaSecp256k1ATMSPolicyCode,
  mkCommitteePlainSchnorrSecp256k1ATMSPolicyCode,
  toDataGenerated,
  toDataHandwritten,
  fromDataGenerated,
  fromDataHandwritten,
  unsafeFromDataGenerated,
  unsafeFromDataHandwritten,
  pairToDataGenerated,
  pairToDataHandwritten,
  pairFromDataGenerated,
  pairFromDataHandwritten,
  pairUnsafeFromDataGenerated,
  pairUnsafeFromDataHandwritten,
  listToDataGenerated,
  listToDataHandwritten,
  listFromDataHandwritten,
  listFromDataGenerated,
  listUnsafeFromDataGenerated,
  listUnsafeFromDataHandwritten,
  toDataWrapper,
  toDataDirect,
  fromDataWrapper,
  fromDataDirect,
  unsafeFromDataWrapper,
  unsafeFromDataDirect,
  toData3CPS,
  toData3Direct,
  fromData3CPS,
  fromData3Direct,
  unsafeFromData3CPS,
  unsafeFromData3Direct,
) where

import Data.Generated qualified as Generated
import Data.Handwritten qualified as Handwritten
import Plutus.V2.Ledger.Api (LedgerBytes, ScriptContext)
import PlutusTx.Code (CompiledCode)
import PlutusTx.TH (compile)
import TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
 )
import TrustlessSidechain.CheckpointValidator (
  InitCheckpointMint,
  mkCheckpointPolicy,
  mkCheckpointValidator,
 )
import TrustlessSidechain.CommitteeCandidateValidator (
  mkCommitteeCandidateValidator,
 )
import TrustlessSidechain.CommitteePlainATMSPolicy (verifyPlainMultisig)
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy qualified as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy qualified as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.DistributedSet (
  Ds,
  DsConfMint,
  DsDatum,
  DsKeyMint,
  mkDsConfPolicy,
  mkDsKeyPolicy,
  mkInsertValidator,
 )
import TrustlessSidechain.FUELMintingPolicy qualified as FUEL
import TrustlessSidechain.MerkleRootTokenMintingPolicy as MerkleRoot
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSRedeemer,
  BlockProducerRegistration,
  CandidatePermissionMint,
  CheckpointDatum,
  CheckpointParameter,
  CheckpointRedeemer,
  CommitteeCertificateMint,
  FUELMintingRedeemer,
  SidechainParams,
  SignedMerkleRootRedeemer,
  UpdateCommitteeDatum,
  UpdateCommitteeHash,
  UpdateCommitteeHashRedeemer,
 )
import TrustlessSidechain.UpdateCommitteeHash (
  InitCommitteeHashMint,
  mkCommitteeOraclePolicy,
  mkUpdateCommitteeHashValidator,
 )
import TrustlessSidechain.Versioning (
  VersionOracleConfig,
 )

toData3CPS :: CompiledCode (Generated.Baz -> BuiltinData)
toData3CPS = $$(compile [||toBuiltinData||])

toData3Direct :: CompiledCode (Handwritten.Baz -> BuiltinData)
toData3Direct = $$(compile [||toBuiltinData||])

fromData3CPS :: CompiledCode (BuiltinData -> Maybe Generated.Baz)
fromData3CPS = $$(compile [||fromBuiltinData||])

fromData3Direct :: CompiledCode (BuiltinData -> Maybe Handwritten.Baz)
fromData3Direct = $$(compile [||fromBuiltinData||])

unsafeFromData3CPS :: CompiledCode (BuiltinData -> Generated.Baz)
unsafeFromData3CPS = $$(compile [||unsafeFromBuiltinData||])

unsafeFromData3Direct :: CompiledCode (BuiltinData -> Handwritten.Baz)
unsafeFromData3Direct = $$(compile [||unsafeFromBuiltinData||])

toDataDirect :: CompiledCode (Handwritten.Bar -> BuiltinData)
toDataDirect = $$(compile [||toBuiltinData||])

toDataWrapper :: CompiledCode (Generated.Bar -> BuiltinData)
toDataWrapper = $$(compile [||toBuiltinData||])

fromDataDirect :: CompiledCode (BuiltinData -> Maybe Handwritten.Bar)
fromDataDirect = $$(compile [||fromBuiltinData||])

fromDataWrapper :: CompiledCode (BuiltinData -> Maybe Generated.Bar)
fromDataWrapper = $$(compile [||fromBuiltinData||])

unsafeFromDataDirect :: CompiledCode (BuiltinData -> Handwritten.Bar)
unsafeFromDataDirect = $$(compile [||unsafeFromBuiltinData||])

unsafeFromDataWrapper :: CompiledCode (BuiltinData -> Generated.Bar)
unsafeFromDataWrapper = $$(compile [||unsafeFromBuiltinData||])

listUnsafeFromDataGenerated :: CompiledCode (BuiltinData -> [Integer])
listUnsafeFromDataGenerated = $$(compile [||unsafeFromBuiltinData||])

listUnsafeFromDataHandwritten :: CompiledCode (BuiltinData -> [Integer])
listUnsafeFromDataHandwritten = $$(compile [||Handwritten.listUnsafeFromData||])

listFromDataGenerated :: CompiledCode (BuiltinData -> Maybe [Integer])
listFromDataGenerated = $$(compile [||fromBuiltinData||])

listFromDataHandwritten :: CompiledCode (BuiltinData -> Maybe [Integer])
listFromDataHandwritten = $$(compile [||Handwritten.listFromData||])

listToDataGenerated :: CompiledCode ([Integer] -> BuiltinData)
listToDataGenerated = $$(compile [||toBuiltinData||])

listToDataHandwritten :: CompiledCode ([Integer] -> BuiltinData)
listToDataHandwritten = $$(compile [||Handwritten.listToData||])

pairUnsafeFromDataGenerated :: CompiledCode (BuiltinData -> (Integer, Integer))
pairUnsafeFromDataGenerated = $$(compile [||unsafeFromBuiltinData||])

pairUnsafeFromDataHandwritten :: CompiledCode (BuiltinData -> (Integer, Integer))
pairUnsafeFromDataHandwritten = $$(compile [||Handwritten.pairUnsafeFromData||])

pairFromDataGenerated :: CompiledCode (BuiltinData -> Maybe (Integer, Integer))
pairFromDataGenerated = $$(compile [||fromBuiltinData||])

pairFromDataHandwritten :: CompiledCode (BuiltinData -> Maybe (Integer, Integer))
pairFromDataHandwritten = $$(compile [||Handwritten.pairFromData||])

pairToDataGenerated :: CompiledCode ((Integer, Integer) -> BuiltinData)
pairToDataGenerated = $$(compile [||toBuiltinData||])

pairToDataHandwritten :: CompiledCode ((Integer, Integer) -> BuiltinData)
pairToDataHandwritten = $$(compile [||Handwritten.pairToData||])

fromDataGenerated :: CompiledCode (BuiltinData -> Maybe Generated.Foo)
fromDataGenerated = $$(compile [||fromBuiltinData||])

fromDataHandwritten :: CompiledCode (BuiltinData -> Maybe Handwritten.Foo)
fromDataHandwritten = $$(compile [||fromBuiltinData||])

toDataGenerated :: CompiledCode (Generated.Foo -> BuiltinData)
toDataGenerated = $$(compile [||toBuiltinData||])

toDataHandwritten :: CompiledCode (Handwritten.Foo -> BuiltinData)
toDataHandwritten = $$(compile [||toBuiltinData||])

unsafeFromDataGenerated :: CompiledCode (BuiltinData -> Generated.Foo)
unsafeFromDataGenerated = $$(compile [||unsafeFromBuiltinData||])

unsafeFromDataHandwritten :: CompiledCode (BuiltinData -> Handwritten.Foo)
unsafeFromDataHandwritten = $$(compile [||unsafeFromBuiltinData||])

newVerify ::
  CompiledCode
    ( [LedgerBytes] ->
      Integer ->
      LedgerBytes ->
      [LedgerBytes] ->
      Bool
    )
newVerify = $$(compile [||verifyPlainMultisig verifyEcdsaSecp256k1Signature||])

mkCPMPCode ::
  CompiledCode (CandidatePermissionMint -> () -> ScriptContext -> Bool)
mkCPMPCode = $$(compile [||mkCandidatePermissionMintingPolicy||])

mkCVCode ::
  CompiledCode
    ( CheckpointParameter ->
      CheckpointDatum ->
      CheckpointRedeemer ->
      ScriptContext ->
      Bool
    )
mkCVCode = $$(compile [||mkCheckpointValidator||])

mkCCVCode ::
  CompiledCode
    ( SidechainParams ->
      BlockProducerRegistration ->
      () ->
      ScriptContext ->
      Bool
    )
mkCCVCode = $$(compile [||mkCommitteeCandidateValidator||])

mkMPFuelCode ::
  CompiledCode
    ( SidechainParams ->
      VersionOracleConfig ->
      FUELMintingRedeemer ->
      ScriptContext ->
      Bool
    )
mkMPFuelCode = $$(compile [||FUEL.mkMintingPolicy||])

mkMPMerkleRootCode ::
  CompiledCode
    ( SidechainParams ->
      VersionOracleConfig ->
      SignedMerkleRootRedeemer ->
      ScriptContext ->
      Bool
    )
mkMPMerkleRootCode = $$(compile [||MerkleRoot.mkMintingPolicy||])

mkUPCVCode ::
  CompiledCode
    ( UpdateCommitteeHash ->
      UpdateCommitteeDatum BuiltinData ->
      UpdateCommitteeHashRedeemer ->
      ScriptContext ->
      Bool
    )
mkUPCVCode = $$(compile [||mkUpdateCommitteeHashValidator||])

mkCommitteeOraclePolicyCode ::
  CompiledCode
    ( InitCommitteeHashMint ->
      () ->
      ScriptContext ->
      Bool
    )
mkCommitteeOraclePolicyCode = $$(compile [||mkCommitteeOraclePolicy||])

mkCPCode ::
  CompiledCode
    ( InitCheckpointMint ->
      () ->
      ScriptContext ->
      Bool
    )
mkCPCode = $$(compile [||mkCheckpointPolicy||])

mkInsertValidatorCode ::
  CompiledCode (Ds -> DsDatum -> () -> ScriptContext -> Bool)
mkInsertValidatorCode = $$(compile [||mkInsertValidator||])

mkDsConfPolicyCode ::
  CompiledCode (DsConfMint -> () -> ScriptContext -> Bool)
mkDsConfPolicyCode = $$(compile [||mkDsConfPolicy||])

mkDsKeyPolicyCode ::
  CompiledCode (DsKeyMint -> () -> ScriptContext -> Bool)
mkDsKeyPolicyCode = $$(compile [||mkDsKeyPolicy||])

mkCommitteePlainEcdsaSecp256k1ATMSPolicyCode ::
  CompiledCode (CommitteeCertificateMint -> VersionOracleConfig -> ATMSRedeemer -> ScriptContext -> Bool)
mkCommitteePlainEcdsaSecp256k1ATMSPolicyCode = $$(compile [||CommitteePlainEcdsaSecp256k1ATMSPolicy.mkMintingPolicy||])

mkCommitteePlainSchnorrSecp256k1ATMSPolicyCode ::
  CompiledCode (CommitteeCertificateMint -> VersionOracleConfig -> ATMSRedeemer -> ScriptContext -> Bool)
mkCommitteePlainSchnorrSecp256k1ATMSPolicyCode = $$(compile [||CommitteePlainSchnorrSecp256k1ATMSPolicy.mkMintingPolicy||])
