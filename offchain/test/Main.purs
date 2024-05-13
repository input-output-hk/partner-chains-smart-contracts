module Test.Main (main) where

import Contract.Prelude

import Mote.Monad (group)
import Test.CandidatePermissionToken as CandidatePermissionToken
import Test.Checkpoint as Checkpoint
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import Test.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import Test.ConfigFile as ConfigFile
import Test.DParameter as DParameter
import Test.Data as Data
import Test.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import Test.FUELProxyPolicy as FUELProxyPolicy
import Test.GarbageCollector as GarbageCollector
import Test.InitSidechain as InitSidechain
import Test.InitSidechain.CandidatePermissionToken as InitCandidatePermissionToken
import Test.InitSidechain.Checkpoint as InitCheckpoint
import Test.InitSidechain.FUEL as InitFUEL
import Test.InitSidechain.TokensMint as InitMint
import Test.MerkleProofSerialisation as MerkleProofSerialisation
import Test.MerkleRoot as MerkleRoot
import Test.MerkleRootChaining as MerkleRootChaining
import Test.MerkleTree as MerkleTree
import Test.Options.Parsers as Options.Parsers
import Test.PermissionedCandidates as PermissionedCandidates
import Test.PoCECDSA as PoCECDSA
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript
import Test.PoCSchnorrSecp256k1 as PoCSchnorrSecp256k1
import Test.PoCSerialiseData as PoCSerialiseData
import Test.Unit.Main as Test.Unit.Main
import Test.UpdateCommitteeHash as UpdateCommitteeHash
import Test.Utils (interpretWrappedTest, plutipGroup)
import Test.Utils.Address as AddressUtils
import Test.Versioning as Versioning

-- | `main` runs all tests.
-- Note. it is necessary to be running a `plutip-server` somewhere for this
-- Note. When executing the tests (with `spago test`), you will probably see a warning
-- ```
-- (node:838881) MaxListenersExceededWarning: Possible EventEmitter memory leak detected. 11 exit listeners added to [process]. Use emitter.setMaxListeners() to increase limit
-- (Use `node --trace-warnings ...` to show where the warning was created)
-- ```
-- which according to the CTL team
-- > You can ignore it, it's not a memory leak, it's just that we attach a lot of listeners to the exit event
main ∷ Effect Unit
main = do
  Test.Unit.Main.runTest
    $ interpretWrappedTest do

        group "Unit tests" do
          MerkleTree.tests
          MerkleProofSerialisation.tests
          Options.Parsers.tests
          AddressUtils.tests
          ConfigFile.tests

        group "Plutip integration tests" do
          InitCandidatePermissionToken.tests
          InitSidechain.tests
          InitMint.tests
          InitCheckpoint.tests
          InitFUEL.tests
          CommitteePlainEcdsaSecp256k1ATMSPolicy.tests
          CommitteePlainSchnorrSecp256k1ATMSPolicy.tests
          CommitteeCandidateValidator.tests
          CandidatePermissionToken.tests
          FUELMintingPolicy.V1.tests
          FUELProxyPolicy.tests
          UpdateCommitteeHash.tests
          MerkleRoot.tests
          MerkleRootChaining.tests
          Checkpoint.tests
          Versioning.tests
          DParameter.tests
          PermissionedCandidates.tests
          GarbageCollector.tests

        plutipGroup "POC Plutip tests" do
          PoCInlineDatum.tests
          PoCReferenceInput.tests
          PoCReferenceScript.tests
          PoCSerialiseData.tests
          PoCECDSA.testScenario
          PoCSchnorrSecp256k1.tests

        group "Roundtrips" $ do
          Data.tests
