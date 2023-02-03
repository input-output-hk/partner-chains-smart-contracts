module Test.Main (main) where

import Contract.Prelude

import Data.Foldable as Foldable
import Mote.Monad as Mote.Monad
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.InitSidechain as InitSidechain
import Test.MPTRoot as MPTRoot
import Test.MerkleProofSerialisation as MerkleProofSerialisation
import Test.MerkleRootChaining as MerkleRootChaining
import Test.MerkleTree as MerkleTree
import Test.Options.Parsers as Options.Parsers
import Test.PlutipTest as Test.PlutipTest
import Test.PoCECDSA as PoCECDSA
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript
import Test.PoCSerialiseData as PoCSerialiseData
import Test.Unit.Main as Test.Unit.Main
import Test.UpdateCommitteeHash as UpdateCommitteeHash

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
  -- Run the merkle tree integration tests
  Test.Unit.Main.runTest
    $
      Foldable.fold
        [ MerkleTree.interpretMerkleTreeTest MerkleTree.tests
        , MerkleProofSerialisation.interpretMerkleProofSerialisationTest
            MerkleProofSerialisation.tests
        , Options.Parsers.interpretOptionsTest Options.Parsers.tests

        -- Plutip tests
        ----------------
        , Test.PlutipTest.interpretPlutipTest InitSidechain.tests
        , Test.PlutipTest.interpretPlutipTest CommitteeCandidateValidator.tests
        , Test.PlutipTest.interpretPlutipTest FUELMintingPolicy.tests
        , Test.PlutipTest.interpretPlutipTest UpdateCommitteeHash.tests
        , Test.PlutipTest.interpretPlutipTest MPTRoot.tests
        , Test.PlutipTest.interpretPlutipTest MerkleRootChaining.tests
        -- Plutip POC tests
        ----------------
        , Test.PlutipTest.interpretPlutipTest
            ( Mote.Monad.group "POC Plutip tests" do
                PoCInlineDatum.tests
                PoCReferenceInput.tests
                PoCReferenceScript.tests
                PoCSerialiseData.tests
                PoCECDSA.testScenario
            )
        ]
