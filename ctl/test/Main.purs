module Test.Main (main) where

import Contract.Monad (launchAff_)
import Contract.Prelude (Effect, Maybe(..), Unit, discard, ($), (/\))
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Unfoldable
import Data.Traversable as Traversable
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.UpdateCommitteeHash as UpdateCommitteeHash
import Node.ChildProcess
import Node.ChildProcess as ChildProcess
import Node.Stream
import Node.Encoding as Encoding
import Node.Buffer as Buffer
import Prelude
import Effect.Console
import Effect.Class
import Effect.Aff
import Effect.Ref as Ref

main ∷ Effect Unit
main = do
  MerkleTree.test -- Merkletree has its own testing framework atm
  launchAff_ do
    let
      distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
        /\ [ BigInt.fromInt 2_000_000_000 ]
      testConfig =
        { slow: Milliseconds 5000.0
        , timeout: Just (Milliseconds 100_000.0) -- plutip can take 15s+ to terminate
        , exit: false
        }
      plutipTest go = runPlutipContract config distribute
        \(alice /\ _bob) → withKeyWallet alice go

    launchPlutipTests
--  runSpec' testConfig [ consoleReporter ] do
--    --    describe "Merkle tree integration tests" do
--    --      it "" MerkleTree.test
--    describe "testScenarios" do
--      it "committeeCandidateValidator" $ plutipTest
--        CommitteeCandidateValidator.testScenario
--      it "fuelMintingPolicy" $ plutipTest FUELMintingPolicy.testScenario
--      it "committeeCandidateValidator" $ plutipTest
--        UpdateCommitteeHash.testScenario

pipeStdout :: Array (Maybe StdIOBehaviour)
pipeStdout = [Just Pipe , Just Pipe , Just Pipe] -- stdin stdout stderr

procOptions :: SpawnOptions
procOptions = { cwd: Nothing , detached: true , env: Nothing , gid: Nothing , stdio: pipeStdout , uid: Nothing}

launchPlutipTests :: Aff Unit
launchPlutipTests = do 
  let a :: Array (Effect (Readable ()))
      a = [ spawnAndWaitForOutput "echo" ["5"] procOptions
          , spawnAndWaitForOutput "echo" ["3"] procOptions
          ]
  let b :: Effect _
      b = Traversable.traverse_ (_ >>= \s -> onDataString s Encoding.UTF8 log) a
  liftEffect b
  pure unit

spawnAndWaitForOutput :: String -> Array String -> _ -> Effect (Readable ())
spawnAndWaitForOutput cmd args opts = do
  child <- spawn cmd args opts
  ChildProcess.onExit child $ const do
    log $ "Process " <> cmd <> " exited."
  pure (stdout child)
