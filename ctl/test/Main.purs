module Test.Main (main) where

import Contract.Monad (liftedM , Contract , launchAff_)
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
import Data.Const (Const)
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Mote (Mote, plan, group, test)
import Mote.Plan (Plan, foldPlan)
import Test.Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Runner
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

-- tmp fn for testing the testing framework
failAlways ∷ Contract () Unit
failAlways = liftedM "fail" (pure Nothing) >>= \_ -> pure unit

testConfig :: Runner.Config
testConfig =
  { slow: Milliseconds 5000.0
  , timeout: Just (Milliseconds 100_000.0) -- plutip server can take 15s+ to terminate
  , exit: false
  }

-- run some contracts in a plutus server seeded with some wallets
plutipTest :: Contract () Unit -> Aff Unit
plutipTest go = let
  distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
    /\ [ BigInt.fromInt 2_000_000_000 ]
  in runPlutipContract config distribute \(alice /\ _bob) → withKeyWallet alice go

main ∷ Effect Unit
main = do
  MerkleTree.test -- Merkletree has its own testing framework atm
  launchAff_ do
    launchPlutipTests
    Runner.runSpec' testConfig [ consoleReporter ] (interpretMote assertionSpec)
--     runSpec' testConfig [ consoleReporter ] do
--       --    describe "Merkle tree integration tests" $ it "" MerkleTree.test
--       describe "testScenarios" do
-- --      it "committeeCandidateValidator" $ plutipTest CommitteeCandidateValidator.testScenario
--         it "fails" (plutipTest failAlways)
-- --      it "fuelMintingPolicy" $ plutipTest FUELMintingPolicy.testScenario
-- --      it "committeeCandidateValidator" $ plutipTest UpdateCommitteeHash.testScenario

-- Convert a mote to a a Spec
interpretMote :: Mote (Const Void) (Aff _) Unit -> Spec Unit
interpretMote = go <<< plan where
  go :: Plan (Const Void) (Aff _) -> Spec Unit
  go = foldPlan
    (\{ label , value } -> it label (void value))     -- tests
    (\label -> pending label)                         -- skipped tests
    (\{ label , value } -> describe label (go value)) -- groups of tests
    sequence_   -- sequence resulting values from previous handlers (Array i -> r)

-- Spec costructed using the `Mote` DSL rather than `Spec`
assertionSpec :: Mote (Const Void) (Aff _) Unit
assertionSpec =
--group "Test" $ test "fails" (plutipTest failAlways)
  group "Test" $ test "ok" (liftEffect $ spawnAndWaitForOutput "echo" ["5"] procOptions)

-- standard options for spawning a process with pipe option which redirects stdin,stdout and stderr
procOptions :: ChildProcess.SpawnOptions
procOptions = { cwd: Nothing , detached: true , env: Nothing , gid: Nothing
              , stdio: ChildProcess.pipe , uid: Nothing}

-- Spawn plutipTests in their own server instance in parallel and capture outputs
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

-- make a pipe and return stdout of running process
spawnAndWaitForOutput :: String -> Array String -> ChildProcess.SpawnOptions -> Effect (Readable ())
spawnAndWaitForOutput cmd args opts = do
  child <- spawn cmd args opts
  ChildProcess.onExit child $ const do
    log $ "Process " <> cmd <> " exited."
  pure (stdout child)
