module Test.Main (main) where

import Contract.Monad (liftedM , Contract)
import Contract.Prelude (Effect, (/\))
import Contract.Test.Plutip (runPlutipContract , PlutipConfig)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Maybe(Maybe(..))
import Data.Foldable(sequence_)
import Data.Const (Const)
import Data.UInt(UInt , fromInt)
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Mote (Mote, plan, group, test)
import Mote.Plan (Plan, foldPlan)
import Test.Spec (Spec , describe , it , pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Runner
import Test.UpdateCommitteeHash as UpdateCommitteeHash
import Node.ChildProcess (spawn , stdout)
import Node.ChildProcess as ChildProcess
import Node.Stream(Read , Readable , Stream)
import Prelude(Unit, Void, bind, const, discard, pure, unit, void, ($), (+), (<$>), (<<<), (<>), (>>=))
import Effect.Console(log)
import Effect.Class(liftEffect)
import Effect.Aff(Aff, Milliseconds(..), launchAff_)

-- tmp fn for testing the testing framework
failContract ∷ Contract () Unit
failContract = liftedM "fail" (pure Nothing) >>= \_ -> pure unit

testConfig :: Runner.Config
testConfig =
  { slow: Milliseconds 5000.0
  , timeout: Just (Milliseconds 100_000.0) -- plutip server can take 15s+ to terminate
  , exit: false
  }

-- add number to all ports in config
incPorts :: UInt -> PlutipConfig -> PlutipConfig
incPorts n config = ((((config
  { port = n + config.port })
  { ogmiosConfig { port = config.ogmiosConfig.port + n } })
  { ogmiosDatumCacheConfig { port = config.ogmiosDatumCacheConfig.port + n }})
  { postgresConfig { port = config.postgresConfig.port + n }})
  { ctlServerConfig = (\cfg -> cfg { port = cfg.port + n }) <$> config.ctlServerConfig }

-- run some contracts in a plutus server seeded with some wallets
-- n is the number to increment all plutip config ports
plutipTest :: Int -> Contract () Unit -> Aff Unit
plutipTest n go = let
  distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
    /\ [ BigInt.fromInt 2_000_000_000 ]
  in runPlutipContract (incPorts (fromInt n) config) distribute \(alice /\ _bob) → withKeyWallet alice go

-- Spec constructed using the `Mote` DSL rather than `Spec`
oldSpec :: Mote (Const Void) (Aff Unit) Unit
oldSpec = group "testScenarios" do
--test "fails" (plutipTest failContract)
  test "Merkle tree integration tests" $ liftEffect (MerkleTree.test)
  test "committeeCandidateValidator" $ plutipTest 0 CommitteeCandidateValidator.testScenario
  test "fuelMintingPolicy" $ plutipTest 1 FUELMintingPolicy.testScenario
  test "committeeCandidateValidator" $ plutipTest 2 UpdateCommitteeHash.testScenario

-- override stdout to collate results independently
testRedirectedSpec :: Mote (Const Void) (Aff (Stream (read :: Read))) Unit
testRedirectedSpec = group "TestTestFramework" $ do
  test "ok" (liftEffect $ spawnAndWaitForOutput "echo" ["5"] procOptions)
  test "ko" (liftEffect $ spawnAndWaitForOutput "false" [] procOptions)

main ∷ Effect Unit
main = do
  launchAff_ do
--  launchPlutipTests
    Runner.runSpec' testConfig [ consoleReporter ] (interpretMoteIdentity oldSpec)

-- Convert a mote to a a Spec, this is left here to clarify whats going on
interpretMoteIdentity :: Mote (Const Void) (Aff _) Unit -> Spec Unit
interpretMoteIdentity = go <<< plan where
  go :: Plan (Const Void) (Aff _) -> Spec Unit
  go = foldPlan
    (\{ label , value } -> it label (void value))     -- tests
    (\label -> pending label)                         -- skipped tests
    (\{ label , value } -> {-Spec.parallel $-} describe label (go value)) -- groups of tests
    sequence_   -- sequence resulting values from previous handlers (Array i -> r)

{-
-- Convert mote to Spec, this time accumulating output pipes
interpretMote :: Mote (Const Void) (Aff (Stream (read :: Read))) Unit -> Spec Unit
interpretMote = go <<< plan where
  doTest { label , value } = it label $ attempt value >>= case _ of
    Right s -> do
      liftEffect $ onDataString s Encoding.UTF8 log
      pure unit
    Left e  -> throwError e
  go :: Plan (Const Void) (Aff _) -> Spec Unit
  go = foldPlan
    doTest
    (\label -> (pending label))          -- skipped tests
    (\{ label , value } -> describe label (go value))
    sequence_   -- sequence resulting values from previous handlers (Array i -> r)

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
-}

-- standard options for spawning a process with pipe option which redirects stdin,stdout and stderr
procOptions :: ChildProcess.SpawnOptions
procOptions = { cwd: Nothing , detached: true , env: Nothing , gid: Nothing
              , stdio: ChildProcess.pipe , uid: Nothing}

-- make a pipe and return stdout of running process
spawnAndWaitForOutput :: String -> Array String -> ChildProcess.SpawnOptions -> Effect (Readable ())
spawnAndWaitForOutput cmd args opts = do
  child <- spawn cmd args opts
  ChildProcess.onExit child $ const do
    log $ "Process " <> cmd <> " exited."
  pure (stdout child)
