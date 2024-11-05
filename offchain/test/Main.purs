module Test.Main (main) where

import Contract.Prelude

import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Testnet (testTestnetContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote.Monad (group)
import Mote.TestPlanM (TestPlanM)
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.ConfigFile as ConfigFile
import Test.DParameter as DParameter
import Test.Data as Data
import Test.Governance as Governance
import Test.IlliquidCirculationSupply as IlliquidCirculationSupply
import Test.PermissionedCandidates as PermissionedCandidates
import Test.Reserve as Reserve
import Test.Spec.Runner (defaultConfig)
import Test.Unit.Main as Test.Unit.Main
import Test.Utils (interpretPureTest)
import Test.Utils.Address as AddressUtils
import Test.Versioning as Versioning

main :: Effect Unit
main = do
  pureSuite
  interruptOnSignal SIGINT =<< launchAff do
    flip cancelWith (effectCanceler (exitCode 1)) do
      interpretWithConfig
        defaultConfig { timeout = Just $ Milliseconds 90_000.0, exit = true }
        testnetSuite

pureSuite :: Effect Unit
pureSuite = do
  Test.Unit.Main.runTest do
    interpretPureTest do
      group "Unit tests" do
        AddressUtils.tests
        ConfigFile.tests

      group "Roundtrips" do
        Data.tests

testnetSuite :: TestPlanM (Aff Unit) Unit
testnetSuite = do
  testTestnetContracts config do
    group "Testnet integration tests" do
      PermissionedCandidates.suite
      IlliquidCirculationSupply.suite
      Reserve.suite
      CommitteeCandidateValidator.suite
      Versioning.suite
      DParameter.suite
      Governance.suite
