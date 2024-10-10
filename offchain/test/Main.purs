module Test.Main (main) where

import Contract.Prelude

import Mote.Monad (group)
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.ConfigFile as ConfigFile
import Test.DParameter as DParameter
import Test.Data as Data
import Test.IlliquidCirculationSupply as IlliquidCirculationSupply
import Test.InitSidechain.TokensMint as InitMint
import Test.PermissionedCandidates as PermissionedCandidates
import Test.Reserve as Reserve
import Test.TestnetTest (interpretTestnetTest)
import Test.Unit.Main as Test.Unit.Main
import Test.Utils (interpretPureTest)
import Test.Utils.Address as AddressUtils
import Test.Versioning as Versioning

main :: Effect Unit
main = do
  Test.Unit.Main.runTest do
    interpretPureTest do
      group "Unit tests" do
        AddressUtils.tests
        ConfigFile.tests

      group "Roundtrips" $ do
        Data.tests

    interpretTestnetTest do
      group "Testnet integration tests" do
        IlliquidCirculationSupply.tests
        Reserve.tests
        InitMint.tests
        CommitteeCandidateValidator.tests
        Versioning.tests
        DParameter.tests
        PermissionedCandidates.tests
