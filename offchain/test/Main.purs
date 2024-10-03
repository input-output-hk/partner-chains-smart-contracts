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
import Test.Unit.Main as Test.Unit.Main
import Test.Utils (interpretWrappedTest)
import Test.Utils.Address as AddressUtils
import Test.Versioning as Versioning

-- | `main` runs all tests.
-- Note. When executing the tests (with `spago test`), you will probably see a warning
-- ```
-- (node:838881) MaxListenersExceededWarning: Possible EventEmitter memory leak detected. 11 exit listeners added to [process]. Use emitter.setMaxListeners() to increase limit
-- (Use `node --trace-warnings ...` to show where the warning was created)
-- ```
-- which according to the CTL team
-- > You can ignore it, it's not a memory leak, it's just that we attach a lot of listeners to the exit event
main :: Effect Unit
main = do
  Test.Unit.Main.runTest
    $ interpretWrappedTest do

        group "Unit tests" do
          AddressUtils.tests
          ConfigFile.tests

        group "Testnet integration tests" do
          IlliquidCirculationSupply.tests
          Reserve.tests
          InitMint.tests
          CommitteeCandidateValidator.tests
          Versioning.tests
          DParameter.tests
          PermissionedCandidates.tests

        group "Roundtrips" $ do
          Data.tests
