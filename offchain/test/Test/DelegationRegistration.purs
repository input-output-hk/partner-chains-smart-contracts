module Test.DelegationRegistration
  ( tests
  ) where

import Contract.Address (StakePubKeyHash(StakePubKeyHash))
import Contract.Prelude (bind, discard, sequence_, void, ($))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash)
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.List.Lazy (replicate)
import Mote.Monad as Mote.Monad
import Run (EFFECT, Run, liftEffect)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSampleOne)
import Test.Utils (WrappedTests, dummySidechainParams, plutipGroup)
import Test.Utils.QuickCheck (ArbitraryPubKeyHash(..))
import TrustlessSidechain.DelegationRegistration (delegationRegistration)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.SidechainParams (SidechainParams)
import Type.Row (type (+))

-- | `tests` wraps up all the committee candidate validator tests conveniently
tests ∷ WrappedTests
tests = plutipGroup "Delegator registration" $ do
  testScenarioSuccess1
  testScenarioSuccess2

runDelegationRegistration ∷
  ∀ r.
  SidechainParams →
  Run (APP + EFFECT + r)
    TransactionHash
runDelegationRegistration scParams =
  do
    ArbitraryPubKeyHash sidechainPubKey ← liftEffect $ randomSampleOne
      (arbitrary ∷ Gen ArbitraryPubKeyHash)
    let
      sidechainSig = hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    delegationRegistration scParams (StakePubKeyHash sidechainPubKey)
      sidechainSig

-- Register
testScenarioSuccess1 ∷ PlutipTest
testScenarioSuccess1 = Mote.Monad.test "Register "
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      void $ runDelegationRegistration dummySidechainParams

-- Register multipe times
testScenarioSuccess2 ∷ PlutipTest
testScenarioSuccess2 =
  Mote.Monad.test "10 registrations"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sequence_ $ replicate 10 $ runDelegationRegistration dummySidechainParams
