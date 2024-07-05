module Test.DelegatorRegistration
  ( tests
  ) where

import Cardano.Types.BigNum as BigNum
import Contract.Address (StakePubKeyHash(StakePubKeyHash))
import Contract.Prelude (bind, discard, sequence_, void, ($), (<$>))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash)
import Contract.Wallet as Wallet
import Data.List.Lazy (replicate)
import Mote.Monad as Mote.Monad
import Run (EFFECT, Run, liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, dummySidechainParams, testnetGroup)
import TrustlessSidechain.DelegatorRegistration (delegatorRegistration)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.SidechainParams (SidechainParams)
import Type.Row (type (+))

-- | `tests` wraps up all the committee candidate validator tests conveniently
tests ∷ WrappedTests
tests = testnetGroup "Delegator registration" $ do
  testScenarioSuccess1
  testScenarioSuccess2

runDelegatorRegistration ∷
  ∀ r.
  SidechainParams →
  Run (APP + EFFECT + r)
    TransactionHash
runDelegatorRegistration scParams =
  do
    sidechainPubKey ← StakePubKeyHash <$>
      (liftEffect $ randomSampleOne arbitrary)
    let
      sidechainSig = hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    delegatorRegistration scParams sidechainPubKey
      sidechainSig

-- Register
testScenarioSuccess1 ∷ TestnetTest
testScenarioSuccess1 = Mote.Monad.test "Register "
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      void $ runDelegatorRegistration dummySidechainParams

-- Register multipe times
testScenarioSuccess2 ∷ TestnetTest
testScenarioSuccess2 =
  Mote.Monad.test "10 registrations"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sequence_ $ replicate 10 $ runDelegatorRegistration dummySidechainParams
