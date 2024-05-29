module Test.Foo where

import Contract.Prelude

import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests)
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Foo as Foo
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

tests ∷ WrappedTests
tests =
  Test.Utils.plutipGroup "Foo"
    $ do
        testScenarioSuccess

testScenarioSuccess ∷ PlutipTest
testScenarioSuccess =
  Mote.Monad.test
    "Minting Foo token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        _ ← Foo.mintFooTokenLookupsAndConstraints >>=
          balanceSignAndSubmit "Minting Foo token"

        fooInfo ← Foo.fooCurrencyInfo

        Test.Utils.assertIHaveOutputWithAsset
          fooInfo.currencySymbol
          Foo.fooTokenName

        pure unit
