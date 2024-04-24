module Test.MinotaurStake (tests) where

import Contract.Prelude

import Contract.Prim.ByteArray as ByteArray
import Contract.Wallet as Wallet
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Test.UtxoDistribution
  ( InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  )
import Ctl.Internal.Wallet.Key (PrivateStakeKey(PrivateStakeKey))
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.MinotaurStake as MinotaurStake
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  )

-- | `tests` aggregate all the DParameterPolicy tests in one convenient
-- | function
tests ∷ PrivateKey → WrappedTests
tests pk = plutipGroup "Minting, and burning a Minotaur Stake Token" $
  do
    testScenarioSuccess pk

testScenarioSuccess ∷ PrivateKey → PlutipTest
testScenarioSuccess privateKey =
  Mote.Monad.test "Minting and updating a Minotaur Stake Token"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( InitialUTxOsWithStakeKey (PrivateStakeKey privateKey)
            [ BigInt.fromInt 1_000_000
            , BigInt.fromInt 5_000_000
            , BigInt.fromInt 150_000_000
            , BigInt.fromInt 150_000_000
            ]
        )

    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        _ ←
          ( MinotaurStake.mkMinotaurDelegateLookupsAndConstraints
              { stakePoolId: ByteArray.hexToByteArrayUnsafe "abababababa"
              , partnerChainRewardAddress: ByteArray.hexToByteArrayUnsafe
                  "abababababa"
              }
              >>=
                balanceSignAndSubmit
                  "Test: delegate minotaur stake"
          )
        pure unit
