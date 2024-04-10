module Test.MinotaurStake (tests) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Prim.ByteArray as ByteArray
import Contract.Utxos (getUtxo)
import Contract.Wallet as Wallet
import Ctl.Internal.Deserialization.Keys
  ( freshPrivateKey
  )
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Test.UtxoDistribution
  ( InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  )
import Ctl.Internal.Wallet.Key (PrivateStakeKey(PrivateStakeKey))
import Data.Array as Array
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run as Run
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, fails, getOwnTransactionInput, plutipGroup)
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  , toSidechainParams
  )
import TrustlessSidechain.MinotaurStake as MinotaurStake
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  )

-- | `tests` aggregate all the DParameterPolicy tests in one convenient
-- | function
tests ∷ PrivateKey → WrappedTests
tests pk = plutipGroup "Minting, and burning a DParameter Token" $
  do
    testScenarioSuccess pk

testScenarioSuccess ∷ PrivateKey → PlutipTest
testScenarioSuccess privateKey =
  Mote.Monad.test "Minting and updating a DParameter Token"
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
