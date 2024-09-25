module Test.DParameter (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Wallet as Wallet
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, fails, getOwnTransactionInput, testnetGroup)
import TrustlessSidechain.DParameter as DParameter
import TrustlessSidechain.Effects.Log (logInfo')
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  )
import TrustlessSidechain.Versioning as Versioning

-- | `tests` aggregate all the DParameterPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = testnetGroup "Minting, and burning a DParameter Token" $
  do
    testScenario

testScenario ∷ TestnetTest
testScenario =
  Mote.Monad.test "Minting and updating a DParameter Token"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 1_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 150_000_000
        , BigNum.fromInt 150_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        logInfo' "Version not yet inserted"

        void $ initTokensMint sidechainParams 1
        void $ Versioning.initializeVersion sidechainParams 1

        logInfo' "DParameter not yet inserted"
        void
          $
            ( DParameter.mkInsertDParameterLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesCount: BigInt.fromInt 2
                , registeredCandidatesCount: BigInt.fromInt 3
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: insert D param"
            )
        logInfo' "DParameter inserted"
        void
          $
            ( DParameter.mkUpdateDParameterLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesCount: BigInt.fromInt 3
                , registeredCandidatesCount: BigInt.fromInt 4
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: update D param"
            )

        ( void
            $
              ( DParameter.mkUpdateDParameterLookupsAndConstraints
                  sidechainParams
                  { permissionedCandidatesCount: BigInt.fromInt 3
                  , registeredCandidatesCount: BigInt.fromInt 4
                  }
                  >>=
                    balanceSignAndSubmitWithoutSpendingUtxo
                      (unwrap sidechainParams).genesisUtxo
                      "Test: update removed D param"
              )
        ) # withUnliftApp fails

        pure unit
