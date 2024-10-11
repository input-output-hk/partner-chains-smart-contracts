module Test.DParameter (suite) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test.Testnet (withWallets)
import Contract.Wallet (withKeyWallet)
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Test.Utils (TestnetTest, fails, getOwnTransactionInput)
import TrustlessSidechain.DParameter as DParameter
import TrustlessSidechain.Effects.Env (emptyEnv)
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  )

suite :: TestnetTest
suite = group "Minting, and burning a DParameter Token" do
  testScenario

testScenario :: TestnetTest
testScenario =
  test "Minting and updating a DParameter Token" do
    let
      initialDistribution =
        [ BigNum.fromInt 1_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 150_000_000
        , BigNum.fromInt 150_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do

        pkh <- getOwnPaymentPubKeyHash
        genesisUtxo <- getOwnTransactionInput
        let
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

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
