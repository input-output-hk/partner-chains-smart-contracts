module Test.PermissionedCandidates (suite) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Testnet (withWallets)
import Contract.Wallet (withKeyWallet)
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Test.Utils (TestnetTest, fails, getOwnTransactionInput)
import TrustlessSidechain.Effects.Env (emptyEnv)
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  )

suite :: TestnetTest
suite = group "Minting, and burning a PermissionedCandidates Token" $
  do
    testScenario

testScenario :: TestnetTest
testScenario =
  test "Minting, updating and removing a PermissionedCandidates Token" do
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
            ( PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesToAdd:
                    [ { sidechainKey: hexToByteArrayUnsafe "bb11"
                      , auraKey: hexToByteArrayUnsafe "cc11"
                      , grandpaKey: hexToByteArrayUnsafe "dd11"
                      }
                    , { sidechainKey: hexToByteArrayUnsafe "bb22"
                      , auraKey: hexToByteArrayUnsafe "cc22"
                      , grandpaKey: hexToByteArrayUnsafe "dd22"
                      }
                    ]
                , permissionedCandidatesToRemove: Nothing
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: insert permissioned candidates"
            )

        void
          $
            ( PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesToAdd:
                    [ { sidechainKey: hexToByteArrayUnsafe "bb33"
                      , auraKey: hexToByteArrayUnsafe "cc33"
                      , grandpaKey: hexToByteArrayUnsafe "dd33"
                      }
                    ]
                , permissionedCandidatesToRemove: Just
                    [ { sidechainKey: hexToByteArrayUnsafe "bb22"
                      , auraKey: hexToByteArrayUnsafe "cc22"
                      , grandpaKey: hexToByteArrayUnsafe "dd22"
                      }
                    ]
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: update permissioned candidates"
            )

        ( void
            ( PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesToAdd:
                    [ { sidechainKey: hexToByteArrayUnsafe "bb33"
                      , auraKey: hexToByteArrayUnsafe "cc33"
                      , grandpaKey: hexToByteArrayUnsafe "dd33"
                      }
                    ]
                , permissionedCandidatesToRemove: Just
                    [ { sidechainKey: hexToByteArrayUnsafe "bb22"
                      , auraKey: hexToByteArrayUnsafe "cc22"
                      , grandpaKey: hexToByteArrayUnsafe "dd22"
                      }
                    ]
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: update permissioned candidates to the same value again (should fail)"
            )
        ) # withUnliftApp fails

        void
          $
            ( PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
                sidechainParams
                { permissionedCandidatesToAdd: []
                , permissionedCandidatesToRemove: Nothing
                }
                >>=
                  balanceSignAndSubmitWithoutSpendingUtxo
                    (unwrap sidechainParams).genesisUtxo
                    "Test: remove permissioned candidates"
            )

        _ <- initTokensMint sidechainParams 1
        pure unit
