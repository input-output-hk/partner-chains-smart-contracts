module Test.PermissionedCandidates (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, fails, getOwnTransactionInput, testnetGroup)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  )

-- | `tests` aggregate all the PermissionedCandidatesPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = testnetGroup "Minting, and burning a PermissionedCandidates Token" $
  do
    testScenarioSuccess
    testScenarioFailure

testScenarioSuccess ∷ TestnetTest
testScenarioSuccess =
  Mote.Monad.test "Minting, updating and removing a PermissionedCandidates Token"
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

        _ ← initTokensMint sidechainParams 1
        pure unit

testScenarioFailure ∷ TestnetTest
testScenarioFailure =
  Mote.Monad.test
    "Minting PermissionedCandidates, and then updating with the same values (should fail)"
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

        ( void
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
        ) # withUnliftApp fails

        pure unit
