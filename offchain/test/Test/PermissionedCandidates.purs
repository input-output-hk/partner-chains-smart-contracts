module Test.PermissionedCandidates (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run as Run
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, fails, getOwnTransactionInput, testnetGroup)
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  , toSidechainParams
  )
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
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
          keyCount = 25
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ aggregateKeys
                $ map unwrap initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initGovernanceAuthority: Governance.mkGovernanceAuthority
                pkh
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            }

          sidechainParams = toSidechainParams (unwrap initScParams)

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

        _ ← initSidechain initScParams 1
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
          keyCount = 25
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ aggregateKeys
                $ map unwrap initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initGovernanceAuthority: Governance.mkGovernanceAuthority
                pkh
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            }

          sidechainParams = toSidechainParams (unwrap initScParams)

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
