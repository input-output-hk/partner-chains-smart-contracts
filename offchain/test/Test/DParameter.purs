module Test.DParameter (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (getUtxo)
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
import TrustlessSidechain.DParameter as DParameter
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  , toSidechainParams
  )
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  )

-- | `tests` aggregate all the DParameterPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = testnetGroup "Minting, and burning a DParameter Token" $
  do
    testScenarioSuccess
    testScenarioFailure

testScenarioSuccess ∷ TestnetTest
testScenarioSuccess =
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
            , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            }

          sidechainParams = toSidechainParams (unwrap initScParams)

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

        _ ← initSidechain initScParams 1
        pure unit

testScenarioFailure ∷ TestnetTest
testScenarioFailure =
  Mote.Monad.test
    "Minting, and updating a DParameter Token with the same value. (this should fail)"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 1_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 150_000_000
        , BigNum.fromInt 150_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        genesisOutput ← liftContract $ getUtxo genesisUtxo
        liftContract $ logInfo' (show genesisOutput)
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
            , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            }

          sidechainParams = toSidechainParams (unwrap initScParams)

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

        ( void
            $
              ( DParameter.mkUpdateDParameterLookupsAndConstraints
                  sidechainParams
                  { permissionedCandidatesCount: BigInt.fromInt 2
                  , registeredCandidatesCount: BigInt.fromInt 3
                  }
                  >>=
                    balanceSignAndSubmitWithoutSpendingUtxo
                      (unwrap sidechainParams).genesisUtxo
                      "Test: update removed D param"
              )
        ) # withUnliftApp fails
