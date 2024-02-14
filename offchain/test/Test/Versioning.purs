module Test.Versioning (tests) where

import Contract.Prelude

import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils
  ( WrappedTests
  , getOwnTransactionInput
  , plutipGroup
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.MerkleRoot
  ( merkleRootCurrencyInfo
  )
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.Types (ScriptId(..))
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `tests` aggregate all the Versioning tests in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Minting and burning versioning tokens" $ do
  testScenarioSuccess

testScenarioSuccess ∷ PlutipTest
testScenarioSuccess =
  Mote.Monad.test "Insert new version, update it, and burn it"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
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
            , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                pkh
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            }

        { sidechainParams } ← initSidechain initScParams 1

        { mintingPolicy: merkleRootTokenMintingPolicy } ←
          merkleRootCurrencyInfo sidechainParams
        committeeCandidateValidator ←
          getCommitteeCandidateValidator sidechainParams

        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              1
              (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
          >>=
            balanceSignAndSubmit "Test: insert policy version"

        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              1
              (CommitteeCandidateValidator /\ committeeCandidateValidator)
          >>=
            balanceSignAndSubmit "Test: insert validator version"

        void
          $ Versioning.updateVersionLookupsAndConstraints
              sidechainParams
              1
              2
              (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
          >>=
            balanceSignAndSubmit "Test: update policy version"

        void
          $ Versioning.updateVersionLookupsAndConstraints
              sidechainParams
              1
              2
              (CommitteeCandidateValidator /\ committeeCandidateValidator)
          >>=
            balanceSignAndSubmit "Test: update validator version"

        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              2
              MerkleRootTokenPolicy
          >>=
            balanceSignAndSubmit "Test: invalidate policy version"

        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              2
              CommitteeCandidateValidator
          >>=
            balanceSignAndSubmit "Test: invalidate validator version"
