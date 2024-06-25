module Test.FUELProxyPolicy (tests) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.NetworkId (NetworkId(TestnetId))
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Run (liftEffect) as Run
import Test.MerkleRoot as Test.MerkleRoot
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, fails, getOwnTransactionInput, plutipGroup)
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.FUELMintingPolicy.V1 (MerkleTreeEntry(..))
import TrustlessSidechain.FUELMintingPolicy.V1 as Mint.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as Mint.V2
import TrustlessSidechain.FUELProxyPolicy
  ( FuelMintParams(..)
  , mkFuelProxyBurnLookupsAndConstraints
  , mkFuelProxyMintLookupsAndConstraints
  )
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.Utils.Address
  ( fromPaymentPubKeyHash
  , getOwnPaymentPubKeyHash
  )
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning as Versioning

-- | `tests` aggregate all the FUELProxyPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = plutipGroup "Claiming and burning FUEL tokens using proxy mechanism" $
  do
    testScenarioSuccess
    testScenarioSuccess2
    testScenarioFailure

-- | Mint and burn multiuple times, updating minting and burning strategies
-- | between operations.  In particular we:
-- |   - mint 5
-- |   - update minting startegy
-- |   - mint 7
-- |   - burn 10
-- |   - update burning strategy
-- |   - burn 2
testScenarioSuccess ∷ PlutipTest
testScenarioSuccess =
  Mote.Monad.test "Multiple minting and burning, updating policies in between"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 150_000_000, BigNum.fromInt 150_000_000 ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        pkh ← getOwnPaymentPubKeyHash
        let ownRecipient = Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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

        { sidechainParams } ← initSidechain initScParams 1
        let
          amount = BigInt.fromInt 5
          recipient = fromPaymentPubKeyHash TestnetId pkh
          index = BigInt.fromInt 0
          previousMerkleRoot = Nothing
          ownEntry =
            MerkleTreeEntry
              { index
              , amount
              , previousMerkleRoot
              , recipient: ownRecipient
              }

          ownEntryBytes = unwrap $ encodeCbor $ toData ownEntry
          merkleTree =
            unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
              [ ownEntryBytes ]

          merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp
            ownEntryBytes
            merkleTree
        void $ Test.MerkleRoot.saveRoot
          { sidechainParams
          , merkleTreeEntries: [ ownEntry ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }

        let
          fuelMintingParams = FuelMintParamsV1 $
            Mint.V1.FuelMintParams
              { amount
              , recipient
              , sidechainParams
              , merkleProof
              , index
              , previousMerkleRoot
              , dsUtxo: Nothing
              }

        -- Mint 5 fuel tokens using FUEL policy v1
        void
          $ mkFuelProxyMintLookupsAndConstraints sidechainParams
              fuelMintingParams
          >>=
            balanceSignAndSubmit "Test: mint fuel via v1 proxy"

        -- Insert new version of scripts.  Both version 1 and 2 are active and
        -- available at this point.
        void $ Versioning.insertVersion
          { sidechainParams, atmsKind: (unwrap initScParams).initATMSKind }
          2

        -- Mint 7 fuel tokens using FUEL policy v2
        void
          $ mkFuelProxyMintLookupsAndConstraints sidechainParams
              ( FuelMintParamsV2
                  (Mint.V2.FuelMintParams { amount: BigInt.fromInt 7 })
              )
          >>=
            balanceSignAndSubmit "Test: mint fuel via v2 proxy"

        -- Burn 10 fuel tokens using FUEL policy v1
        void
          $ mkFuelProxyBurnLookupsAndConstraints
              { amount: BigInt.fromInt 10
              , recipient: hexToByteArrayUnsafe "aabbcc"
              , sidechainParams
              , version: BigNum.fromInt 1
              }

          >>=
            balanceSignAndSubmit "Test: burn fuel via v1 proxy"

        -- Burn 2 fuel tokens using FUEL policy v2
        void
          $ mkFuelProxyBurnLookupsAndConstraints
              { amount: BigInt.fromInt 2
              , recipient: hexToByteArrayUnsafe "aabbcc"
              , sidechainParams
              , version: BigNum.fromInt 2
              }
          >>=
            balanceSignAndSubmit "Test: burn fuel via v2 proxy"

testScenarioSuccess2 ∷ PlutipTest
testScenarioSuccess2 =
  Mote.Monad.test "Minting FUELProxy, and then minting again"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 150_000_000, BigNum.fromInt 150_000_000 ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        pkh ← getOwnPaymentPubKeyHash
        let ownRecipient = Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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

        { sidechainParams } ← initSidechain initScParams 1
        let
          amount = BigInt.fromInt 5
          amount2 = BigInt.fromInt 10
          recipient = fromPaymentPubKeyHash TestnetId pkh
          index = BigInt.fromInt 0
          index2 = BigInt.fromInt 1
          previousMerkleRoot = Nothing
          ownEntry =
            MerkleTreeEntry
              { index
              , amount
              , previousMerkleRoot
              , recipient: ownRecipient
              }
          ownEntry2 =
            MerkleTreeEntry
              { index: index2
              , amount: amount2
              , previousMerkleRoot
              , recipient: ownRecipient
              }

          ownEntryBytes = unwrap $ encodeCbor $ toData ownEntry
          ownEntryBytes2 = unwrap $ encodeCbor $ toData ownEntry2

          merkleTree =
            unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
              [ ownEntryBytes, ownEntryBytes2 ]

          merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp
            ownEntryBytes
            merkleTree
          merkleProof2 = unsafePartial $ fromJust $ MerkleTree.lookupMp
            ownEntryBytes2
            merkleTree
        void $ Test.MerkleRoot.saveRoot
          { sidechainParams
          , merkleTreeEntries: [ ownEntry, ownEntry2 ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }

        let
          fuelMintingParams = FuelMintParamsV1 $
            Mint.V1.FuelMintParams
              { amount
              , recipient
              , sidechainParams
              , merkleProof
              , index
              , previousMerkleRoot
              , dsUtxo: Nothing
              }
          fuelMintingParams2 = FuelMintParamsV1 $
            Mint.V1.FuelMintParams
              { amount: amount2
              , recipient
              , sidechainParams
              , merkleProof: merkleProof2
              , index: index2
              , previousMerkleRoot
              , dsUtxo: Nothing
              }

        -- Mint 5 fuel tokens using FUEL policy v1
        void
          $ mkFuelProxyMintLookupsAndConstraints sidechainParams
              fuelMintingParams
          >>=
            balanceSignAndSubmit "Test: mint fuel via v1 proxy"

        void
          $ mkFuelProxyMintLookupsAndConstraints sidechainParams
              fuelMintingParams2
          >>=
            balanceSignAndSubmit "Test: mint fuel via v1 proxy"

-- | Mint using version 1, update to version 2, burn using version 2
-- | this should fail
testScenarioFailure ∷ PlutipTest
testScenarioFailure =
  Mote.Monad.test
    "Attempt to burn tokens using invalidated version (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 150_000_000, BigNum.fromInt 150_000_000 ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do

          pkh ← getOwnPaymentPubKeyHash
          let ownRecipient = Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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

          { sidechainParams } ← initSidechain initScParams 1
          let
            amount = BigInt.fromInt 5
            recipient = fromPaymentPubKeyHash TestnetId pkh
            index = BigInt.fromInt 0
            previousMerkleRoot = Nothing
            ownEntry =
              MerkleTreeEntry
                { index
                , amount
                , previousMerkleRoot
                , recipient: ownRecipient
                }

            ownEntryBytes = unwrap $ encodeCbor $ toData ownEntry
            merkleTree =
              unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
                [ ownEntryBytes ]

            merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp
              ownEntryBytes
              merkleTree
          void $ Test.MerkleRoot.saveRoot
            { sidechainParams
            , merkleTreeEntries: [ ownEntry ]
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , previousMerkleRoot: Nothing
            }

          let
            fuelMintingParams = FuelMintParamsV1 $
              Mint.V1.FuelMintParams
                { amount
                , recipient
                , sidechainParams
                , merkleProof
                , index
                , previousMerkleRoot
                , dsUtxo: Nothing
                }

          -- Mint 5 fuel tokens using FUEL policy v1
          void
            $ mkFuelProxyMintLookupsAndConstraints sidechainParams
                fuelMintingParams
            >>=
              balanceSignAndSubmit "Test: mint fuel via v1 proxy"

          -- Update scripts, invalidating version 1 policies
          void $ Versioning.insertVersion
            { sidechainParams, atmsKind: (unwrap initScParams).initATMSKind }
            2

          void $ Versioning.invalidateVersion
            { sidechainParams, atmsKind: (unwrap initScParams).initATMSKind }
            1

          -- Attempt to burn fuel using invalidated version 1 policy.  Should
          -- fail.
          void
            $ mkFuelProxyBurnLookupsAndConstraints
                { amount: BigInt.fromInt 5
                , recipient: hexToByteArrayUnsafe "aabbcc"
                , sidechainParams
                , version: BigNum.fromInt 1
                }
            >>=
              balanceSignAndSubmit "Test: burn fuel via v1 proxy"
          # withUnliftApp fails
