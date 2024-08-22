module Test.UpdateCommitteeHash
  ( testScenario1
  , testScenario2
  , testScenario3
  , testScenario4
  , testScenario5
  , updateCommitteeHash
  , updateCommitteeHashWith
  , tests
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Partial.Unsafe as Unsafe
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures(PlainEcdsaSecp256k1)
  , ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Env (Env, ask)
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.GarbageCollector as GarbageCollector
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.FUEL (initFuel)
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(..)
  , getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  , aggregateKeys
  , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe
  , byteArrayToEcdsaSecp256k1PubKeyUnsafe
  , generatePrivKey
  , multiSign
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `generateUchmSignatures` generates the public keys and corresponding
-- | signatures of the current committee for the new committee given.
-- This may be helpful when attempting to use the CLI interface to generate
-- test cases manually.
generateUchmSignatures ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe RootHash
  , -- the sidechain epoch
    sidechainEpoch ∷ BigInt
  , -- the Reader Env
    env ∷ Env
  } →
  Contract (Array (Tuple EcdsaSecp256k1PubKey EcdsaSecp256k1Signature))
generateUchmSignatures
  { sidechainParams
  , currentCommitteePrvKeys
  , newCommitteePrvKeys
  , previousMerkleRoot
  , sidechainEpoch
  , env
  } = do
  let
    -- Order the private keys by lexicographical ordering of the signatures, so
    -- it's easy to give the sorted pubkey with its associated signature.
    currentCommitteePubKeys /\ currentCommitteePrvKeys' =
      Array.unzip
        $ Array.sortWith fst
        $ map (\prvKey → toPubKeyUnsafe prvKey /\ prvKey) currentCommitteePrvKeys

    newAggregatePubKeys = aggregateKeys $ map unwrap $ Array.sort $
      map
        toPubKeyUnsafe
        newCommitteePrvKeys

  -- Creating the update committee hash validator (since we want to pay the
  -- committee oracle back to the same address)
  ---------------------------
  { validatorHash: updateCommitteeHashValidatorHash } ←
    unliftApp env $ getUpdateCommitteeHashValidator sidechainParams

  -- Building the message to sign
  ---------------------------
  committeeMessage ←
    liftContractM "Failed to serialise update committee hash message"
      $ UpdateCommitteeHash.serialiseUchmHash
      $ UpdateCommitteeHashMessage
          { sidechainParams
          , newAggregatePubKeys
          , previousMerkleRoot
          , sidechainEpoch
          , validatorHash: updateCommitteeHashValidatorHash
          }
  let
    committeeSignatures = Array.zip
      currentCommitteePubKeys
      (multiSign currentCommitteePrvKeys' committeeMessage)

  pure committeeSignatures

-- | `updateCommitteeHash` is a convenient wrapper around
-- | `UpdateCommitteeHash.updateCommitteeHash` for writing tests.
-- | Note that this makes the entire current committee sign the message.
updateCommitteeHash ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe RootHash
  , -- sidechain epoch of the new committee
    sidechainEpoch ∷ BigInt
  , -- the Reader Env
    env ∷ Env
  } →
  Contract Unit
updateCommitteeHash params = updateCommitteeHashWith params pure

-- | `updateCommitteeHashWith params f` is a convenient wrapper around
-- | `UpdateCommitteeHash.updateCommitteeHash` for writing tests which modify the
-- | inputted 'UpdateCommitteeHashParams' with the given function `f`.
-- |
-- | In particular, the function `f` can be used to change the signatures
-- | provided by the committee.
updateCommitteeHashWith ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe RootHash
  , -- sidechain epoch of the new committee
    sidechainEpoch ∷ BigInt
  , env ∷ Env
  } →
  ( UpdateCommitteeHashParams PlutusData →
    Contract (UpdateCommitteeHashParams PlutusData)
  ) →
  Contract Unit
updateCommitteeHashWith params f = void do
  committeeSignatures ← generateUchmSignatures params

  let
    newAggregatePubKeys = aggregateKeys $ map unwrap $ Array.sort
      $ map toPubKeyUnsafe
      $
        params.newCommitteePrvKeys
    uchp =
      UpdateCommitteeHashParams
        { sidechainParams: params.sidechainParams
        , newAggregatePubKeys: toData newAggregatePubKeys
        , aggregateSignature:
            PlainEcdsaSecp256k1
              (map (Just <$> _) committeeSignatures)
        -- take `pubkey /\ sig` and convert to `pubkey /\ Just sig`
        , previousMerkleRoot: params.previousMerkleRoot
        , sidechainEpoch: params.sidechainEpoch
        , mNewCommitteeValidatorHash: Nothing
        }

  uchp' ← f uchp

  unliftApp params.env $ UpdateCommitteeHash.updateCommitteeHash uchp'

-- | `tests` aggregates all UpdateCommitteeHash the tests.
tests ∷ WrappedTests
tests = testnetGroup "Committee handover (committe hash update)" $ do
  testScenario1
  testScenario2
  testScenario3
  testScenario4
  testScenario5

-- | 'testScenario1' updates the committee hash
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "Simple update committee hash"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 40_000_000
      , BigNum.fromInt 40_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      liftContract $ logInfo' "UpdateCommitteeHash 'testScenario1'"
      genesisUtxo ← Test.Utils.getOwnTransactionInput

      pkh ← getOwnPaymentPubKeyHash
      let
        keyCount = 40
      initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
        generatePrivKey
      let
        initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
        aggregatedCommittee = toData $ aggregateKeys $ map
          unwrap
          initCommitteePubKeys
        sidechainParams =
          SidechainParams
            { chainId: BigInt.fromInt 1
            , genesisUtxo
            , thresholdNumerator: BigInt.fromInt 2
            , thresholdDenominator: BigInt.fromInt 3
            , governanceAuthority: Governance.mkGovernanceAuthority pkh
            }

      _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
      _ ←
        initFuel sidechainParams
          zero
          aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
      nextCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
        generatePrivKey

      env ← ask
      liftContract $ updateCommitteeHash
        { sidechainParams
        , currentCommitteePrvKeys: initCommitteePrvKeys
        , newCommitteePrvKeys: nextCommitteePrvKeys
        , previousMerkleRoot: Nothing
        , sidechainEpoch: BigInt.fromInt 1
        , env
        }

      void
        $ GarbageCollector.mkBurnNFTsLookupsAndConstraints sidechainParams
        >>= balanceSignAndSubmit "Test: burn NFTs"

-- | `testScenario2` updates the committee hash with a threshold ratio of 1/1,
-- | but should fail because there isn't enough committee members signing the update
-- | off.
testScenario2 ∷ TestnetTest
testScenario2 =
  Mote.Monad.test "Update committee hash without honest majority (should fail)"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        liftContract $ logInfo' "UpdateCommitteeHash 'testScenario2'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 2
        -- woohoo!! smaller committee size so it's easy to remove the majority
        -- sign below, and make this test case fail...
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        { sidechainParams: scParams } ← initTokensMint sidechainParams
          ATMSPlainEcdsaSecp256k1
          1
        _ ←
          initFuel sidechainParams
            zero
            aggregatedCommittee
            ATMSPlainEcdsaSecp256k1
            1
        nextCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey

        env ← ask
        liftContract $ Test.Utils.fails
          $ updateCommitteeHashWith
              { sidechainParams: scParams
              , currentCommitteePrvKeys: initCommitteePrvKeys
              , newCommitteePrvKeys: nextCommitteePrvKeys
              , previousMerkleRoot: Nothing
              , sidechainEpoch: BigInt.fromInt 1
              , env
              }
          $ \(UpdateCommitteeHashParams params) →
              pure
                $ UpdateCommitteeHashParams
                $ params
                    { aggregateSignature =
                        unsafePartial $
                          case params.aggregateSignature of
                            PlainEcdsaSecp256k1
                              [ c1 /\ _s1, c2 /\ s2 ] →
                              PlainEcdsaSecp256k1
                                [ c1 /\ Nothing
                                , c2 /\ s2
                                ]
                    }

-- | `testScenario3` initialises the committee with an out of order committee
-- | (by moving the smallest committee member to the end), and updates the committee
-- | hash when the signatures / new committee are given out of order (in
-- | reverse order actually); and updates it again
testScenario3 ∷ TestnetTest
testScenario3 =
  Mote.Monad.test "Update committee hash with out of order committee"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "UpdateCommitteeHash 'testScenario3'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 80
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
        let
          initCommitteePubKeys = Array.sort
            (map toPubKeyUnsafe initCommitteePrvKeys)
          aggregatedCommittee = toData $ aggregateKeys
            $ map unwrap
            $ case Array.uncons initCommitteePubKeys of
                Nothing → mempty
                Just { head, tail } → tail <> Array.singleton head
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 6
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ←
          initFuel sidechainParams
            zero
            aggregatedCommittee
            ATMSPlainEcdsaSecp256k1
            1
        nextCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
        nextNextCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey

        let
          reverseSignaturesAndNewCommittee ∷
            UpdateCommitteeHashParams PlutusData →
            UpdateCommitteeHashParams PlutusData
          reverseSignaturesAndNewCommittee uchp =
            wrap
              ( (unwrap uchp)
                  { aggregateSignature =
                      Unsafe.unsafePartial $
                        case (unwrap uchp).aggregateSignature of
                          PlainEcdsaSecp256k1 sigs →
                            PlainEcdsaSecp256k1 $
                              Array.reverse sigs
                  }
              )
        env ← ask
        -- the first update
        liftContract $ updateCommitteeHashWith
          { sidechainParams
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , newCommitteePrvKeys: nextCommitteePrvKeys
          , previousMerkleRoot: Nothing
          , sidechainEpoch: BigInt.fromInt 1
          , env
          }
          (pure <<< reverseSignaturesAndNewCommittee)

        -- the second update
        liftContract $ updateCommitteeHash
          { sidechainParams
          , currentCommitteePrvKeys: nextCommitteePrvKeys
          , newCommitteePrvKeys: nextNextCommitteePrvKeys
          , previousMerkleRoot: Nothing
          , sidechainEpoch: BigInt.fromInt 2
          , env
          }

        void
          $ GarbageCollector.mkBurnNFTsLookupsAndConstraints sidechainParams
          >>= balanceSignAndSubmit "Test: burn NFTs"

-- | `testScenario4` is given in #277
testScenario4 ∷ TestnetTest
testScenario4 =
  Mote.Monad.test "Unsorted committee members (issue #277)"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        liftContract $ logInfo' "UpdateCommitteeHash 'testScenario3'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        pkh ← getOwnPaymentPubKeyHash
        -- the committees as given in the test case
        let
          initCommitteePrvKeys =
            [ byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "3e77009e691a2c38c53d5c0608af90af5c793efaa6cfe9e8670b141ed0376911"
            , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "d9465fedde9190b2760bb37ac2b89cf97d7121a98807f8849532e58750d23725"
            , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "3563a2e4d2b373b4b8ea0397b7437e7386d3d39216a77fa3ceb8f64a43d98f56"
            ]
          nextCommitteePrvKeys =
            [ byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "1b7267b5d84a108d67bd8cdc95750d135c1a1fb6482531ddfa0923c043b308f1"
            , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "173d5d8cd43bd6119c633e654d00bebc2165e6875190b132dc93d5ee1b7d2448"
            , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe $ hexToByteArrayUnsafe
                "34edb67b9f73389280214dae93e62074a9fcfd1eefadd4406cd7d27fd64b46a8"
            ]

          -- initialising the committee (translated from the CLI command
          aggregatedCommittee = toData $ aggregateKeys
            $ map unwrap
            $
              [ byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                  "03d9e83bde65acf38fc97497210d7e6f6a1aebf5d4cd9b193c90b81a81c55bc678"
              , byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                  "03885cccf474b81faba56097f58fcca98a3c8986bc09cdbd163e54add33561f34c"
              , byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                  "032aa087b8e4a983a7220e1d2eb2db6a6bf8fbed9fad7f5af6824e05f0017c69e0"
              ]
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 78
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ←
          initFuel sidechainParams
            zero
            aggregatedCommittee
            ATMSPlainEcdsaSecp256k1
            1
        env ← ask
        liftContract $ updateCommitteeHashWith
          { sidechainParams
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , newCommitteePrvKeys: nextCommitteePrvKeys
          , previousMerkleRoot: Nothing
          , sidechainEpoch: BigInt.fromInt 2
          , env
          }
          \uchp →
            pure $ wrap $ (unwrap uchp)
              { newAggregatePubKeys = toData $ aggregateKeys
                  $ map unwrap
                  $
                    [ byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                        "02b37ba1e0a18e8b3723e57fb6b220836ba6417ab75296f08f717106ad731ac47b"
                    , byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                        "02cb793bcfcab7f17453f4c5e0e07a2818c6df4d7995aa1b7a0f0b219c6cfe0e20"
                    , byteArrayToEcdsaSecp256k1PubKeyUnsafe $ hexToByteArrayUnsafe
                        "0377c83c74fbccf05671697bf343a71a9c221568721c8e77f330fe82e9b08fdfea"
                    ]
              -- the signatures from the issue aren't quite right (since it
              -- didn't order the committee), so we won't include those
              -- signatures
              }

        void
          $ GarbageCollector.mkBurnNFTsLookupsAndConstraints sidechainParams
          >>= balanceSignAndSubmit "Test: burn NFTs"

-- | `testScenario5` is essentially `testScenario2` but updates the committee
-- | with exactly the required signatures instead.
testScenario5 ∷ TestnetTest
testScenario5 =
  Mote.Monad.test
    "Update committee hash with the exact amount of signatures needed"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        liftContract $ logInfo' "UpdateCommitteeHash 'testScenario2'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        ownPkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 4
        initCommitteePrvKeys ← liftEffect
          $ sequence
          $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority ownPkh
              }

        { sidechainParams: scParams } ← initTokensMint sidechainParams
          ATMSPlainEcdsaSecp256k1
          1
        _ ←
          initFuel sidechainParams
            zero
            aggregatedCommittee
            ATMSPlainEcdsaSecp256k1
            1
        nextCommitteePrvKeys ← liftEffect
          $ sequence
          $ Array.replicate keyCount generatePrivKey

        env ← ask
        liftContract
          $ updateCommitteeHashWith
              { sidechainParams: scParams
              , currentCommitteePrvKeys: initCommitteePrvKeys
              , newCommitteePrvKeys: nextCommitteePrvKeys
              , previousMerkleRoot: Nothing
              , sidechainEpoch: BigInt.fromInt 1
              , env
              }
          $ \(UpdateCommitteeHashParams params) →
              pure
                $ UpdateCommitteeHashParams
                $ params
                    { aggregateSignature =
                        Unsafe.unsafePartial
                          ( case params.aggregateSignature of
                              PlainEcdsaSecp256k1
                                [ c1 /\ _s1
                                , c2 /\ s2
                                , c3 /\ s3
                                , c4 /\ s4
                                ] → PlainEcdsaSecp256k1
                                [ c1 /\ Nothing
                                , c2 /\ s2
                                , c3 /\ s3
                                , c4 /\ s4
                                ]
                          )
                    }

        void
          $ GarbageCollector.mkBurnNFTsLookupsAndConstraints scParams
          >>= balanceSignAndSubmit "Test: burn NFTs"
