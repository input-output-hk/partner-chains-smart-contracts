-- | `Test.MerkleRootChaining` includes tests which demonstrate the merkle root
-- | chaining with both updating the committee hash and creating new merkle roots.
module Test.MerkleRootChaining (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Log as Log
import Contract.PlutusData (toData)
import Contract.Wallet as Wallet
import Data.Array as Array
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (liftEffect) as Run
import Run.Except (note) as Run
import Test.MerkleRoot as Test.MerkleRoot
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.UpdateCommitteeHash as Test.UpdateCommitteeHash
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures(PlainEcdsaSecp256k1)
  , ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Env (ask)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.FUELMintingPolicy.V1
  ( MerkleTreeEntry(MerkleTreeEntry)
  )
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.FUEL (initFuel)
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

-- | `tests` aggregates all MerkleRootChaining tests together conveniently
tests ∷ WrappedTests
tests = testnetGroup "MerkleRootChaining tests" $ do
  testScenario1
  testScenario2

-- | `testScenario1` demonstrates (should succeed)
-- |    1. Initializing the sidechain with a committee.
-- |    2. Saving a merkle root
-- |    3. Update the committee hash
-- |    4. Update the committee hash
-- |    5. Saving a merkle root
-- |    6. Saving another merkle root
-- |    7. Updating the committee hash again
-- | Note how this demonstrates "working" behavior for arbitrarily many (well,
-- | 0-2) "saving merkle root" actions between the update committee hash
-- | actions
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "Merkle root chaining scenario 1"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      ownPaymentPubKeyHash ← getOwnPaymentPubKeyHash
      let
        ownRecipient = Test.MerkleRoot.paymentPubKeyHashToBech32Bytes
          ownPaymentPubKeyHash

      -- 1. Initializing the sidechain
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 1. Initializing the sidechain"
      genesisUtxo ← Test.Utils.getOwnTransactionInput

      let keyCount = 25 -- See partner-chains-smart-contracts#64
      committee1PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      let
        aggregatedCommittee = toData
          $ Utils.Crypto.aggregateKeys
          $ map unwrap
          $ map
              Utils.Crypto.toPubKeyUnsafe
              committee1PrvKeys
        sidechainParams =
          SidechainParams
            { chainId: BigInt.fromInt 69_420
            , genesisUtxo
            , thresholdNumerator: BigInt.fromInt 2
            , thresholdDenominator: BigInt.fromInt 3
            , governanceAuthority: Governance.mkGovernanceAuthority
                ownPaymentPubKeyHash
            }

      _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
      _ ←
        initFuel sidechainParams
          zero
          aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1

      -- 2. Saving a merkle root.
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 2. saving a merkle root"
      { merkleRoot: merkleRoot2 } ← Test.MerkleRoot.saveRoot
        { sidechainParams
        , merkleTreeEntries:
            [ MerkleTreeEntry
                { index: BigInt.fromInt 0
                , amount: BigInt.fromInt 69
                , previousMerkleRoot: Nothing
                , recipient: ownRecipient
                }
            ]
        , currentCommitteePrvKeys: committee1PrvKeys
        , previousMerkleRoot: Nothing
        }

      -- 3. Updating the committee hash
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 3. updating the committee hash"
      committee3PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      env ← ask
      liftContract $ Test.UpdateCommitteeHash.updateCommitteeHash
        { sidechainParams
        , currentCommitteePrvKeys: committee1PrvKeys
        , newCommitteePrvKeys: committee3PrvKeys
        , previousMerkleRoot: Just merkleRoot2
        , sidechainEpoch: BigInt.fromInt 1
        , env
        }

      -- 4. Updating the committee hash
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 4. updating the committee hash"
      committee4PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      liftContract $ Test.UpdateCommitteeHash.updateCommitteeHash
        { sidechainParams
        , currentCommitteePrvKeys: committee3PrvKeys
        , newCommitteePrvKeys: committee4PrvKeys
        , -- Note: this is the same merkle root as the last committee update.
          previousMerkleRoot: Just merkleRoot2
        , sidechainEpoch: BigInt.fromInt 2
        , env
        }

      -- 5. Saving a merkle root
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 5. saving the merkle root"
      { merkleRoot: merkleRoot5 } ← Test.MerkleRoot.saveRoot
        { sidechainParams
        , merkleTreeEntries:
            [ MerkleTreeEntry
                { index: BigInt.fromInt 0
                , amount: BigInt.fromInt 69
                , -- Note: this is the same merkle root as used in 4.
                  previousMerkleRoot: Just merkleRoot2
                , recipient: ownRecipient
                }
            ]
        , -- Note: the current committee is from 4.
          currentCommitteePrvKeys: committee4PrvKeys
        , previousMerkleRoot: Just merkleRoot2
        }

      -- 6. Saving a merkle root
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 6. saving the merkle root"
      { merkleRoot: merkleRoot6 } ← Test.MerkleRoot.saveRoot
        { sidechainParams
        , merkleTreeEntries:
            [ MerkleTreeEntry
                { index: BigInt.fromInt 0
                , amount: BigInt.fromInt 69
                , -- Note: this is the same merkle root as used in 5.
                  previousMerkleRoot: Just merkleRoot5
                , recipient: ownRecipient
                }
            ]
        , -- Note: the current committee is from 4.
          currentCommitteePrvKeys:
            committee4PrvKeys
        , previousMerkleRoot: Just merkleRoot5
        }

      -- 7. Updating the committee hash
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario1': 7. updating the committee hash"
      committee7PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      liftContract $ Test.UpdateCommitteeHash.updateCommitteeHash
        { sidechainParams
        , currentCommitteePrvKeys: committee4PrvKeys
        , newCommitteePrvKeys: committee7PrvKeys
        , previousMerkleRoot: Just merkleRoot6
        , sidechainEpoch: BigInt.fromInt 3
        , env
        }

      pure unit

-- | `testScenario2` demonstrates (should fail)
-- |    1. Initializing the sidechain with a committee.
-- |    2. Saving a merkle root
-- |    3. Attempt (but fail) to update the committee hash with the merkle root
-- |    as `Nothing`
testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "Merkle root chaining scenario 2 (should fail)"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      , BigNum.fromInt 100_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      ownPaymentPubKeyHash ← getOwnPaymentPubKeyHash
      let
        ownRecipient = Test.MerkleRoot.paymentPubKeyHashToBech32Bytes
          ownPaymentPubKeyHash

      -- 1. Initializing the sidechain
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario2': 1. Initializing the sidechain"
      genesisUtxo ← Test.Utils.getOwnTransactionInput

      let keyCount = 80
      committee1PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      let
        aggregatedCommittee = toData
          $ Utils.Crypto.aggregateKeys
          $ map unwrap
          $ map
              Utils.Crypto.toPubKeyUnsafe
              committee1PrvKeys
        sidechainParams =
          SidechainParams
            { chainId: BigInt.fromInt 69_420
            , genesisUtxo
            , thresholdNumerator: BigInt.fromInt 2
            , thresholdDenominator: BigInt.fromInt 3
            , governanceAuthority: Governance.mkGovernanceAuthority
                ownPaymentPubKeyHash
            }

      _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
      _ ←
        initFuel sidechainParams
          zero
          aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
      -- 2. Saving a merkle root
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario2': 2. saving the merkle root"
      { merkleRoot: merkleRoot2 } ← Test.MerkleRoot.saveRoot
        { sidechainParams
        , merkleTreeEntries:
            [ MerkleTreeEntry
                { index: BigInt.fromInt 0
                , amount: BigInt.fromInt 69
                , previousMerkleRoot: Nothing
                , recipient: ownRecipient
                }
            ]
        , currentCommitteePrvKeys: committee1PrvKeys
        , previousMerkleRoot: Nothing
        }

      -- 3. Updating the committee hash with the wrong merkle root.
      -------------------------------
      liftContract $ Log.logInfo'
        "'Test.MerkleRootChaining.testScenario2': 3. updating the committee hash incorrectly"
      -- create a new committee
      committee3PrvKeys ← Run.liftEffect $ sequence $ Array.replicate keyCount
        Utils.Crypto.generatePrivKey
      let
        committee1PubKeys = map Utils.Crypto.toPubKeyUnsafe committee1PrvKeys
        committee3PubKeys = map Utils.Crypto.toPubKeyUnsafe committee3PrvKeys
      -- the message updates committee1 to be committee3

      { validatorHash: updateCommitteeHashValidatorHash } ←
        UpdateCommitteeHash.getUpdateCommitteeHashValidator sidechainParams

      -- finally, build the message
      committee1Message ←
        Run.note
          ( GenericInternalError
              "error 'Test.MerkleRootChaining.testScenario2': failed to serialise and hash update committee hash message"
          )
          $ UpdateCommitteeHash.serialiseUchmHash
          $ UpdateCommitteeHashMessage
              { sidechainParams: sidechainParams
              , newAggregatePubKeys: Utils.Crypto.aggregateKeys $ map
                  unwrap
                  committee3PubKeys
              ,
                -- Note: since we can trust the committee will sign the "correct" root,
                -- we necessarily know that the message that they sign should be
                -- the previousMerkleRoot which is `merkleRoot2` in this case.
                previousMerkleRoot: Just merkleRoot2
              , sidechainEpoch: BigInt.fromInt 1
              , validatorHash: updateCommitteeHashValidatorHash
              }

      withUnliftApp Test.Utils.fails
        $ void
        $ UpdateCommitteeHash.updateCommitteeHash
        $
          UpdateCommitteeHashParams
            { sidechainParams
            , newAggregatePubKeys: Utils.Crypto.aggregateKeys $ map
                unwrap
                committee3PubKeys
            , aggregateSignature:
                PlainEcdsaSecp256k1 $
                  Array.zip
                    committee1PubKeys
                    ( Just <$> Utils.Crypto.multiSign committee1PrvKeys
                        committee1Message
                    )
            ,
              -- Note: this is the EVIL thing -- we try to update the
              -- committee hash without really putting in the previous merkle
              -- root
              previousMerkleRoot: Nothing
            , sidechainEpoch: BigInt.fromInt 1
            , mNewCommitteeValidatorHash: Nothing
            }
