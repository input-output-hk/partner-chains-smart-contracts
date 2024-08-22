module Test.Checkpoint
  ( saveCheckpointTest
  , notEnoughSignaturesTest
  , outOfOrderCheckpointTest
  , invalidCheckpointBlockHashTest
  , signedByUnknownCommitteeTest
  , committeeChangeCheckpointTest
  , tests
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array (mapWithIndex)
import Data.Array as Array
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (liftEffect) as Run
import Run.Except (note) as Run
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.UpdateCommitteeHash (updateCommitteeHash)
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSAggregateSignatures(PlainEcdsaSecp256k1)
  , ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Env (ask)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.Checkpoint (initCheckpoint)
import TrustlessSidechain.InitSidechain.FUEL (initFuel)
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  , aggregateKeys
  , generatePrivKey
  , multiSign
  , toPubKeyUnsafe
  )

generateCheckpointSignatures ∷
  { sidechainParams ∷ SidechainParams
  , currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  } →
  Maybe (Array (Tuple EcdsaSecp256k1PubKey EcdsaSecp256k1Signature))
generateCheckpointSignatures
  { sidechainParams
  , currentCommitteePrvKeys
  , newCheckpointBlockHash
  , newCheckpointBlockNumber
  , sidechainEpoch
  } = do
  let
    -- Order the private keys by lexicographical ordering of the signatures, so
    -- it's easy to give the sorted pubkey with its associated signature.
    currentCommitteePubKeys /\ currentCommitteePrvKeys' =
      Array.unzip
        $ Array.sortWith fst
        $ map (\prvKey → toPubKeyUnsafe prvKey /\ prvKey) currentCommitteePrvKeys

  checkpointMessage ← Checkpoint.serialiseCheckpointMessage $
    Checkpoint.CheckpointMessage
      { sidechainParams: sidechainParams
      , checkpointBlockHash: newCheckpointBlockHash
      , checkpointBlockNumber: newCheckpointBlockNumber
      , sidechainEpoch: sidechainEpoch
      }

  let
    committeeSignatures = Array.zip
      currentCommitteePubKeys
      (multiSign currentCommitteePrvKeys' checkpointMessage)

  pure committeeSignatures

tests ∷ WrappedTests
tests = testnetGroup "Checkpointing" $ do
  signedByUnknownCommitteeTest
  committeeChangeCheckpointTest
  saveCheckpointTest
  notEnoughSignaturesTest
  outOfOrderCheckpointTest
  invalidCheckpointBlockHashTest

saveCheckpointTest ∷ TestnetTest
saveCheckpointTest =
  Mote.Monad.test
    "Save checkpoint should succeed if enough signatures are provided"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'saveCheckpointTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 25
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0 -- same epoch checkpoint
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1 $ map
                (Just <$> _)
                committeeSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        void $ Checkpoint.saveCheckpoint saveCheckpointInput

notEnoughSignaturesTest ∷ TestnetTest
notEnoughSignaturesTest =
  Mote.Monad.test
    "Save checkpoint should fail if there are not enough signatures"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'notEnoughSignaturesTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 5
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0 -- same epoch checkpoint
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          committeeSignatures' = map (Just <$> _) committeeSignatures
          notEnoughSignatures = mapWithIndex
            ( \idx (Tuple pk sig) →
                if idx < 3 then Tuple pk Nothing
                else Tuple pk sig
            )
            committeeSignatures'

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1
                notEnoughSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        withUnliftApp Test.Utils.fails $ void $ Checkpoint.saveCheckpoint
          saveCheckpointInput

outOfOrderCheckpointTest ∷ TestnetTest
outOfOrderCheckpointTest =
  Mote.Monad.test
    "Save checkpoint should fail if the checkpoint block number is not strictly increasing"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'outOfOrderCheckpointTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 5
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        let
          newCheckpointBlockHash =
            hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 0 -- same block number checkpoint
          sidechainEpoch = BigInt.fromInt 0
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1 $ map
                (Just <$> _)
                committeeSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        withUnliftApp Test.Utils.fails $ void $ Checkpoint.saveCheckpoint
          saveCheckpointInput

invalidCheckpointBlockHashTest ∷ TestnetTest
invalidCheckpointBlockHashTest =
  Mote.Monad.test
    "Save checkpoint should fail if the checkpoint block hash is invalid"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'invalidCheckpointBlockHashTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 5
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe
            "aabbccddeeffgghhiijjkkllmmnnoo" -- same as genesis hash which is already checkpointed
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1 $ map
                (Just <$> _)
                committeeSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        withUnliftApp Test.Utils.fails $ void $ Checkpoint.saveCheckpoint
          saveCheckpointInput

-- | Typical scenario -- a checkpoint is provided by a committee
-- | that doesn't have its committee hash stored on chain
signedByUnknownCommitteeTest ∷ TestnetTest
signedByUnknownCommitteeTest =
  Mote.Monad.test
    "Save checkpoint should fail if the checkpoint is signed by an unknown committee"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'signedByUnknownCommitteeTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 5
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        newCommitteeKeys ← Run.liftEffect $ sequence $ Array.replicate 5
          generatePrivKey

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: newCommitteeKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        unknownCommitteeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1 $ map
                (Just <$> _)
                unknownCommitteeSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        withUnliftApp Test.Utils.fails $ void $ Checkpoint.saveCheckpoint
          saveCheckpointInput

-- | 1. Init sidechain checkpoints the genesis
-- | 2. UpdateCommitee changes the committee
-- | 3. SaveCheckpoint is called with the new committee (valid checkpoint)
committeeChangeCheckpointTest ∷ TestnetTest
committeeChangeCheckpointTest =
  Mote.Monad.test
    "Save checkpoint should succeed if the checkpoint is signed by the new committee"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ logInfo' "Checkpoint 'committeeChangeCheckpointTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash
        let
          keyCount = 5
        initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          keyCount
          generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          aggregatedCommittee = toData $ aggregateKeys $ map
            unwrap
            initCommitteePubKeys
          genesisHash = hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority pkh
              }

        _ ← initTokensMint sidechainParams ATMSPlainEcdsaSecp256k1 1
        _ ← initFuel sidechainParams zero aggregatedCommittee
          ATMSPlainEcdsaSecp256k1
          1
        _ ← initCheckpoint sidechainParams genesisHash ATMSPlainEcdsaSecp256k1 1

        newCommitteeKeys ← Run.liftEffect $ sequence $ Array.replicate 5
          generatePrivKey

        env ← ask

        liftContract $ updateCommitteeHash
          { sidechainParams
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , newCommitteePrvKeys: newCommitteeKeys
          , previousMerkleRoot: Nothing
          , sidechainEpoch: BigInt.fromInt 1
          , env
          }

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 1
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: newCommitteeKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          Run.note
            ( GenericInternalError
                "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            )
            $ generateCheckpointSignatures toSign

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , aggregateSignature: PlainEcdsaSecp256k1 $ map
                (Just <$> _)
                committeeSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        void $ Checkpoint.saveCheckpoint saveCheckpointInput
