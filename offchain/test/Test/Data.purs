module Test.Data (tests) where

import Contract.Prelude

import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  )
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.String.CodeUnits (fromCharArray)
import Mote.Monad (test)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
  ( Gen
  , arrayOf
  , chooseInt
  , elements
  , vectorOf
  )
import Test.Utils (WrappedTests, pureGroup)
import Test.Utils.Laws (toDataLaws)
import Test.Utils.QuickCheck
  ( ArbitraryAddress(ArbitraryAddress)
  , ArbitraryAssetClass(ArbitraryAssetClass)
  , ArbitraryBigInt(ArbitraryBigInt)
  , ArbitraryCurrencySymbol(ArbitraryCurrencySymbol)
  , ArbitraryPaymentPubKeyHash(ArbitraryPaymentPubKeyHash)
  , ArbitraryPubKey(ArbitraryPubKey)
  , ArbitrarySignature(ArbitrarySignature)
  , ArbitraryTransactionInput(ArbitraryTransactionInput)
  , ArbitraryValidatorHash(ArbitraryValidatorHash)
  , DA
  , NonNegative(NonNegative)
  , Positive(Positive)
  , liftArbitrary
  , suchThatMap
  )
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  )
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointParameter(CheckpointParameter)
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( BlockProducerRegistration(BlockProducerRegistration)
  , BlockProducerRegistrationMsg(BlockProducerRegistrationMsg)
  )
import TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)
  , DsDatum(DsDatum)
  , DsKeyMint(DsKeyMint)
  , Node(Node)
  )
import TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof(CombinedMerkleProof)
  , FUELMint(FUELMint)
  , MerkleTreeEntry(MerkleTreeEntry)
  )
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  , SignedMerkleRootRedeemer(SignedMerkleRootRedeemer)
  )
import TrustlessSidechain.MerkleTree
  ( MerkleProof(MerkleProof)
  , RootHash
  , Side(L, R)
  , Up(Up)
  , byteArrayToRootHashUnsafe
  )
import TrustlessSidechain.SidechainParams
  ( SidechainParams(SidechainParams)
  )
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import TrustlessSidechain.Utils.Address
  ( byteArrayToBech32BytesUnsafe
  )
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , ecdsaSecp256k1PubKey
  )

tests ∷ WrappedTests
tests = pureGroup "Data roundtrip tests" $ do
  test "SidechainParams" $ liftEffect $ toDataLaws testCount genSP
  test "EcdsaSecp256k1PubKey" $ liftEffect $ toDataLaws smallTestCount genPK
  test "CandidatePermissionMint" $ liftEffect $ toDataLaws testCount genCPM
  test "BlockProducerRegistration" $ liftEffect $ toDataLaws testCount genBPR
  test "BlockProducerRegistrationMsg" $ liftEffect $ toDataLaws testCount
    genBPRM
  -- BlockProducerRegistrationMsg?
  test "MerkleTreeEntry" $ liftEffect $ toDataLaws testCount genMTE
  test "MerkleRootInsertionMessage" $ liftEffect $ toDataLaws testCount genMRIM
  test "SignedMerkleRootRedeemer" $ liftEffect $ toDataLaws testCount genSMRR
  test "SignedMerkleRootMint" $ liftEffect $ toDataLaws smallTestCount genSMRM
  test "RootHash" $ liftEffect $ toDataLaws testCount genRH
  test "Side" $ liftEffect $ toDataLaws smallTestCount genSide
  test "Up" $ liftEffect $ toDataLaws testCount genUp
  test "MerkleProof" $ liftEffect $ toDataLaws testCount genMP
  test "CombinedMerkleProof" $ liftEffect $ toDataLaws smallTestCount genCMP
  test "FUELMint" $ liftEffect $ toDataLaws smallTestCount genFM
  -- FUELRedeemer not exported
  test "UpdateCommitteeDatum" $ liftEffect $ toDataLaws testCount genUCD
  test "UpdateCommitteeHash" $ liftEffect $ toDataLaws smallTestCount genUCH
  test "UpdateCommitteeHashMessage" $ liftEffect $ toDataLaws smallTestCount
    genUCHM
  test "UpdateCommitteeHashRedeemer" $ liftEffect $ toDataLaws testCount genUCHR
  test "CommitteeCertificateMint" $ liftEffect $ toDataLaws testCount genCCM
  test "CheckpointParameter" $ liftEffect $ toDataLaws smallTestCount genCP
  test "Ds" $ liftEffect $ toDataLaws testCount genDs
  test "DsDatum" $ liftEffect $ toDataLaws testCount genDsDatum
  test "DsConfDatum" $ liftEffect $ toDataLaws smallTestCount genDsConfDatum
  -- Ib not exported
  test "DsConfMint" $ liftEffect $ toDataLaws testCount genDsConfMint
  test "DsKeyMint" $ liftEffect $ toDataLaws testCount genDsKeyMint
  test "Node" $ liftEffect $ toDataLaws testCount genNode
  where
  testCount ∷ Int
  testCount = 10_000

  smallTestCount ∷ Int
  smallTestCount = 1_000

-- Generators

genBPRM ∷ Gen BlockProducerRegistrationMsg
genBPRM = do
  bprmSidechainParams ← genSP
  bprmSidechainPubKey ← genGH
  ArbitraryTransactionInput bprmInputUtxo ← arbitrary
  pure $ BlockProducerRegistrationMsg
    { bprmSidechainParams
    , bprmSidechainPubKey
    , bprmInputUtxo
    }

genNode ∷ Gen Node
genNode = do
  nKey ← genGH
  nNext ← genGH
  pure $ Node
    { nKey
    , nNext
    }

genDsKeyMint ∷ Gen DsKeyMint
genDsKeyMint = do
  ArbitraryValidatorHash dskmValidatorHash ← arbitrary
  ArbitraryCurrencySymbol dskmConfCurrencySymbol ← arbitrary
  pure $ DsKeyMint
    { dskmValidatorHash
    , dskmConfCurrencySymbol
    }

genDsConfMint ∷ Gen DsConfMint
genDsConfMint = DsConfMint <$> do
  ArbitraryTransactionInput ti ← arbitrary
  pure ti

genDsConfDatum ∷ Gen DsConfDatum
genDsConfDatum = do
  ArbitraryCurrencySymbol dscKeyPolicy ← arbitrary
  ArbitraryCurrencySymbol dscFUELPolicy ← arbitrary
  pure $ DsConfDatum
    { dscKeyPolicy
    , dscFUELPolicy
    }

genDsDatum ∷ Gen DsDatum
genDsDatum = DsDatum <$> genGH

genDs ∷ Gen Ds
genDs = Ds <$> do
  ArbitraryCurrencySymbol cs ← arbitrary
  pure cs

genCP ∷ Gen CheckpointParameter
genCP = do
  sidechainParams ← genSP
  ArbitraryAssetClass checkpointAssetClass ← arbitrary
  ArbitraryCurrencySymbol committeeOracleCurrencySymbol ← arbitrary
  ArbitraryCurrencySymbol committeeCertificateVerificationCurrencySymbol ←
    arbitrary
  pure $ CheckpointParameter
    { sidechainParams
    , checkpointAssetClass
    , committeeOracleCurrencySymbol
    , committeeCertificateVerificationCurrencySymbol
    }

genCCM ∷ Gen CommitteeCertificateMint
genCCM = do
  ArbitraryCurrencySymbol committeeOraclePolicy ← arbitrary
  Positive (ArbitraryBigInt thresholdNumerator) ← arbitrary
  Positive (ArbitraryBigInt thresholdDenominator) ← arbitrary
  pure $ CommitteeCertificateMint
    { committeeOraclePolicy
    , thresholdNumerator
    , thresholdDenominator
    }

genUCHR ∷ Gen UpdateCommitteeHashRedeemer
genUCHR = do
  previousMerkleRoot ← liftArbitrary genRH
  pure $ UpdateCommitteeHashRedeemer
    { previousMerkleRoot
    }

genUCHM ∷ Gen (UpdateCommitteeHashMessage DA)
genUCHM = do
  sidechainParams ← genSP
  newAggregatePubKeys ← arbitrary
  previousMerkleRoot ← liftArbitrary genRH
  Positive (ArbitraryBigInt sidechainEpoch) ← arbitrary
  ArbitraryAddress validatorAddress ← arbitrary
  pure $ UpdateCommitteeHashMessage
    { sidechainParams
    , newAggregatePubKeys
    , previousMerkleRoot
    , sidechainEpoch
    , validatorAddress
    }

genUCH ∷ Gen UpdateCommitteeHash
genUCH = do
  sidechainParams ← genSP
  ArbitraryCurrencySymbol committeeOracleCurrencySymbol ← arbitrary
  ArbitraryCurrencySymbol committeeCertificateVerificationCurrencySymbol ←
    arbitrary
  ArbitraryCurrencySymbol merkleRootTokenCurrencySymbol ← arbitrary
  pure $ UpdateCommitteeHash
    { sidechainParams
    , committeeOracleCurrencySymbol
    , committeeCertificateVerificationCurrencySymbol
    , merkleRootTokenCurrencySymbol
    }

genUCD ∷ Gen (UpdateCommitteeDatum DA)
genUCD = do
  aggregatePubKeys ← arbitrary
  Positive (ArbitraryBigInt sidechainEpoch) ← arbitrary
  pure $ UpdateCommitteeDatum
    { aggregatePubKeys
    , sidechainEpoch
    }

genFM ∷ Gen FUELMint
genFM = do
  ArbitraryCurrencySymbol merkleRootTokenCurrencySymbol ← arbitrary
  sidechainParams ← genSP
  ArbitraryCurrencySymbol dsKeyCurrencySymbol ← arbitrary
  pure $ FUELMint
    { merkleRootTokenCurrencySymbol
    , sidechainParams
    , dsKeyCurrencySymbol
    }

genCMP ∷ Gen CombinedMerkleProof
genCMP = do
  transaction ← genMTE
  merkleProof ← genMP
  pure $ CombinedMerkleProof
    { transaction
    , merkleProof
    }

genMP ∷ Gen MerkleProof
genMP = MerkleProof <$> arrayOf genUp

genUp ∷ Gen Up
genUp = do
  siblingSide ← genSide
  sibling ← genRH
  pure $ Up
    { siblingSide
    , sibling
    }

genSide ∷ Gen Side
genSide = elements $ NE.cons' L [ R ]

genSMRM ∷ Gen SignedMerkleRootMint
genSMRM = do
  sidechainParams ← genSP
  ArbitraryCurrencySymbol committeeCertificateVerificationCurrencySymbol ←
    arbitrary
  ArbitraryValidatorHash merkleRootValidatorHash ← arbitrary
  pure $ SignedMerkleRootMint
    { sidechainParams
    , committeeCertificateVerificationCurrencySymbol
    , merkleRootValidatorHash
    }

genSMRR ∷ Gen SignedMerkleRootRedeemer
genSMRR = do
  previousMerkleRoot ← liftArbitrary genRH
  pure $ SignedMerkleRootRedeemer
    { previousMerkleRoot
    }

genMRIM ∷ Gen MerkleRootInsertionMessage
genMRIM = do
  sidechainParams ← genSP
  merkleRoot ← genRH
  previousMerkleRoot ← liftArbitrary genRH
  pure $ MerkleRootInsertionMessage
    { sidechainParams
    , merkleRoot
    , previousMerkleRoot
    }

genMTE ∷ Gen MerkleTreeEntry
genMTE = do
  index ← genBigIntBits 32
  amount ← genBigIntBits 256
  recipient ← byteArrayToBech32BytesUnsafe <$> genGH
  previousMerkleRoot ← liftArbitrary genRH
  pure $ MerkleTreeEntry
    { index
    , amount
    , recipient
    , previousMerkleRoot
    }

genBPR ∷ Gen BlockProducerRegistration
genBPR = do
  ArbitraryPubKey bprSpoPubKey ← arbitrary
  bprSidechainPubKey ← genGH
  ArbitrarySignature bprSpoSignature ← arbitrary
  bprSidechainSignature ← genGH
  ArbitraryTransactionInput bprInputUtxo ← arbitrary
  ArbitraryPaymentPubKeyHash bprOwnPkh ← arbitrary
  pure $ BlockProducerRegistration
    { bprSpoPubKey
    , bprSidechainPubKey
    , bprSpoSignature
    , bprSidechainSignature
    , bprInputUtxo
    , bprOwnPkh
    }

genCPM ∷ Gen CandidatePermissionMint
genCPM = do
  sidechainParams ← genSP
  ArbitraryTransactionInput candidatePermissionTokenUtxo ← arbitrary
  pure $ CandidatePermissionMint
    { sidechainParams
    , candidatePermissionTokenUtxo
    }

genPK ∷ Gen EcdsaSecp256k1PubKey
genPK = suchThatMap (genByteArrayLen 33) ecdsaSecp256k1PubKey

genSP ∷ Gen SidechainParams
genSP = do
  NonNegative (ArbitraryBigInt chainId) ← arbitrary
  genesisHash ← genGH
  ArbitraryTransactionInput genesisUtxo ← arbitrary
  Positive (ArbitraryBigInt thresholdNumerator) ← arbitrary
  Positive (ArbitraryBigInt thresholdDenominator) ← arbitrary
  pure $ SidechainParams
    { chainId
    , genesisHash
    , genesisUtxo
    , thresholdNumerator
    , thresholdDenominator
    }

genRH ∷ Gen RootHash
genRH = byteArrayToRootHashUnsafe <$> genByteArrayLen 32

genGH ∷ Gen ByteArray
genGH = byteArrayFromIntArrayUnsafe <$> arrayOf (chooseInt 0 255)

genByteArrayLen ∷ Int → Gen ByteArray
genByteArrayLen len =
  byteArrayFromIntArrayUnsafe <$> vectorOf len (chooseInt 0 255)

-- Doing this is a bit tricky, as you can easily overflow if you use the naive
-- method, as Purescript limits Int to 32 bits (4 bytes), and the number is
-- signed.
--
-- Instead, we generate a binary string of required size, then convert it.
genBigIntBits ∷ Int → Gen BigInt
genBigIntBits bitSize = suchThatMap mkChars
  (fromCharArray >>> BigInt.fromBase 2)
  where
  mkChars ∷ Gen (Array Char)
  mkChars = vectorOf bitSize (elements (NE.cons' '0' [ '1' ]))
