module Test.MerkleProofSerialisation (tests) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (PlutusData(Constr, Integer, Bytes, List))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray as ByteArray
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Test.Unit (Test)
import Test.Unit.Assert as Test.Unit.Assert
import Test.Utils (WrappedTests, pureGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof(CombinedMerkleProof)
  , MerkleTreeEntry(MerkleTreeEntry)
  )
import TrustlessSidechain.MerkleTree
  ( MerkleProof(MerkleProof)
  , Side(L, R)
  , Up(Up)
  , byteArrayToRootHashUnsafe
  )
import TrustlessSidechain.Utils.Address
  ( byteArrayToBech32BytesUnsafe
  )

-- | `MerkleProofSerialisationTest` is a convenient type alias around `Mote`
-- | wrapping `Test` with no bracketting for setup
type MerkleProofSerialisationTest = Mote (Const Void) Test Unit

-- | `tests` follows the integration tests described in #249. We have a test
-- | cases for:
-- |    1. Testing if the plutus data representation, AND the hex encoded cbor of the plutus data is as expected
-- |    2. Testing if hex encoded cbor is as expected.
tests ∷ WrappedTests
tests = pureGroup "Merkle proof serialisation for #249" do
  test1
  test2
  test3

combinedMerkleProofTestCase ∷ CombinedMerkleProof
combinedMerkleProofTestCase =
  CombinedMerkleProof
    { transaction: MerkleTreeEntry
        { index: Test.Utils.unsafeBigIntFromString "-8858258933817599851"
        , amount: Test.Utils.unsafeBigIntFromString "8887194232705394223"
        , recipient: byteArrayToBech32BytesUnsafe $
            ByteArray.hexToByteArrayUnsafe
              "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
        , previousMerkleRoot: Just
            ( byteArrayToRootHashUnsafe $ ByteArray.hexToByteArrayUnsafe
                "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
            )
        }
    , merkleProof: MerkleProof
        [ Up
            { siblingSide: L
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
            }
        , Up
            { siblingSide: R
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
            }
        , Up
            { siblingSide: L
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
            }
        , Up
            { siblingSide: L
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
            }
        , Up
            { siblingSide: R
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
            }
        , Up
            { siblingSide: R
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
            }
        , Up
            { siblingSide: L
            , sibling: byteArrayToRootHashUnsafe $
                ByteArray.hexToByteArrayUnsafe
                  "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
            }
        ]
    }

-- | `test1` is an integration test of serialization to plutus data
test1 ∷ MerkleProofSerialisationTest
test1 =
  Mote.Monad.test
    "Merkle Proof seralised to Plutus data matches expected value"
    $
      Test.Unit.Assert.assert "expected different plutus data"
    $
      let
        testCase = combinedMerkleProofTestCase

        expectedPlutusData =
          Constr (BigNum.fromInt 0)
            [ Constr (BigNum.fromInt 0)
                [ Integer $ Test.Utils.unsafeBigIntFromString "-8858258933817599851"
                , Integer $ Test.Utils.unsafeBigIntFromString "8887194232705394223"
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
                , Constr (BigNum.fromInt 0)
                    [ Bytes $ ByteArray.hexToByteArrayUnsafe
                        "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                    ]
                ]
            , List
                [ Constr (BigNum.fromInt 0)
                    [ Integer zero
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer one
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer zero
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer zero
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer one
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer one
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                    ]
                , Constr (BigNum.fromInt 0)
                    [ Integer zero
                    , Bytes $ ByteArray.hexToByteArrayUnsafe
                        "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                    ]
                ]
            ]
      in
        expectedPlutusData == PlutusData.toData testCase

-- | `test2` is an integration test of serialization to cbor
test2 ∷ MerkleProofSerialisationTest
test2 =
  Mote.Monad.test
    "Merkle Proof serialised to CBOR matches expected value"
    $
      Test.Unit.Assert.assert "expected different cbor"
    $
      let
        testCase = combinedMerkleProofTestCase
        expectedCbor = ByteArray.hexToCborBytesUnsafe
          "d8799fd8799f3b7aeedb9136a0e76a1b7b55a8117a4e262f5820ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180d8799f5820803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01ffff9fd8799f005820595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004bffd8799f0158208073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ffffd8799f00582000ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162ffd8799f005820803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2ffd8799f0158207b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301ffd8799f015820a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079ffd8799f005820a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00ffffff"
      in
        expectedCbor == PlutusData.serializeData testCase

-- | `test3` is an integration test to see if the hex encoded cbor is as
-- | expected
test3 ∷ MerkleProofSerialisationTest
test3 =
  Mote.Monad.test
    "Merkle Proof serialised to CBOR matches expected value"
    $
      Test.Unit.Assert.assert "expected different plutus data"
    $
      let
        testCase =
          CombinedMerkleProof
            { transaction: MerkleTreeEntry
                { index: Test.Utils.unsafeBigIntFromString "12542"
                , amount: Test.Utils.unsafeBigIntFromString "539422"
                , recipient: byteArrayToBech32BytesUnsafe $
                    ByteArray.hexToByteArrayUnsafe
                      "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
                , previousMerkleRoot:
                    Just
                      $ byteArrayToRootHashUnsafe
                      $
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                            )
                        )
                }
            , merkleProof: MerkleProof
                [ Up
                    { siblingSide: L
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                            )
                        )
                    }
                , Up
                    { siblingSide: R
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                            )
                        )
                    }
                , Up
                    { siblingSide: L
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                            )
                        )
                    }
                , Up
                    { siblingSide: L
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                            )
                        )
                    }
                , Up
                    { siblingSide: R
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                            )
                        )
                    }
                , Up
                    { siblingSide: R
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                            )
                        )
                    }
                , Up
                    { siblingSide: L
                    , sibling: byteArrayToRootHashUnsafe
                        ( ByteArray.hexToByteArrayUnsafe
                            ( "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                            )
                        )
                    }
                ]
            }
        expectedCbor = ByteArray.hexToCborBytesUnsafe
          "d8799fd8799f1930fe1a00083b1e5820ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180d8799f5820803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01ffff9fd8799f005820595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004bffd8799f0158208073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ffffd8799f00582000ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162ffd8799f005820803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2ffd8799f0158207b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301ffd8799f015820a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079ffd8799f005820a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00ffffff"
      in
        expectedCbor == PlutusData.serializeData testCase
