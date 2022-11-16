module Test.MerkleProofSerialisation (test) where

import Contract.Prelude

import Contract.PlutusData (PlutusData(..))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray as ByteArray
import Effect.Class.Console as Console
import FUELMintingPolicy (CombinedMerkleProof(..), MerkleTreeEntry(..))
import MerkleTree (MerkleProof(..), RootHash(..), Side(..), Up(..))
import Test.Utils as Test.Utils
import Utils.SerialiseData as Utils.SerialiseData

-- | `test` follows the integration tests described in #249. We have a test case for:
-- | 1. Testing if the plutus data representation is as expected
-- | 2. Testing if the hex encoded cbor of the plutus data is as expected
test ∷ Effect Unit
test = do
  let
    testCase =
      CombinedMerkleProof
        { transaction: MerkleTreeEntry
            { index: Test.Utils.unsafeBigIntFromString "-8858258933817599851"
            , amount: Test.Utils.unsafeBigIntFromString "8887194232705394223"
            , recipient: ByteArray.hexToByteArrayUnsafe
                "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
            , previousMerkleRoot: Just
                ( ByteArray.hexToByteArrayUnsafe
                    "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                )
            }
        , merkleProof: MerkleProof
            [ Up
                { siblingSide: L
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                }
            , Up
                { siblingSide: R
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                }
            , Up
                { siblingSide: L
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                }
            , Up
                { siblingSide: L
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                }
            , Up
                { siblingSide: R
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                }
            , Up
                { siblingSide: R
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                }
            , Up
                { siblingSide: L
                , sibling: RootHash $ ByteArray.hexToByteArrayUnsafe
                    "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                }
            ]
        }

    expectedPlutusData =
      Constr zero $
        [ Constr zero $
            [ Integer $ Test.Utils.unsafeBigIntFromString "-8858258933817599851"
            , Integer $ Test.Utils.unsafeBigIntFromString "8887194232705394223"
            , Bytes $ ByteArray.hexToByteArrayUnsafe
                "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
            , Constr zero
                [ Bytes $ ByteArray.hexToByteArrayUnsafe
                    "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                ]
            ]
        , List
            [ Constr zero
                [ Integer zero
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                ]
            , Constr zero
                [ Integer one
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                ]
            , Constr zero
                [ Integer zero
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                ]
            , Constr zero
                [ Integer zero
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                ]
            , Constr zero
                [ Integer one
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                ]
            , Constr zero
                [ Integer one
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                ]
            , Constr zero
                [ Integer zero
                , Bytes $ ByteArray.hexToByteArrayUnsafe
                    "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                ]
            ]
        ]

  -- Plutus data test
  Console.info "Testing Merkle Proof Serialization integration plutus data..."
  Test.Utils.assertBy eq expectedPlutusData $ PlutusData.toData testCase
  Console.info
    "Testing Merkle Proof Serialization integration plutus data succeeded!"

  -- Cbor serialisation test
  Console.info
    "Testing Merkle Proof Serialization integration cbor of plutus data..."
  let
    expectedCbor = ByteArray.hexToByteArrayUnsafe
      "d8799fd8799f3b7aeedb9136a0e76a1b7b55a8117a4e262f5820ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180d8799f5820803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01ffff9fd8799f005820595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004bffd8799f0158208073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ffffd8799f00582000ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162ffd8799f005820803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2ffd8799f0158207b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301ffd8799f015820a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079ffd8799f005820a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00ffffff"
  Test.Utils.assertBy eq (Just expectedCbor) $ Utils.SerialiseData.serialiseData
    $ expectedPlutusData
  Console.info
    "Testing Merkle Proof Serialization integration cbor of plutus data succeeded!"

  pure unit
