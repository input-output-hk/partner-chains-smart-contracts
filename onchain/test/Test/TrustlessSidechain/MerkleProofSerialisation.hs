module Test.TrustlessSidechain.MerkleProofSerialisation (
  testUp,
  testSide,
  testRootHash,
  testCombinedMerkleProof,
) where

import Data.ByteString (ByteString)
import Plutus.V1.Ledger.Bytes qualified as Bytes
import PlutusCore.Data (Data (B, Constr, I, List))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.IsData.Class (
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import TrustlessSidechain.MerkleTree (
  MerkleProof (MerkleProof),
  RootHash (RootHash),
  Side (L, R),
  Up (Up),
  sibling,
  siblingSide,
 )
import TrustlessSidechain.Types (
  CombinedMerkleProof (CombinedMerkleProof),
  MerkleTreeEntry (MerkleTreeEntry),
  cmpMerkleProof,
  cmpTransaction,
  mteAmount,
  mteIndex,
  mtePreviousMerkleRoot,
  mteRecipient,
 )
import Prelude qualified

-- | 'unsafeFromHex' unsafely converts hex to the byte representation
unsafeFromHex :: ByteString -> ByteString
unsafeFromHex bs = case Bytes.fromHex bs of
  Right bs' -> case Bytes.getLedgerBytes bs' of
    BuiltinByteString bs'' -> bs''
  Left err -> Prelude.error err

{- | 'testSide' includes some integration tests with the SC_EVM people for #249
 for the 'Side' type
-}
testSide :: TestTree
testSide =
  Tasty.testGroup
    "TrustlessSidechain.MerkleTree.Side plutus data integration tests"
    [ HUnit.testCase "L :: Side --> 0" $
        let expected = Builtins.mkI 0
            actual = toBuiltinData L
         in expected HUnit.@=? actual
    , HUnit.testCase "R :: Side --> 1" $
        let expected = Builtins.mkI 1
            actual = toBuiltinData R
         in expected HUnit.@=? actual
    , -- Some "property" based testing tests (but since the domain is so
      -- small, we just hardcode the entire sample space) for Side
      HUnit.testCase "Side -toBuiltinData-> Plutus Data -fromBuiltinData-> Side == id for (L :: Side)" $
        let expected = L
            actual = unsafeFromBuiltinData (toBuiltinData L)
         in expected HUnit.@=? actual
    , HUnit.testCase "Side -toBuiltinData-> Plutus Data -fromBuiltinData-> Side == id for (R :: Side)" $
        let expected = R
            actual = unsafeFromBuiltinData (toBuiltinData R)
         in expected HUnit.@=? actual
    , HUnit.testCase "PlutusData -fromBuiltinData-> Side -toBuiltinData-> PlutusData == id for 0" $
        let expected = Builtins.mkI 0
            actual = unsafeFromBuiltinData (Builtins.mkI 0)
         in expected HUnit.@=? actual
    , HUnit.testCase "PlutusData -fromBuiltinData-> Side -toBuiltinData-> PlutusData == id for 1" $
        let expected = Builtins.mkI 1
            actual = unsafeFromBuiltinData (Builtins.mkI 1)
         in expected HUnit.@=? actual
    ]

{- | 'testRootHash' includes some integration tests with the SC_EVM people for #249
 for the 'Side' type
-}
testRootHash :: TestTree
testRootHash =
  Tasty.testGroup
    "TrustlessSidechain.MerkleTree.RootHash plutus data integration tests"
    -- Hard coded integration tests for what #249 expects for RootHash
    [ HUnit.testCase "RootHash is encoded as a regular bytestring" $
        let expected = Builtins.mkB "maltese"
            actual = toBuiltinData $ RootHash "maltese"
         in expected HUnit.@=? actual
    , HUnit.testCase "RootHash is encoded as a regular bytestring" $
        let expected = Builtins.mkB "pomeranian"
            actual = toBuiltinData $ RootHash "pomeranian"
         in expected HUnit.@=? actual
    ]

-- | 'testUp' includes some integration tests as from #249.
testUp :: TestTree
testUp =
  Tasty.testGroup
    "TrustlessSidechain.MerkleTree.Up plutus data representation integration tests"
    [ HUnit.testCase "Up Plutus Data Representation" $
        let expected =
              Builtins.dataToBuiltinData $
                Constr
                  0
                  [ I 0
                  , B "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                  ]
            actual =
              IsData.toBuiltinData $
                Up
                  { siblingSide = L
                  , sibling = RootHash "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                  }
         in expected HUnit.@=? actual
    , HUnit.testCase "Up encoded in the 'natural' way" $
        let expected =
              Builtins.mkConstr
                0
                [ Builtins.mkI 1
                , Builtins.mkB "pomeranian"
                ]
            actual =
              toBuiltinData $
                Up
                  { siblingSide = R
                  , sibling = RootHash "pomeranian"
                  }
         in expected HUnit.@=? actual
    ]

-- | 'testCombinedMerkleProof' includes some integration tests as from #249.
testCombinedMerkleProof :: TestTree
testCombinedMerkleProof =
  Tasty.testGroup
    "TrustlessSidechain.MerkleTree.CombinedMerkleProof plutus data representation / serialisation integration tests"
    [ HUnit.testCase "CombinedMerkleProof Plutus data representation" $
        Builtins.dataToBuiltinData expectedPlutusData HUnit.@=? IsData.toBuiltinData combinedMerkleProof
    , HUnit.testCase "CombinedMerkleProof Plutus serialisation" $
        expectedCBor HUnit.@=? Builtins.serialiseData (IsData.toBuiltinData combinedMerkleProof)
    ]
  where
    combinedMerkleProof =
      CombinedMerkleProof
        { cmpTransaction =
            MerkleTreeEntry
              { mteIndex = -8858258933817599851
              , mteAmount = 8887194232705394223
              , mteRecipient =
                  BuiltinByteString
                    ( unsafeFromHex
                        "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
                    )
              , mtePreviousMerkleRoot =
                  Just
                    ( BuiltinByteString
                        ( unsafeFromHex
                            "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                        )
                    )
              }
        , cmpMerkleProof =
            MerkleProof
              [ Up
                  { siblingSide = L
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                            )
                        )
                  }
              , Up
                  { siblingSide = R
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                            )
                        )
                  }
              , Up
                  { siblingSide = L
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                            )
                        )
                  }
              , Up
                  { siblingSide = L
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                            )
                        )
                  }
              , Up
                  { siblingSide = R
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                            )
                        )
                  }
              , Up
                  { siblingSide = R
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                            )
                        )
                  }
              , Up
                  { siblingSide = L
                  , sibling =
                      RootHash
                        ( BuiltinByteString
                            ( unsafeFromHex
                                "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                            )
                        )
                  }
              ]
        }

    expectedPlutusData =
      Constr
        0
        [ Constr
            0
            [ I (-8858258933817599851)
            , I 8887194232705394223
            , B
                ( unsafeFromHex
                    "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
                )
            , Constr
                0
                [ B
                    ( unsafeFromHex
                        "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
                    )
                ]
            ]
        , List
            [ Constr
                0
                [ I 0
                , B
                    ( unsafeFromHex
                        "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
                    )
                ]
            , Constr
                0
                [ I 1
                , B
                    ( unsafeFromHex
                        "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
                    )
                ]
            , Constr
                0
                [ I 0
                , B
                    ( unsafeFromHex
                        "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
                    )
                ]
            , Constr
                0
                [ I 0
                , B
                    ( unsafeFromHex
                        "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
                    )
                ]
            , Constr
                0
                [ I 1
                , B
                    ( unsafeFromHex
                        "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
                    )
                ]
            , Constr
                0
                [ I 1
                , B
                    ( unsafeFromHex
                        "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
                    )
                ]
            , Constr
                0
                [ I 0
                , B
                    ( unsafeFromHex
                        "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
                    )
                ]
            ]
        ]

    expectedCBor =
      BuiltinByteString
        ( unsafeFromHex
            "d8799fd8799f3b7aeedb9136a0e76a1b7b55a8117a4e262f5820ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180d8799f5820803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01ffff9fd8799f005820595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004bffd8799f0158208073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ffffd8799f00582000ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162ffd8799f005820803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2ffd8799f0158207b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301ffd8799f015820a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079ffd8799f005820a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00ffffff"
        )
