module Test.TrustlessSidechain.MerkleProofSerialisation (testUp, testSide, testRootHash) where

import TrustlessSidechain.MerkleTree (RootHash (..), Side (..), Up (..))

import PlutusCore.Data (Data (..))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class (
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Prelude

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit

{- | 'testSide' includes some integration tests with the mamba people for #249
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

{- | 'testRootHash' includes some integration tests with the mamba people for #249
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
