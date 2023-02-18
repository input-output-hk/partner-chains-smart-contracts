-- | Provides some integration unit tests with the on chain implementation of
-- | the merkle tree.
module Test.MerkleTree (tests) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Prim.ByteArray as ByteArray
import Data.Const (Const)
import Data.Maybe as Maybe
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Partial.Unsafe as Unsafe
import Test.Unit (Test)
import Test.Unit.Assert as Test.Unit.Assert
import Test.Utils (WrappedTests, pureGroup)
import TrustlessSidechain.MerkleTree
  ( MerkleProof(MerkleProof)
  , MerkleTree(Bin, Tip)
  , Side(L, R)
  , Up(Up)
  , byteArrayToRootHashUnsafe
  )
import TrustlessSidechain.MerkleTree as MerkleTree

-- | `MerkleTreeTest` is a convenient type alias around `Mote`
-- | wrapping `Test` with no bracketting for setup
type MerkleTreeTest = Mote (Const Void) Test Unit

-- | These tests are integration tests which are unit testing to see if the
-- | outputs from the Haskell implementation are the same as the values we are
-- | creating on the purescript end.
-- | This mainly verifies that we are serializing / deserializing things properly
-- | on the purescript side.
-- |
-- | `tests` wraps up all the merkle tree tests conveniently, so one may execute
-- | ```
-- | Test.Unit.Main.runTest (interpretMerkleTreeTest tests)
-- | ```
-- | to run all tests
tests ∷ WrappedTests
tests = pureGroup "Merkle tree integration tests" do
  test1
  test2
  test3
  test4
  test5
  test6

-- | `unsafeByteArrayFromAscii` is a partial function which wraps `Types.ByteArray.byteArrayFromAscii`
unsafeByteArrayFromAscii ∷ String → ByteArray
unsafeByteArrayFromAscii = Unsafe.unsafePartial
  (Maybe.fromJust <<< ByteArray.byteArrayFromAscii)

-- | `eqUpToLeft` returns `true` if either: both arguments are `Right` and the
-- | arguments are equal; or both arguments are `Left` (note the arguments to left
-- | don't have to be equal).
eqUpToLeft ∷ ∀ a b. Eq b ⇒ Either a b → Either a b → Boolean
eqUpToLeft (Right a0) (Right a1) = a0 == a1
eqUpToLeft (Left _) (Left _) = true
eqUpToLeft _ _ = false

-- | Testing if
-- | ```
-- | fromList ["maltese", "yorkie", "pomeranian"]
-- | ```
-- | produces the same tree as generated from the Haskell side.
test1 ∷ MerkleTreeTest
test1 =
  Mote.Monad.test
    "`fromList [\"maltese\", \"yorkie\", \"pomeranian\"]` is same as hardcoded tree"
    $
      Test.Unit.Assert.assert "expected different tree"
    $
      let
        expected =
          Bin
            ( byteArrayToRootHashUnsafe
                ( hexToByteArrayUnsafe
                    "f5259910f2ce05d66ec99cd1ff8a0ef983fb8a6123cf118e18f21e346d141cc7"
                )
            )
            ( Tip
                ( byteArrayToRootHashUnsafe
                    ( hexToByteArrayUnsafe
                        "a8ae306d7dddba230de2a8844d36d4ed5efcf9f4d32b01dd96f8e800a564ef31"
                    )
                )
            )
            ( Bin
                ( byteArrayToRootHashUnsafe
                    ( hexToByteArrayUnsafe
                        "4887abf7a75e4c9e4a4abf38378d992b629c16e69d233b21702be642920fa4a2"
                    )
                )
                ( Tip
                    ( byteArrayToRootHashUnsafe
                        ( hexToByteArrayUnsafe
                            "9e9979ffe1ec57d45fdafe56c8741f1093688f2a8639f0f682400709416a9f40"
                        )
                    )
                )
                ( Tip
                    ( byteArrayToRootHashUnsafe
                        ( hexToByteArrayUnsafe
                            "f10e71fd4b801fe5989221bc5698ea2848bcdd9b271d3c37490109783a14c052"
                        )
                    )
                )
            )
        actual = MerkleTree.fromArray
          [ unsafeByteArrayFromAscii "maltese"
          , unsafeByteArrayFromAscii "yorkie"
          , unsafeByteArrayFromAscii "pomeranian"
          ]
      in
        eqUpToLeft (Right expected) actual

-- | Testing if
-- | ```
-- | fromList []
-- | ```
-- | both returns an error
test2 ∷ MerkleTreeTest
test2 =
  Mote.Monad.test
    "`fromList []` is an error"
    $ Test.Unit.Assert.assert
        "expected error"
    $
      let
        expected = Left "bad"
        actual = MerkleTree.fromArray []
      in
        eqUpToLeft expected actual

-- | Testing if
-- | ```
-- | let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
-- | in lookupMp "maltese" merkleTree
-- | ```
-- | produce the same `Maybe MerkleProof`.
test3 ∷ MerkleTreeTest
test3 =
  Mote.Monad.test
    "`let merkleTree = fromList [\"maltese\", \"yorkie\", \"pomeranian\"]\
    \ in lookupMp \"maltese\" merkleTree` produces same hardcoded merkle proof"
    $
      Test.Unit.Assert.assert
        "expected different merkle proof"
    $
      let
        expected =
          Just
            ( MerkleProof
                [ ( Up
                      { siblingSide: R
                      , sibling:
                          ( byteArrayToRootHashUnsafe
                              ( hexToByteArrayUnsafe
                                  "f10e71fd4b801fe5989221bc5698ea2848bcdd9b271d3c37490109783a14c052"
                              )
                          )
                      }
                  )
                , ( Up
                      { siblingSide: L
                      , sibling:
                          ( byteArrayToRootHashUnsafe
                              ( hexToByteArrayUnsafe
                                  "a8ae306d7dddba230de2a8844d36d4ed5efcf9f4d32b01dd96f8e800a564ef31"
                              )
                          )
                      }
                  )
                ]
            )
        actual = do
          merkleTree ← MerkleTree.fromArray
            [ unsafeByteArrayFromAscii "maltese"
            , unsafeByteArrayFromAscii "yorkie"
            , unsafeByteArrayFromAscii "pomeranian"
            ]
          pure $ MerkleTree.lookupMp (unsafeByteArrayFromAscii "maltese")
            merkleTree
      in
        eqUpToLeft (Right expected) actual

-- | Testing if
-- | ```
-- | let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
-- | in lookupMp "pug" merkleTree
-- | ```
-- | produce does not produce a merkle proof
test4 ∷ MerkleTreeTest
test4 =
  Mote.Monad.test
    "`let merkleTree = fromList [\"maltese\", \"yorkie\", \"pomeranian\"]\
    \ in lookupMp \"pug\" merkleTree` produces no merkle proof"
    $ Test.Unit.Assert.assert "expected no merkle proof"
    $
      let
        expected = Nothing
        actual = do
          merkleTree ← MerkleTree.fromArray
            [ unsafeByteArrayFromAscii "maltese"
            , unsafeByteArrayFromAscii "yorkie"
            , unsafeByteArrayFromAscii "pomeranian"
            ]
          pure $ MerkleTree.lookupMp (unsafeByteArrayFromAscii "pug") merkleTree
      in
        eqUpToLeft (Right expected) actual

-- | Testing if
-- | ```
-- | let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
-- |     Just merkleProof = lookupMp "maltese" merkleTree
-- | in memberMp "maltese" merkleProof (rootHash merkleTree)
-- | ```
-- | is true
test5 ∷ MerkleTreeTest
test5 =
  Mote.Monad.test
    "`let merkleTree = fromList [\"maltese\", \"yorkie\", \"pomeranian\"]\
    \     Just merkleProof = lookupMp \"maltese\" merkleTree\
    \ in memberMp \"maltese\" merkleProof (rootHash merkleTree)` is true"
    $
      Test.Unit.Assert.assert "expected true"
    $
      let
        expected = true
        actual = do
          merkleTree ← MerkleTree.fromArray
            [ unsafeByteArrayFromAscii "maltese"
            , unsafeByteArrayFromAscii "yorkie"
            , unsafeByteArrayFromAscii "pomeranian"
            ]
          let
            maybeMerkleProof = MerkleTree.lookupMp
              (unsafeByteArrayFromAscii "maltese")
              merkleTree
            merkleProof = Unsafe.unsafePartial Maybe.fromJust maybeMerkleProof
          pure $ MerkleTree.memberMp (unsafeByteArrayFromAscii "maltese")
            merkleProof
            (MerkleTree.rootHash merkleTree)
      in
        eqUpToLeft (Right expected) actual

-- | Testing if
-- | ```
-- | let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
-- |     Just merkleProof = lookupMp "maltese" merkleTree
-- | in memberMp "pug" merkleProof (rootHash merkleTree)
-- | ```
-- | is False
test6 ∷ MerkleTreeTest
test6 =
  Mote.Monad.test
    "`let merkleTree = fromList [\"maltese\", \"yorkie\", \"pomeranian\"]\
    \     Just merkleProof = lookupMp \"maltese\" merkleTree\
    \ in memberMp \"pug\" merkleProof (rootHash merkleTree)` is false"
    $ Test.Unit.Assert.assert
        "expected false"
    $
      let
        expected = false
        actual = do
          merkleTree ← MerkleTree.fromArray
            [ unsafeByteArrayFromAscii "maltese"
            , unsafeByteArrayFromAscii "yorkie"
            , unsafeByteArrayFromAscii "pomeranian"
            ]
          let
            maybeMerkleProof = MerkleTree.lookupMp
              (unsafeByteArrayFromAscii "maltese")
              merkleTree
            merkleProof = Unsafe.unsafePartial Maybe.fromJust maybeMerkleProof
          pure $ MerkleTree.memberMp (unsafeByteArrayFromAscii "pug")
            merkleProof
            (MerkleTree.rootHash merkleTree)
      in
        eqUpToLeft (Right expected) actual
