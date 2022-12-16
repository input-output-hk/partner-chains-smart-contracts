-- | Provides some integration unit tests with the on chain implementation of
-- | the merkle tree.
module Test.MerkleTree (test) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Prim.ByteArray as ByteArray
import Data.Maybe as Maybe
import Effect.Class.Console as Console
import MerkleTree
  ( MerkleProof(MerkleProof)
  , MerkleTree(Bin, Tip)
  , RootHash(RootHash)
  , Side(L, R)
  , Up(Up)
  )
import MerkleTree as MerkleTree
import Partial.Unsafe as Unsafe
import Test.Utils (assertBy)

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

-- | These tests are integration tests which are unit testing to see if the
-- | outputs from the Haskell implementation are the same as the values we are
-- | creating on the purescript end.
-- | This mainly verifies that we are serializing / deserializing things properly
-- | on the purescript side.
test ∷ Effect Unit
test = do
  -- Testing if
  -- ```
  -- fromList ["maltese", "yorkie", "pomeranian"]
  -- ```
  -- produces the same tree.
  void do
    let
      expected =
        Bin
          ( RootHash
              ( hexToByteArrayUnsafe
                  "f5259910f2ce05d66ec99cd1ff8a0ef983fb8a6123cf118e18f21e346d141cc7"
              )
          )
          ( Tip
              ( RootHash
                  ( hexToByteArrayUnsafe
                      "a8ae306d7dddba230de2a8844d36d4ed5efcf9f4d32b01dd96f8e800a564ef31"
                  )
              )
          )
          ( Bin
              ( RootHash
                  ( hexToByteArrayUnsafe
                      "4887abf7a75e4c9e4a4abf38378d992b629c16e69d233b21702be642920fa4a2"
                  )
              )
              ( Tip
                  ( RootHash
                      ( hexToByteArrayUnsafe
                          "9e9979ffe1ec57d45fdafe56c8741f1093688f2a8639f0f682400709416a9f40"
                      )
                  )
              )
              ( Tip
                  ( RootHash
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
    assertBy eqUpToLeft (Right expected) actual

  -- Testing if
  -- ```
  -- fromList []
  -- ```
  -- both return error
  void do
    let
      expected = Left "bad"
      actual = MerkleTree.fromArray []
    assertBy eqUpToLeft expected actual

  -- Testing if
  -- ```
  -- let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
  -- in memberMp "maltese" merkleTree
  -- ```
  -- produce the same `Maybe MerkleProof`.
  void do
    let
      expected =
        Just
          ( MerkleProof
              [ ( Up
                    { siblingSide: R
                    , sibling:
                        ( RootHash
                            ( hexToByteArrayUnsafe
                                "f10e71fd4b801fe5989221bc5698ea2848bcdd9b271d3c37490109783a14c052"
                            )
                        )
                    }
                )
              , ( Up
                    { siblingSide: L
                    , sibling:
                        ( RootHash
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
        pure $ MerkleTree.lookupMp (unsafeByteArrayFromAscii "maltese") merkleTree
    assertBy eqUpToLeft (Right expected) actual

  -- Testing if
  -- ```
  -- let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
  -- in memberMp "pug" merkleTree
  -- ```
  -- produce the same `Maybe MerkleProof`.
  void do
    let
      expected = Nothing
      actual = do
        merkleTree ← MerkleTree.fromArray
          [ unsafeByteArrayFromAscii "maltese"
          , unsafeByteArrayFromAscii "yorkie"
          , unsafeByteArrayFromAscii "pomeranian"
          ]
        pure $ MerkleTree.lookupMp (unsafeByteArrayFromAscii "pug") merkleTree
    assertBy eqUpToLeft (Right expected) actual

  -- Testing if
  -- ```
  -- let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
  --     Just merkleProof = lookupMp "maltese" merkleTree
  -- in memberMp "maltese" merkleProof (rootHash merkleTree)
  -- ```
  -- are both true.
  void do
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
    assertBy eqUpToLeft (Right expected) actual

  -- Testing if
  -- ```
  -- let merkleTree = fromList ["maltese", "yorkie", "pomeranian"]
  --     Just merkleProof = lookupMp "maltese" merkleTree
  -- in memberMp "pug" merkleProof (rootHash merkleTree)
  -- ```
  -- are both False.
  void do
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
        pure $ MerkleTree.memberMp (unsafeByteArrayFromAscii "pug") merkleProof
          (MerkleTree.rootHash merkleTree)
    assertBy eqUpToLeft (Right expected) actual

  Console.info "Merkle tree integration tests succeeded"
