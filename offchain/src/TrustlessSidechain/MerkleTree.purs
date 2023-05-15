-- | Implements merkle tree functionality
module TrustlessSidechain.MerkleTree
  ( Side(L, R)
  , MerkleTree(Bin, Tip)
  , Up(Up)
  , MerkleProof(MerkleProof)
  , RootHash
  , rootHashFromByteArray
  , byteArrayToRootHashUnsafe
  , fromList
  , lookupMp
  , fromArray
  , memberMp
  , rootMp
  , rootHash
  , unRootHash
  ) where

import Contract.Prelude

import Contract.Hashing as Hashing
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer, Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Data.Function as Function
import Data.List (List(Cons, Nil), (:))
import Data.String.Common as String
import Data.Unfoldable as Unfoldable

-- * Merkle tree data types
-- Each of these types should correspond the the on chain types.

-- | See `src/TrustlessSidechain/MerkleTree.hs`
-- | Invariants:
-- |    - the underlying `ByteArray` should be the image of `blake2b256Hash`
newtype RootHash = RootHash ByteArray

derive instance Eq RootHash

derive instance Ord RootHash

instance Show RootHash where
  show (RootHash byteArray) = "(" <> "byteArrayToRootHashUnsafe "
    <> show byteArray
    <> ")"

-- Note [`ToData` / `FromData` Instances of the Merkle Tree]
-- All of these instances should correspond to `/src/TrustlessSidechain/MerkleTree.hs`
derive newtype instance ToData RootHash

derive newtype instance FromData RootHash

-- | `byteArrayToRootHashUnsafe` constructs a `RootHash` from a given
-- | `ByteArray` without checking any invariants.
byteArrayToRootHashUnsafe ∷ ByteArray → RootHash
byteArrayToRootHashUnsafe = RootHash

-- | `rootHashFromByteArray` acts as a smart constructor for `RootHash`
-- | to wrap the provided `ByteArray` with the newtype constructor `RootHash`.
-- | This only verifies that the `ByteArray` is `256 bits * byte / 8 bit =
-- | 32 bytes` long since it is infeasible to verify that the given `ByteArray`
-- | is the image of a preimage resistant hash function
rootHashFromByteArray ∷ ByteArray → Maybe RootHash
rootHashFromByteArray byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ RootHash byteArray
  | otherwise = Nothing

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data Side
  = L
  | R

derive instance Eq Side

instance Show Side where
  show L = "L"
  show R = "R"

instance ToData Side where
  toData L = Integer zero
  toData R = Integer one

instance FromData Side where
  fromData plutusData = case plutusData of
    Integer n
      | n == zero → Just L
      | n == one → Just R
    _ → Nothing

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

instance Show MerkleTree where
  show (Bin h l r) = String.joinWith " " [ "Bin", show h, show l, show r ]
  show (Tip h) = String.joinWith " " [ "Tip", show h ]

instance Eq MerkleTree where
  eq (Bin rh0 l0 r0) (Bin rh1 l1 r1) =
    rh0 == rh1 && l0 == l1 && r0 == r1
  eq (Tip rh0) (Tip rh1) = rh0 == rh1
  eq _ _ = false

instance ToData MerkleTree where
  toData (Bin roothash l r) =
    Constr (BigNum.fromInt 0) [ toData roothash, toData l, toData r ]
  toData (Tip roothash) =
    Constr (BigNum.fromInt 1) [ toData roothash ]

instance FromData MerkleTree where
  fromData plutusData = case plutusData of
    Constr n args
      | n == (BigNum.fromInt 0) → case args of
          [ roothash, l, r ] → Bin <$> fromData roothash <*> fromData l <*>
            fromData r
          _ → Nothing
      | n == (BigNum.fromInt 1) → case args of
          [ roothash ] → Tip <$> fromData roothash
          _ → Nothing
    _ → Nothing

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype Up = Up { siblingSide ∷ Side, sibling ∷ RootHash }

derive instance Generic Up _

derive instance Newtype Up _

derive instance Eq Up

instance Show Up where
  show = genericShow

instance ToData Up where
  toData (Up record) = Constr (BigNum.fromInt 0)
    [ toData record.siblingSide, toData record.sibling ]

instance FromData Up where
  fromData plutusData = case plutusData of
    Constr n [ a, b ]
      | n == BigNum.fromInt 0 →
          Up <$> (({ siblingSide: _, sibling: _ }) <$> fromData a <*> fromData b)
    _ → Nothing

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype MerkleProof = MerkleProof (Array Up)

derive instance Generic MerkleProof _

derive instance Newtype MerkleProof _

derive instance Eq MerkleProof

instance Show MerkleProof where
  show = genericShow

derive newtype instance ToData MerkleProof

derive newtype instance FromData MerkleProof

-- * Internal helper functions

-- | `mergeRootHashes l r` computes `hash (1 : l ++ r)`
mergeRootHashes ∷ RootHash → RootHash → RootHash
mergeRootHashes l r = hashInternalNode (((<>) `Function.on` unRootHash) l r)

-- | `hashInternalNode b` computes `hash (1 : b)`
hashInternalNode ∷ ByteArray → RootHash
hashInternalNode = hash <<< (ByteArray.byteArrayFromIntArrayUnsafe [ 1 ] <> _)

-- | `hashLeaf b` computes `hash (0 : b)`
hashLeaf ∷ ByteArray → RootHash
hashLeaf = hash <<< (ByteArray.byteArrayFromIntArrayUnsafe [ 0 ] <> _)

--  | Wrapper around the internal hashing function (this uses blake2b256Hash)
hash ∷ ByteArray → RootHash
hash = RootHash <<< Hashing.blake2b256Hash

-- | `listToArray` converts a `List` to an `Array`
listToArray ∷ ∀ (a ∷ Type). List a → Array a
listToArray =
  let
    go Nil = Nothing
    go (Cons a as) = Just (a /\ as)
  in
    Unfoldable.unfoldr go

-- * Merkle tree functionality

-- | `rootHash` gets the root hash of the given merkle tree.
rootHash ∷ MerkleTree → RootHash
rootHash (Bin roothash _ _) = roothash
rootHash (Tip roothash) = roothash

-- | `unRootHash` returns the underlying `ByteArray` of the `RootHash`
unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash byteArray) = byteArray

-- |`'fromList` computes a merkle tree from the given list, and errors in the
-- | case when the given list is empty.
-- |
-- | See the `/src/TrustlessSidechain/MerkleTree.hs` for more details.
fromList ∷ List ByteArray → Either String MerkleTree
fromList Nil = Left "MerkleTree.fromList: empty list"
fromList ls =
  let
    mergeAll ∷ List MerkleTree → MerkleTree
    mergeAll (r : Nil) = r
    mergeAll rs = mergeAll $ mergePairs Nil rs

    mergePairs ∷ List MerkleTree → List MerkleTree → List MerkleTree
    mergePairs acc (a : b : cs) =
      let
        merged = mergeRootHashes (rootHash a) (rootHash b)
      in
        mergePairs (Bin merged a b : acc) cs
    mergePairs acc (a : Nil) = a : acc
    mergePairs acc Nil = acc
  in
    Right $ mergeAll $ (Tip <<< hashLeaf) <$> ls

-- | `fromArray` is `fromList` with the only difference that it accepts an
-- | `Array` instead of a `List`
fromArray ∷ Array ByteArray → Either String MerkleTree
fromArray = fromList <<< foldr (:) Nil

-- | `memberMp bt mp rh` computes `rootMp bt mp == rh` i.e., verifies that
-- | the corresponding root of `bt` and `mp` is the given root hash.
memberMp ∷ ByteArray → MerkleProof → RootHash → Boolean
memberMp bt mp rh = rootMp bt mp == rh

-- | `lookupMp bt mt` computes the merkle proof for `bt` assuming that `bt`
-- | is a leaf for the merkle tree `mt`.
lookupMp ∷ ByteArray → MerkleTree → Maybe MerkleProof
lookupMp bt =
  let
    go prf mt = case mt of
      Tip h
        | h == hsh → Just prf
        | otherwise → Nothing
      Bin _h l r →
        let
          l' = rootHash l
          r' = rootHash r
        in
          case go (Up { siblingSide: L, sibling: l' } : prf) r of
            Just res → Just res
            Nothing → go (Up { siblingSide: R, sibling: r' } : prf) l
    hsh = hashLeaf bt
  in
    ((MerkleProof <<< listToArray) <$> _) <<< go Nil

-- | `rootMp bt mp` computes the root hash of `bt` assuming that `mp` is
-- | its corresponding merkle proof.
-- |
-- | See the `/src/TrustlessSidechain/MerkleTree.hs` for more details.
rootMp ∷ ByteArray → MerkleProof → RootHash
rootMp bt mp =
  let
    go acc (Up { siblingSide, sibling }) =
      case siblingSide of
        L → mergeRootHashes sibling acc
        R → mergeRootHashes acc sibling
  in
    foldl go (hashLeaf bt) $ unwrap mp
