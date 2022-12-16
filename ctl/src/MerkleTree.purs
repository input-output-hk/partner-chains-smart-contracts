-- | Implements merkle tree functionality
module MerkleTree
  ( Side(L, R)
  , MerkleTree(Bin, Tip)
  , Up(Up)
  , MerkleProof(MerkleProof)
  , RootHash(RootHash)

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
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(..)
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
newtype RootHash = RootHash ByteArray

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data Side
  = L
  | R

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype Up = Up { siblingSide ∷ Side, sibling ∷ RootHash }

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype MerkleProof = MerkleProof (Array Up)

instance Show MerkleTree where
  show (Bin h l r) = String.joinWith " " [ "Bin", show h, show l, show r ]
  show (Tip h) = String.joinWith " " [ "Tip", show h ]

derive instance Generic RootHash _
derive instance Newtype RootHash _
derive instance Eq RootHash
derive instance Ord RootHash

derive instance Generic MerkleProof _
derive instance Newtype MerkleProof _
derive instance Eq MerkleProof

derive instance Generic Up _
derive instance Newtype Up _
derive instance Eq Up

instance Eq Side where
  eq L L = true
  eq R R = true
  eq _ _ = false

instance Show Side where
  show L = "L"
  show R = "R"

instance Show Up where
  show = genericShow

instance Eq MerkleTree where
  eq (Bin rh0 l0 r0) (Bin rh1 l1 r1) =
    rh0 == rh1 && l0 == l1 && r0 == r1
  eq (Tip rh0) (Tip rh1) = rh0 == rh1
  eq _ _ = false

instance Show MerkleProof where
  show = genericShow

instance Show RootHash where
  show = genericShow

-- Note [`ToData` / `FromData` Instances of the Merkle Tree]
-- All of these instances should correspond to `/src/TrustlessSidechain/MerkleTree.hs`
derive newtype instance ToData RootHash
derive newtype instance FromData RootHash

instance ToData Side where
  toData L = Integer zero
  toData R = Integer one

instance FromData Side where
  fromData plutusData = case plutusData of
    Integer n
      | n == zero → Just L
      | n == one → Just R
    _ → Nothing

instance ToData Up where
  toData (Up record) = Constr zero
    [ toData record.siblingSide, toData record.sibling ]

instance FromData Up where
  fromData plutusData = case plutusData of
    Constr n [ a, b ]
      | n == zero →
          Up <$> (({ siblingSide: _, sibling: _ }) <$> fromData a <*> fromData b)
    _ → Nothing

derive newtype instance ToData MerkleProof
derive newtype instance FromData MerkleProof

instance ToData MerkleTree where
  toData (Bin roothash l r) =
    Constr zero [ toData roothash, toData l, toData r ]
  toData (Tip roothash) =
    Constr one [ toData roothash ]

instance FromData MerkleTree where
  fromData plutusData = case plutusData of
    Constr n args
      | n == zero → case args of
          [ roothash, l, r ] → Bin <$> fromData roothash <*> fromData l <*>
            fromData r
          _ → Nothing
      | n == one → case args of
          [ roothash ] → Tip <$> fromData roothash
          _ → Nothing
    _ → Nothing

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
listToArray ∷ ∀ a. List a → Array a
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

-- | `unRootHash` is an alias for `unwrap` i.e., it coerces the newtype wrapper
-- | `RootHash`
unRootHash ∷ RootHash → ByteArray
unRootHash = unwrap

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
