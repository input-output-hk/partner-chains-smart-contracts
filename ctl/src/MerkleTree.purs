module MerkleTree where

import Contract.Prelude

import Contract.Hashing (blake2b256Hash)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List as List
import Data.String (joinWith)
import Prelude (map, (<<<), (<>))

-- ! Wrapper around internal hashing function
hash ∷ ByteArray → RootHash
hash = RootHash <<< blake2b256Hash

newtype RootHash = RootHash ByteArray

derive instance Generic RootHash _
derive instance Newtype RootHash _

instance Show RootHash where
  show = genericShow

unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash s) = s

data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

instance Show MerkleTree where
  show (Bin h l r) = joinWith " " [ "Bin", show h, show l, show r ]
  show (Tip h) = joinWith " " [ "Tip", show h ]

rootHash ∷ MerkleTree → RootHash
rootHash (Bin h _ _) = h
rootHash (Tip h) = h

fromList ∷ List ByteArray → Either String MerkleTree
fromList List.Nil = Left "MerkleTree.fromList: empty list"
fromList ls =
  let
    mergeAll ∷ List MerkleTree → MerkleTree
    mergeAll (r : Nil) = r
    mergeAll rs = mergeAll $ mergePairs rs

    mergePairs ∷ List MerkleTree → List MerkleTree
    mergePairs (a : b : cs) =
      let
        merged = mergeRootHashes (rootHash a) (rootHash b)
        tail = mergePairs cs
      in
        Bin merged a b : tail
    mergePairs cs = cs

    leaves ∷ List MerkleTree
    leaves = map (Tip <<< hashLeaf) ls
  in
    Right $ mergeAll leaves

mergeRootHashes ∷ RootHash → RootHash → RootHash
mergeRootHashes l r = hashInternalNode (((<>) `on` unRootHash) l r)

hashInternalNode ∷ ByteArray → RootHash
hashInternalNode = hash <<< (byteArrayFromIntArrayUnsafe [ 1 ] <> _)

hashLeaf ∷ ByteArray → RootHash
hashLeaf = hash <<< (byteArrayFromIntArrayUnsafe [ 0 ] <> _)
