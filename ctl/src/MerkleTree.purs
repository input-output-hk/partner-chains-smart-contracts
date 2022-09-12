module MerkleTree where

import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.List (List(..), (:))
import Prelude (map, (<<<), (<>))

-- ! Wrapper around internal hashing function
hash ∷ ByteArray → RootHash
hash = RootHash -- <<< blake2b_256 TODO find appropriate purescript hashing function

newtype RootHash = RootHash ByteArray

unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash s) = s

data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

rootHash ∷ MerkleTree → RootHash
rootHash (Bin h _ _) = h
rootHash (Tip h) = h

fromList ∷ List ByteArray → MerkleTree
--fromList [] = error "MerkleTree.fromList: empty list"
fromList ls =
  let
    mergeAll ∷ List MerkleTree → MerkleTree
    mergeAll (r : Nil) = r
    mergeAll rs = mergeAll (mergePairs rs)

    mergePairs ∷ List MerkleTree → List MerkleTree
    mergePairs (a : b : cs) = Bin (mergeRootHashes (rootHash a) (rootHash b)) a b
      : mergePairs cs
    mergePairs cs = cs
  in
    mergeAll (map (Tip <<< hashLeaf) ls)

mergeRootHashes ∷ RootHash → RootHash → RootHash
mergeRootHashes l r = hashInternalNode (((<>) `on` unRootHash) l r)

hashInternalNode ∷ ByteArray → RootHash
hashInternalNode = hash <<< (_ <> byteArrayFromIntArrayUnsafe [ 1 ])

hashLeaf ∷ ByteArray → RootHash
hashLeaf = hash <<< (_ <> byteArrayFromIntArrayUnsafe [ 0 ])
