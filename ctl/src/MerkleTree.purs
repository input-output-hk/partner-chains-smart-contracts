module MerkleTree where

import Contract.Prelude

import Contract.Hashing (blake2b256Hash)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.List (List(..), (:))
import Prelude (map, (<<<), (<>))

-- ! Wrapper around internal hashing function
hash ∷ ByteArray → Aff RootHash
hash = map RootHash <<< blake2b256Hash

newtype RootHash = RootHash ByteArray

unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash s) = s

data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

rootHash ∷ MerkleTree → RootHash
rootHash (Bin h _ _) = h
rootHash (Tip h) = h

fromList ∷ List ByteArray → Aff MerkleTree
--fromList [] = error "MerkleTree.fromList: empty list"
fromList ls =
  let
    mergeAll ∷ List MerkleTree → Aff MerkleTree
    mergeAll (r : Nil) = pure r
    mergeAll rs = mergePairs rs >>= mergeAll

    mergePairs ∷ List MerkleTree → Aff (List MerkleTree)
    mergePairs (a : b : cs) = do
      merged ← mergeRootHashes (rootHash a) (rootHash b)
      tail ← mergePairs cs
      pure $ Bin merged a b : tail
    mergePairs cs = pure cs

    leaves ∷ Aff (List MerkleTree)
    leaves = traverse (map Tip <$> hashLeaf) ls
  in
    mergeAll =<< leaves

mergeRootHashes ∷ RootHash → RootHash → Aff RootHash
mergeRootHashes l r = hashInternalNode (((<>) `on` unRootHash) l r)

hashInternalNode ∷ ByteArray → Aff RootHash
hashInternalNode = hash <<< (_ <> byteArrayFromIntArrayUnsafe [ 1 ])

hashLeaf ∷ ByteArray → Aff RootHash
hashLeaf = hash <<< (_ <> byteArrayFromIntArrayUnsafe [ 0 ])
