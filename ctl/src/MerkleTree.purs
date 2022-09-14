module MerkleTree where

import Contract.Prelude

import Contract.Hashing (blake2b256Hash)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List as List
import Data.String (joinWith)
import Effect.Aff as Aff
import Effect.Exception (error)
import Prelude (map, (<<<), (<>))

-- ! Wrapper around internal hashing function
hash ∷ ByteArray → Aff RootHash
hash = map RootHash <<< blake2b256Hash

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

fromList ∷ List ByteArray → Aff MerkleTree
fromList List.Nil =
  Aff.throwError $ error "MerkleTree.fromList: empty list"
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
hashInternalNode = hash <<< (byteArrayFromIntArrayUnsafe [ 1 ] <> _)

hashLeaf ∷ ByteArray → Aff RootHash
hashLeaf = hash <<< (byteArrayFromIntArrayUnsafe [ 0 ] <> _)
