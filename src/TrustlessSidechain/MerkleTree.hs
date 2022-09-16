{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

-- N.B. The pragma @{-# OPTIONS_GHC -fexpose-all-unfoldings #-}@ is needed
-- because apparently some parts of the implementation of
-- 'PlutusTx.IsData.Class.FromData' (which is automatically derived via
-- TemplateHaskell magic) of some of the newtypes aren't getting inlined
-- properly. This is the sledgehammer to guarantee that these are inlined --
-- albeit, this is apparently an experimental flag, so stability might not be
-- guaranteed...
--
-- Some alternatives would be to
--
--  1. Use the pragma @{-# OPTIONS_GHC -fno-specialise #-}@ in the
--  module which imports these types, and in this module as well.
--
--  2. Get rid of the newtype wrapper completely and just replace it with a
--  type alias -- but the STANDARDS.md says we shouldn't do this :)

{- | This module is an implementation of a merkle tree suitable for on chain
 and off chain code. This is meant to be imported qualified i.e.,

 > import TrustlessSidechain.MerkleTree qualified as MT

 It is based off of [hydra-poc](https://github.com/input-output-hk/hydra-poc)
 with some improvements.

 Ultimately, the decision to write our own merkle tree came down to:

    1.  We'd like to optimize things later, so it's better if we write our code
    ourselves (so we can actually optimize!)

    2.  There were some low hanging fruit optimizations that we could perform
    already

    3.  We can document ours nicely :)
-}
module TrustlessSidechain.MerkleTree (
  -- * Types
  RootHash (RootHash, unRootHash),
  MerkleProof (MerkleProof, unMerkleProof),
  MerkleTree,
  emptyMp,

  -- * Building the Merkle Tree
  fromList,
  fromNonEmpty,

  -- * Creating and querying Merkle proofs / the root hash
  lookupMp,
  memberMp,
  rootMp,
  rootHash,

  -- * Internal
  height,
  mergeRootHashes,
  hash,
  hashLeaf,
  hashInternalNode,
) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import PlutusPrelude (NonEmpty, (<|>))
import PlutusPrelude qualified
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import Schema qualified
import Prelude qualified

{- | 'RootHash' is the hash that is the root of a 'MerkleTree'.

 Internally, this is just a newtype wrapper around 'BuiltinByteString' for
 some type level book keeping to remember what we have hashed and what we
 haven't; and also represents hashes of subtrees of a merkle tree.
-}
newtype RootHash = RootHash {unRootHash :: BuiltinByteString}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving anyclass (Schema.ToSchema)

makeIsDataIndexed ''RootHash [('RootHash, 0)]
deriveJSON defaultOptions ''RootHash

instance Eq RootHash where
  RootHash l == RootHash r = l == r

{- | Internal data type. 'Side' is used in 'Up' to decide whether a *sibling* of a node is on the
 left or right side.

 In a picture,
 >     ...
 >    parent
 >     /  \
 >   you   sibling
 > ...      ...

 >     ...
 >    parent
 >     /  \
 > sibling you
 > ...      ...

 are siblings.
-}
data Side = L | R
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving anyclass (Schema.ToSchema)

makeIsDataIndexed ''Side [('L, 0), ('R, 1)]
deriveJSON defaultOptions ''Side

{- | Internal data type. 'Up' is a single step up from a leaf of a 'MerkleTree' to recompute the
 root hash. In particular, this data type recovers information of the
 sibling.

 E.g. given the merkle tree,
 >       1234
 >      /    \
 >   12       34
 >  /  \      / \
 >  1   2    3   4
 we will represent the path from @2@ to the root @1234@ by the list
  @[Up L 1, Up R 34]@ -- see 'lookupMp' for more details.
-}
data Up = Up {siblingSide :: Side, sibling :: RootHash}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving anyclass (Schema.ToSchema)

makeIsDataIndexed ''Up [('Up, 0)]
deriveJSON defaultOptions ''Up

{- | 'MerkleProof' is the proof to decide whether a 'BuiltinByteString' was
 included in a 'RootHash'.

 See 'memberMp' for details.
-}
newtype MerkleProof = MerkleProof {unMerkleProof :: [Up]}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving anyclass (Schema.ToSchema)

makeIsDataIndexed ''MerkleProof [('MerkleProof, 0)]
deriveJSON defaultOptions ''MerkleProof

-- | 'emptyMp' is an empty 'MerkleProof'.
emptyMp :: MerkleProof
emptyMp = MerkleProof {unMerkleProof = []}

-- | 'hash' is an internal function which is a wrapper around the desired hashing function.
{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> RootHash
hash = RootHash . Builtins.blake2b_256

{- | 'hashLeaf' is an internal function used to hash a leaf for the merkle
 tree. See: Note [2nd Preimage Attack on The Merkle Tree]
-}
{-# INLINEABLE hashLeaf #-}
hashLeaf :: BuiltinByteString -> RootHash
hashLeaf = hash . Builtins.consByteString 0

{- | 'hashInternalNode' is an internal function used to hash an internal node
 in the Merkle tree. See: Note [2nd Preimage Attack on The Merkle Tree]
-}
{-# INLINEABLE hashInternalNode #-}
hashInternalNode :: BuiltinByteString -> RootHash
hashInternalNode = hash . Builtins.consByteString 1

-- | 'mergeRootHashes' is an internal function which combines two 'BuiltinByteString' in the 'MerkleTree'
{-# INLINEABLE mergeRootHashes #-}
mergeRootHashes :: RootHash -> RootHash -> RootHash
mergeRootHashes l r = hashInternalNode $ (Builtins.appendByteString `PlutusPrelude.on` unRootHash) l r

{- | 'MerkleTree' is a tree of hashes. See 'fromList' and 'fromNonEmpty' for
 building a 'MerkleTree', and see 'lookupMp' and 'memberMp' for creating and
 verifying 'MerkleProof'.
-}
data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash
  deriving (Prelude.Show, Prelude.Eq)

makeIsDataIndexed ''MerkleTree [('Bin, 0), ('Tip, 1)]

-- Note [Merkle Tree Invariants]:
--      1. @Bin h l r@ satisfies @h = rootHash l `mergeRootHashes` rootHash r@

-- | 'height' is used for QuickCheck to verify invariants.
{-# INLINEABLE height #-}
height :: MerkleTree -> Integer
height = \case
  Bin _ l r -> 1 + max (height l) (height r)
  Tip _ -> 1

-- | /O(1)/ 'rootHash' returns the topmost hash.
{-# INLINEABLE rootHash #-}
rootHash :: MerkleTree -> RootHash
rootHash = \case
  Bin h _ _ -> h
  Tip h -> h

{- | /O(n log n)/. Throws an error when the list is empty, but otherwise executes
 'fromNonEmpty'.

 > 'fromList' [] == error
 > fromList ["a", "b"] == fromNonEmpty ["a", "b"]
-}
{-# INLINEABLE fromList #-}
fromList :: [BuiltinByteString] -> MerkleTree
fromList [] = traceError "illegal TrustlessSidechain.MerkleTree.fromList with empty list"
fromList lst = mergeAll . map (Tip . hashLeaf) $ lst
  where
    mergeAll :: [MerkleTree] -> MerkleTree
    mergeAll [r] = r
    mergeAll rs = mergeAll $ mergePairs rs

    mergePairs :: [MerkleTree] -> [MerkleTree]
    mergePairs (a : b : cs) =
      let a' = rootHash a
          b' = rootHash b
       in Bin (mergeRootHashes a' b') a b : mergePairs cs
    mergePairs cs = cs

{- | /O(n log n)/. Builds a 'MerkleTree' from a 'NonEmpty' list of
 'BuiltinByteString'.

 An example of using 'fromNonEmpty':

 > {\-# LANGUAGE OverloadedStrings #-\}
 > import TrustlessSidechain.MerkleTree qualified as MT
 > import PlutusPrelude qualified
 > pubkey1, pubkey2 :: BuiltinByteString
 > pubkey1 = "pubkey1"
 > pubkey2 = "pubkey2"
 > merkleTree = MT.fromNonEmpty $ PlutusPrelude.fromList [ pubkey1, pubkey2 ]

 Pictorially, given @["p1", "p2", "p3"]@, this creates a tree like
 >              hash (hash (hash "p1" ++ hash "p2") ++ hash "p3")
 >              /                               |
 >    hash (hash "p1" ++ hash "p2")             |
 >      /                    \                  |
 > hash "p1"                hash "p2"       hash "p3"

 N.B. it doesn't exactly do this anymore since this permits second preimage
 attacks: see Note [2nd Preimage Attack on The Merkle Tree].
-}
{-# INLINEABLE fromNonEmpty #-}
fromNonEmpty :: NonEmpty BuiltinByteString -> MerkleTree
fromNonEmpty = fromList . PlutusPrelude.toList

{-
Properties [Merkle Tree Height Bound]
    Let lst be an arbitrary non empty list.
        height (fromNonEmpty lst) <= floor(log_2 (length lst)) + 2

See: Note [Hydra-Poc People Merkle Tree Comparisons]
-}

{- | /O(log n)/. Lookup the the corresponding merkle proof of the
 'BuiltinByteString' in the 'MerkleTree' i.e., this builds the merkle proof
 for the given 'BuiltinByteString' corresponding to this 'MerkleTree' if such
 a merkle proof exists.

 The function will return the corresponding proof as @('Just' value)@, or
 'Nothing' if the Merkle tree does not contain the hash of the
 'BuiltinByteString'

 An example of using 'lookupMp':

 > {\-# LANGUAGE OverloadedStrings #-\}
 > import Data.Maybe (isJust)
 > import TrustlessSidechain.MerkleTree qualified as MT
 >
 > dogs = MT.fromList [ "maltese", "pomeranian", "yorkie" ]
 >
 > main = do
 >     print $ isJust $ lookupMp "maltese" dogs
 >     print $ isJust $ lookupMp "golden retriever" dogs

 The output of this program:

 >   True
 >   False
-}
{-# INLINEABLE lookupMp #-}
lookupMp :: BuiltinByteString -> MerkleTree -> Maybe MerkleProof
lookupMp bt mt = fmap MerkleProof $ go [] mt
  where
    hsh :: RootHash
    hsh = hashLeaf bt

    go :: [Up] -> MerkleTree -> Maybe [Up]
    go prf = \case
      -- Invariant:
      --
      --  1. @Left l@ means @l@ is on the *left* side so @hsh@ was on the
      --  *right* side
      --
      --  2. @Right r@ means @r@ is on the *right* side so @hsh@ was on the
      --  *left* side
      --
      -- Note that we implicitly "reverse" the list so that the deepest node
      -- in the 'MerkleTree' is at the head of the list.
      Bin _h l r ->
        let l' = rootHash l
            r' = rootHash r
         in go (Up L l' : prf) r <|> go (Up R r' : prf) l
      Tip h
        | hsh == h -> Just prf
        | otherwise -> Nothing

{- Properties.
    1. Suppose lst is an arbitrary non empty list.
            x \in lst <==> isJust (lookupMp x (fromNonEmpty lst))

    2.
        Just prf = lookupMp x (fromNonEmpty lst) ==> length prf <= floor (log_2 (length lst)) + 1

    N.B. 2. follows from [Merkle Tree Height Bound].
-}

{- | /O(n)/ in the length of the 'MerkleProof' (which is /O(log n)/ of
 the size of the original 'MerkleTree' of the given 'RootHash').

 An example of using 'memberMp':

 > let merkleTree = fromList ["maltese", "pomeranian", "yorkie"]
 > let Just prf = lookupMp "maltese" merkleTree
 > memberMp "maltese" prf (rootHash merkleTree) == True
-}
{-# INLINEABLE memberMp #-}
memberMp :: BuiltinByteString -> MerkleProof -> RootHash -> Bool
memberMp bt prf rth = rth == rootMp bt prf

{- | @'rootMp' bt prf@ computes the root of a merkle tree with @bt@ as the
 missing element, and @prf@ is the proof of @str@ being in the merkle tree.
-}
rootMp :: BuiltinByteString -> MerkleProof -> RootHash
rootMp bt prf = go (hashLeaf bt) (unMerkleProof prf)
  where
    -- This just undoes the process given in 'lookupMp'.
    go :: RootHash -> [Up] -> RootHash
    go acc [] = acc
    go acc (p : ps) = case siblingSide p of
      L -> go (mergeRootHashes (sibling p) acc) ps
      R -> go (mergeRootHashes acc (sibling p)) ps

{- Properties.
    Suppose lst is an arbitrary non empty list.
    Let tree = fromNonEmpty lst
        Just prf = lookupMp x tree <==> memberMp x prf (rootHash tree) = True
-}

{-
 Note [2nd Preimage Attack on The Merkle Tree]

 A previous implementation was susceptible to a 2nd Preimage Attack. Let's
 recall how the Merkle root was constructed previously. Given a nonempty list of /leaves/,
 we computed the Merkle root by

    1. Hashing all the leaves

    2. Linearly scan through the leaves and replace adjacent leaves with the
    hash of the two leaves appended together

    3. Repeat 2. until we are left with one hash.

 For example, given the non empty list @[a,b,c,d]@, we would compute the Merkle
 root as follows (where @h@ denotes the hash function and @++@ denotes append):

 >     h(h(h(a)++h(b))++h(h(c)++h(d)))
 >         /                   \
 >  h(h(a)++h(b))        h(h(c)++h(d))
 >   /        \           /          \
 > h(a)      h(b)       h(c)        h(d)

 Now, it's easy to see that an adversary can give a distinct nonempty list which would
 form the same Merkle root (which in our example is
 @h(h(h(a)++h(b))++h(h(c)++h(d)))@).
 Consider the non empty list @[h(a)++h(b), h(c)++h(d)]@, and we observe that the Merkle root is computed like

 > h(h(h(a)++h(b))++h(h(c)++h(d)))
 >     /                   \
 > h(h(a)++h(b))        h(h(c)++h(d))

 which has the same root as our original Merkle tree.

 From this result, it is easy to see how one can generate data and proofs for a
 Merkle tree which weren't originally in the Merkle tree.

 To fix this, it's quite simple. We change the procedure to

    1. Prepend a 0 byte to all the leaves, then hash all the leaves individually

    2. Linearly scan through the leaves and replace adjacent leaves with the
    hash of the two leaves appended together prepended with a 1 byte i.e., two
    adjacent leaves @leaf0@ and @leaf1@ is replaced by the single leaf @hash(1
    ++ leaf0 ++ leaf1)@

    3. Repeat 2. until we are left with one hash.

 As an example, given the nonempty list @[a,b,c,d]@, we would compute the Merkle root as

 >       h(1++h(1++h(0++a)++h(0++b))++h(1++h(0++c)++h(0++d)))
 >             /                                    \
 >  h(1++h(0++a)++h(0++b))                  h(1++h(0++c)++h(0++d))
 >   /        \                                 /          \
 > h(0++a)      h(0++b)                     h(0++c)        h(0++d)

 Why does this fix this? If we assume that the underlying hash function @h@ is
 collision resistant, this essentially implies that the @h@ is injective. This
 means that an adverary MUST choose the preimage of one the horizontal "levels"
 as the nonempty list without the @0@ or @1@ prepended. So, if the adversary
 chose any of the levels, the algorithm would prepend a @0@ to it, which would
 mean the root would be different if the adversary picked anything but the
 original leafs i.e., this is collision resistant.
-}

{-
Note [Hydra-Poc People Merkle Tree Comparisons]

We discuss some comparisions between us and the hydra-poc people.

We first note that the hydra-poc people's implementation is susceptible to a
2nd preimage attack whereas ours isn't -- see: Note [2nd Preimage Attack on The
Merkle Tree]

The second main difference how the merkle tree is actually constructed, and we
spend the remaining of this note discussing this aspect. They do a top down
approach (compute the length of the list, divide by 2, then split the list into
the first half / second half, then keep going), where as we do a "bottom up"
approach.

N.B. This technique of going "bottom up" to create the tree is well known
when implementing merge sort
    [1] See 'Data.List.sort'
    [2] "When You Should Use Lists In Haskell (Mostly You Should Not)" by Johannes Waldmann
    [3] "Algorithm Design in Haskell" by Richard Bird / Jeremy Gibbons

The hydra-poc people claim their tree has an exact height upper bound as given
in [Merkle Tree Height Bound]. It's a bit of a peculier choice of constants
[this is implied in their QuickCheck tests], but if we want our implementation
to be at least as good as their solution, we should show that our trees are
bound by the same bound.

We sketch out why this is the case.

Observation 1.
    The height of a merkle tree is upper bounded by the recurrence

    T(n) = 1                    if n = 1
           T(ceil(n/2)) + 1     otherwise

    since this counts the number of times the mergePairs function gets called
    in 'fromList' where we may observe that the height of the tree increases by
    iff mergePairs gets called.

Thus, it suffices to show

    T(n) <= floor(log_2(n)) + 2

for every n >= 1.

Claim 1.
    for every k >= 0
    T(2^k) = log_2(n) + 1

Proof.
    By induction on k.

Claim 2.

    T(n) <= floor(log_2(n)) + 2

for every n >= 1.

Proof. By Claim 1. we only need to consider the case when n is not a power of
2. In which case we may write
    2^k < n
where k is the largest integer satisfying this inequality.
It follows that
    2^k / 2 < ceil(n/2) <= 2^k < n          (*)
and observing that the recurrence T is obviously increasing, we get that
    T(n) = T(ceil(n/2)) + 1
         <= T(2^k) + 1          [T is increasing]
         = (log_2 2^k + 1) + 1  [Claim 1]
         = log_2 2^k + 2
         <= floor(log_2 n) + 2          [Apply (*)]
as required.

Practically, here are some benchmarks from the hydra-poc. The hydra-poc people
have benchmarks like:

| Size | % member max mem | % member max cpu | % builder max mem | % builder max cpu |
| :--- | ---------------: | ---------------: | ----------------: | ----------------: |
| 1    | 2.44             | 1.55             | 2.66              | 1.64              |
| 2    | 2.58             | 1.66             | 3.31              | 2.09              |
| 5    | 2.77             | 1.82             | 8.22              | 5.53              |
| 10   | 2.91             | 1.93             | 25.49             | 17.62             |
| 20   | 3.04             | 2.04             | 94.54             | 65.96             |
| 50   | 3.22             | 2.19             | 0.00              | 0.00              |
| 100  | 3.36             | 2.30             | 0.00              | 0.00              |
| 500  | 3.66             | 2.56             | 0.00              | 0.00              |
Benchmark on-chain-cost: FINISH

And our method has running time like:
| Size | % member max mem | % member max cpu | % builder max mem | % builder max cpu |
| :--- | ---------------: | ---------------: | ----------------: | ----------------: |
| 1    | 2.44             | 1.55             | 2.57              | 1.60              |
| 2    | 2.58             | 1.66             | 2.76              | 1.74              |
| 5    | 2.79             | 1.84             | 3.31              | 2.16              |
| 10   | 2.93             | 1.95             | 4.08              | 2.80              |
| 20   | 3.06             | 2.06             | 5.58              | 4.06              |
| 50   | 3.24             | 2.20             | 9.98              | 7.81              |
| 100  | 3.37             | 2.32             | 17.25             | 14.04             |
| 500  | 3.66             | 2.55             | 75.18             | 63.70             |
Benchmark on-chain-cost: FINISH

where 'member' is 'memberMp' here and 'builder' is 'fromList'.

We see that the bottom up implementation of 'fromList' is almost 10x more
efficient on some cases, and I'm fairly certain the last 3 cases of the
hydra-poc guys are botched and didn't actually run whereas our implementation
actually did run.

I'll note that the hydra-poc people have a slightly more efficient 'memberMp'
I suspect this is just "lucky" with the test cases because in some cases our
member function is faster as well. This follows from [Merkle Tree Height Bound].
-}
