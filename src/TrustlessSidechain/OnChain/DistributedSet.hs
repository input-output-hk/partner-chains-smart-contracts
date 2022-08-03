{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Implementation of a distributed set an on-chain distributed set to admit
 proof not in a set membership
-}
module TrustlessSidechain.OnChain.DistributedSet where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Ledger.Address (Address)
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts (MintingPolicy, TypedValidator, Validator, ValidatorTypes)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef,
 )
import Plutus.V1.Ledger.Scripts (
  Datum (getDatum),
 )
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName (unTokenName),
  Value (getValue),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusPrelude qualified
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import TrustlessSidechain.OnChain.BitField (BitField (unBitField))
import TrustlessSidechain.OnChain.BitField qualified as BitField
import TrustlessSidechain.OnChain.Types (DsRedeemer (dsStr))
import Prelude qualified

{- | 'hash' is an internal function to store the hash of the to put in the set.
 This is just a thin wrapper around the 'Builtins.blake2b_256' hash function

 TODO: We need to actually hash things in the script still.
-}
{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> BuiltinByteString
hash = Builtins.blake2b_256

-- * Data types

{- | 'Ds' (abbr. Distributed Set) is the type which parameterizes the validator
 for the distributed set. It contains the 'CurrencySymbol' which is used to
 hold 'TokenName's that uniquely identify nodes (See Note [Data Structure
 Definition])
-}
newtype Ds = Ds
  { -- | 'dsSymbol' is the 'CurrencySymbol' for the minting policy that
    -- creates tokens which identifies a node.
    dsSymbol :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

{- | 'DSDatum' is the datum in the distributed set. See: Note [Data Structure
 - Definition] and Note [Node Representation].
-}
newtype DsDatum = DsDatum
  { dsEdge :: Edge
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq DsDatum where
  {-# INLINEABLE (==) #-}
  a == b = dsEdge a == dsEdge b

{- | 'Edge' represents an edge in the graph. See: Note [Data Structure
 - Definition] and Note [Node Representation].
-}
data Edge
  = -- | the BuiltinByteString is the suffix of the string in the set.
    Tip BuiltinByteString
  | -- | the BuiltinByteString is the shared part of the edge, and the BitField
    -- (which must be non empty) is the first distinguishing bit to be an edge
    -- to another node.
    Bin BuiltinByteString BitField
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq Edge where
  Tip suf0 == Tip suf1 = suf0 == suf1
  Bin bs0 bf0 == Bin bs1 bf1 = bs0 == bs1 && bf0 == bf1
  _ == _ = False

{- | 'Node' is an internal data type of the tree node used in the validator.
 See: Note [Data Structure Definition] and Note [Node Representation].
-}
data Node = Node
  { nPrefix :: BuiltinByteString
  , nEdge :: Edge
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b =
    nPrefix a == nPrefix b && nEdge a == nEdge b

{- | 'Ib' (abbr. "insert buffer") is the output of 'insertNode' which indicates
 the number of new nodes. In particular, 'insertNode' may either create 2 or
 3 new nodes.
-}
data Ib a
  = Two a a
  | Three a a a
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq a => Eq (Ib a) where
  Two a1 a2 == Two a1' a2' = a1 == a1' && a2 == a2'
  Three a1 a2 a3 == Three a1' a2' a3' = a1 == a1' && a2 == a2' && a3 == a3'
  _ == _ = False

instance Functor Ib where
  fmap f = \case
    Two a1 a2 -> Two (f a1) (f a2)
    Three a1 a2 a3 -> Three (f a1) (f a2) (f a3)

{- | 'DsMint' is the parameter for the minting policy.
 See Note [Node Representation] for more details.
-}
newtype DsMint = DsMint
  { dsmTxOutRef :: TxOutRef
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

makeIsDataIndexed ''Ds [('Ds, 0)]
deriveJSON defaultOptions ''Ds
PlutusTx.makeLift ''Ds

makeIsDataIndexed ''DsDatum [('DsDatum, 0)]
deriveJSON defaultOptions ''DsDatum

makeIsDataIndexed ''Edge [('Tip, 0), ('Bin, 1)]
deriveJSON defaultOptions ''Edge

makeIsDataIndexed ''Ib [('Two, 0), ('Three, 1)]
deriveJSON defaultOptions ''Ib

makeIsDataIndexed ''Node [('Node, 0)]
deriveJSON defaultOptions ''Node

makeIsDataIndexed ''DsMint [('DsMint, 0)]
deriveJSON defaultOptions ''DsMint
PlutusTx.makeLift ''DsMint

{-
 Note [Data Structure Definition]

 In the trustless-sidechain, the distributed set should provide the following functionality.

    * Insertion of a message digest (i.e., an element of the image of a hash
    function which all have a fixed length) which is not already in the set.

 To do this, we define a /distributed set/ as a tree (so the graph must be
 connected) satisfying the following properties:

    * Nodes with the dautm given in 'Node'. We impose the extra requirement
    that each node is unique w.r.t the field 'nPrefix'.

    * Edges: given nodes @a@ and @b@, there exists an edge between @a@ and @b@ iff

    > nPrefix a ++ br ++ [d] == nPrefix b

    for @Bin br ds = nEdge a@ and some @d@ in @ds@ and @ds@ must be non empty.

    * A distinguished node (the root) with 'nPrefix' as the empty string @""@.

 For references, see

    [1] Algorithm Design in C Part 1 (3rd edition) by Robert Sedgewick for the
    Patricia Tree data structure

 which this is mostly based off of.

 One may wonder -- why this representation of nodes over other representations?
 See the following notes for some comparisons.

    * Note [Comparison to Stick Breaking Set]

    * Note [Comparison to Linked List]

    * Note [Maximum Transaction Size]

 Now, we discuss the definitions of the distributed set more carefully.

 Defn 1.
    Given a distributed set, we say that a string @str@ is in the distributed set iff
    there exists some node @a@ for which
    > nPrefix a ++ suf == str
    for @Tip suf = nEdge a@.

 Claim 1.
    If @str@ is not in a distributed set, then there exists a unique node @a@ of the
    satisfying

        (1) @nPrefix a `isPrefixOf` str@; and

        (2) either one of the following conditions is satisfied

            (a) @Tip suf = nEdge a@ but @nPrefix a ++ suf /= str@; or

            (b) @Edge br ds = nEdge a@ and @nPrefix a ++ br ++ [d]@ is not a
            prefix of @str@ for every @d@ in @ds@.
 Proof.
    By construction of the graph, we can find such a node @a@ by following the
    edges (which are prefixes) from the root of the distributed set. This
    process must terminate (as the graph must be finite). Then, to show that
    (a) or (b) are satisfied, assume that (a) is not true, so we know that
    @nEdge a@ must be an 'Edge'. Since the set stores strings (that are message
    digests which all have the same fixed length), it necessarily follows that
    (as in the notation of (b)) that the length of @nPrefix a ++ br ++ [d]@
    must be less than or equal to the length of @str@. But, none such strings
    @nPrefix a ++ br ++ [d]@ can be a prefix of @str@ since we'd otherwise
    contradict the construction of @a@.

    Also, uniqueness is immediate (since otherwise, the prefix wouldn't match
    or we could go further down the tree).

 Claim 2.
   If a node @a@ is of the form @Tip suf = nEdge a@, then every string
   @str /= nPrefix a ++ suf@ with @nPrefix a `isPrefixOf` str@ is not in the
   distributed set.

 Claim 3.
   If a node @a@ is of the form @Edge br ds = nEdge a@, then every string @str@
   with @nPrefix a ++ br ++ [d] `isPrefixOf` str@ for every @d@ in @ds@ is not
   in the distributed set.

 Proof.
   Claim 2. and 3. are immediate.

 Property 1.
    An insertion will yield 2 or 3 new nodes.

 Proof.
    See 'insertNode' for the case analysis.

 Property 2.
    The height of the tree (length of the longest path from root to leaf) is at
    most the length of the message digest.

 Proof.
    Immediate by construction.

 Property 1. and Property 2. entail reasonably efficient onchain validation and
 a way to efficiently query utxos offchain resp.
 -}

{-
 Note [Node Representation]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~
 Here, we answer how we actually represent the a 'Node' @a@ in the block chain.
 At a UTXO, we store

    * @'nPrefix' a@ as a 'TokenName'.

    * @'nEdge' a@ in the datum.

 to represent the node @a@ on the block chain.

 We do this for a few reasons

    * Having @nPrefix a@ on the block chain as a currency symbol would allow
    offchain code to (assuming there is an efficient mapping from 'AssetClass's
    to UTxOs) query how to prove their transaction is not in the distributed
    set.

    * Since we are storing message digests, we know that the output will be 32
    bytes which coincidentally is exactly how large a 'TokenName' may be.

 Now, we need to be a bit careful about minting writing the validators /
 minting policies for this. We describe this (and the security!) in more detail
 here.

 When we initialize the distributed set we must create minting policy which
 (takes as a parameter an unspent transaction, say @inittxout@) that

    * Mints exactly one token with 'TokenName' @""@ which is paid to some
    script (N.B. at this moment, we can't verify that this token is paid to the
    validator script for the distributed set because of a chicken / egg sorta
    problem) that corresponds to 'rootNode' in the distributed set (we
    initialize the set to be non empty with some aribtrary null bytes -- see
    Note [Why Can we Assume the Distributed Set is NonEmpty?]); AND we consume
    the @inittxout@ transaction.

    * (this is used in the general case of inserting things in the distributed
    set). Mints exactly 1 or 2 tokens (of a potentially arbitrary 'TokenName's)
    if and only if (a) we are consuming exactly one transaction which already
    has a 'CurrencySymbol' of this script; and (b) we spend these tokens to
    distinct continuing outputs of the utxo we are spending.

  At this point, other participants may use the above 'CurrencySymbol' with
  'TokenName' @""@ to identify the 'rootNode' of the distributed set. If an
  adversary did something evil and attempted to mint the token to a validator
  which isn't the distributed set, other participants would notice pretty quick
  that something is wrong (since our distributed set validators are
  paramerterized with the 'CurrencySymbol') and hence wouldn't paid to the
  distributed set's validator.

  As for the validator, the validator needs to verify that: when given a
  redeemer (of the string to insert), we create 2-3 new utxos of nodes which
  correspond to the insertion. Note that the second case of the minting policy
  is used to create these new nodes.
-}

{- Note [Why Can We Assume the Distributed Set is NonEmpty?]

 You may notice that this implementation appears to not support "being empty"
 -- which indeed this representation only allows the set to be nonempty.
 This doesn't matter actually since we can initialize the set to start with any
 arbitrary bytes already inserted (we arbitrarily choose some null bytes) and
 since we are storing hashes, by preimage resistance, it will be difficult for
 someone to find something in the domain that hashes to the same arbitrary
 bytes we chose.
-}

{-
Note [Maximum Transaction Size Estimate]

Having read Note [Node Representation], we see that nodes correspond to UTXOs,
and seeing that 'insertNode' must consume and create new UTXOs to prove
something is not in the set, we would like to have an upper bound for how large
these transactions can be since there is an upper limit of the accepted
transaction size.

Each utxo is about
> utxoSize := (10 * 32) bytes
since the 'ScriptContext' must include:

 - 'Address' (can be around 2 * 32 bytes)

 - 'CurrencySymbol' which is 32 bytes

 - 'TokenName' which is 32 bytes

 - 'DatumHash' (includes this twice: once in the 'TxOut' and the other to
 associate the datum with the hash) which is 2*32 bytes

 - 'DsDatum' which is 32 byte bit field and at most a 32 byte extra branch

and we'll add (+ (2*32)) just for good measure when counting overhead for
constructors..

So in the worst case, owing to the fact that insertions (see 'insertNode')
generate at most 3 new nodes (+1 for consuming the input transaction), this
means that inserting a string creates a transaction of at most size

> 4 * utxoSize = 1280 bytes

This is well well under the 16384B transaction size limit..
-}

-- * Standard helper functions

-- | @'isPrefixOfByteString' str0 str1@ returns true iff @str0@ is a prefix of @str1@
{-# INLINEABLE isPrefixOfByteString #-}
isPrefixOfByteString :: BuiltinByteString -> BuiltinByteString -> Bool
isPrefixOfByteString str0 str1 = str0Len <= str1Len && str0 == takeByteString str0Len str1
  where
    str0Len, str1Len :: Integer
    str0Len = lengthOfByteString str0
    str1Len = lengthOfByteString str1

-- | 'snocByteString' appends the character to the end of the string.
{-# INLINEABLE snocByteString #-}
snocByteString :: BuiltinByteString -> Integer -> BuiltinByteString
snocByteString str b = appendByteString str (Builtins.consByteString b emptyByteString)

-- | 'lcp' computes the longest common prefix of two strings.
lcp :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString
lcp s t = takeByteString (go 0) s
  where
    go ix
      | ix >= lengthOfByteString s || ix >= lengthOfByteString t = ix
      | otherwise =
        if s `indexByteString` ix == t `indexByteString` ix
          then go (ix + 1)
          else ix

-- * Helper functions related to 'Ib'

-- | 'toListIb' converts an 'Ib' to a list of 2 or 3 elements.
{-# INLINEABLE toListIb #-}
toListIb :: Ib a -> [a]
toListIb = \case
  Two a b -> [a, b]
  Three a b c -> [a, b, c]

{- | 'fromListIb' converts a list into an 'Ib'. N.B. this function is partial
 and throws an error in the case that the list does not have 2 or 3 elements.
-}
{-# INLINEABLE fromListIb #-}
fromListIb :: [a] -> Ib a
fromListIb = \case
  [a, b, c] -> Three a b c
  [a, b] -> Two a b
  _ -> traceError "error 'fromListIb' invalid list"

{- | 'normalizeIb' sorts the 'Ib' with the minimal number of comparisons of
 either 1 or 3 comparisons.
-}
{-# INLINEABLE normalizeIb #-}
normalizeIb :: Ord a => Ib a -> Ib a
normalizeIb = normalizeIbOn id

{- | 'normalizeIbOn' sorts the 'Ib' with the minimal number of comparisons:
 either 1 or 3 comparisons.
-}
{-# INLINEABLE normalizeIbOn #-}
normalizeIbOn :: Ord b => (a -> b) -> Ib a -> Ib a
normalizeIbOn prj = \case
  Two n1 n2
    | n1 <. n2 -> Two n1 n2
    | otherwise -> Two n2 n1
  Three n1 n2 n3
    | n1 <. n2 ->
      if n2 <. n3
        then Three n1 n2 n3
        else
          if n3 <. n1
            then Three n3 n1 n2
            else Three n1 n3 n2
    | otherwise ->
      if n3 <. n2
        then Three n3 n2 n1
        else
          if n1 <. n3
            then Three n2 n1 n3
            else Three n2 n3 n1
  where
    a <. b = prj a < prj b

-- * Node related functions

-- | 'mkNode' is a wrapper to create a Node from a prefix and the datum.
{-# INLINEABLE mkNode #-}
mkNode :: BuiltinByteString -> DsDatum -> Node
mkNode str d =
  Node
    { nPrefix = str
    , nEdge = dsEdge d
    }

{- | 'rootNode' is the root node of every distributed set. In particular, a
 distributed set must have @""@ as the prefix, and starts with a string of
 null bytes inserted already. See Note [Node Representation] and Note [Data
 Structure Definition] for more details.
-}
{-# INLINEABLE rootNode #-}
rootNode :: Node
rootNode =
  Node
    { nPrefix = Builtins.emptyByteString
    , nEdge = Tip $ unBitField BitField.zeroes256
    }

{- | @'nextNodePrefix' str node@ finds the prefix of the next node (provide it
 exists) of @str@ after following the edges of @node@.

 Preconditions (you must check these yourself)

      - @prefix node `isPrefixOfByteString` str@
-}
nextNodePrefix :: BuiltinByteString -> Node -> Maybe BuiltinByteString
nextNodePrefix str node = case nEdge node of
  Tip _ -> Nothing
  Bin br ds ->
    if br `isPrefixOfByteString` dropByteString (Builtins.lengthOfByteString pr) str
      && BitField.testBitField ds (str `Builtins.indexByteString` (Builtins.lengthOfByteString pr + Builtins.lengthOfByteString br))
      then Just $ takeByteString (Builtins.lengthOfByteString pr + Builtins.lengthOfByteString br + 1) str
      else Nothing
  where
    pr = nPrefix node

{- | @'elemNode' str node@ returns True iff we can be certain that @node@ can
 prove that @str@ is in the set and otherwise throws an exception. See Note
 [Node Representation].
-}
elemNode :: BuiltinByteString -> Node -> Bool
elemNode str node
  | nPrefix node `isPrefixOfByteString` str && isNothing (nextNodePrefix str node) = case nEdge node of
    Tip su -> pr `Builtins.appendByteString` su == str
    Bin _ _ -> False
  | otherwise = traceError "inconclusive 'elemNode'"
  where
    pr = nPrefix node

-- @'insertNode' str node@ returns the list of nodes which should replace
-- @node@ in order to insert @str@ in the tree.
--
-- Preconditions:
--
--      - @elemNode str node@ must be False
--
-- This is mostly just tedious case analysis.
insertNode :: BuiltinByteString -> Node -> Ib Node
insertNode str node =
  if elemNode str node
    then traceError "error 'insertNode' inserting a node which already exists"
    else case nEdge node of
      Tip suf ->
        -- Invariant:
        --  - We know that @str@ and @pr ++ suf@ are not equal (otherwise, the
        --  assertion would fail)
        let br = dropByteString (lengthOfByteString pr) str `lcp` suf
            d1 = suf `Builtins.indexByteString` lengthOfByteString br
            d2 = str `Builtins.indexByteString` (lengthOfByteString pr + lengthOfByteString br)

            n1 =
              Node
                { nPrefix = pr
                , nEdge =
                    Bin br $
                      BitField.setBitField (BitField.setBitField BitField.zeroes256 d1) d2
                }
            n2 =
              Node
                { nPrefix = pr `Builtins.appendByteString` snocByteString br d1
                , nEdge = Tip $ dropByteString (lengthOfByteString br + 1) suf
                }
            n3 =
              Node
                { nPrefix = pr `Builtins.appendByteString` snocByteString br d2
                , nEdge = Tip $ dropByteString (Builtins.lengthOfByteString pr + Builtins.lengthOfByteString br + 1) str
                }
         in Three n1 n2 n3
      Bin br ds ->
        let nbr = dropByteString (lengthOfByteString pr) str `lcp` br

            ntip =
              let len = lengthOfByteString pr + lengthOfByteString nbr + 1
               in Node
                    { nPrefix = takeByteString len str
                    , nEdge = Tip $ dropByteString len str
                    }
         in if lengthOfByteString nbr == lengthOfByteString br
              then
                let n1 =
                      Node
                        { nPrefix = pr
                        , nEdge = Bin br $ BitField.setBitField ds (str `Builtins.indexByteString` (lengthOfByteString pr + lengthOfByteString br))
                        }
                 in Two n1 ntip
              else -- implies that @lengthOfByteString nbr < lengthOfByteString br@

                let d1 = br `Builtins.indexByteString` lengthOfByteString nbr
                    d2 = str `Builtins.indexByteString` (lengthOfByteString pr + lengthOfByteString nbr)

                    n1 =
                      Node
                        { nPrefix = pr
                        , nEdge = Bin nbr $ BitField.setBitField (BitField.setBitField BitField.zeroes256 d1) d2
                        }
                    n2 =
                      Node
                        { nPrefix = pr `Builtins.appendByteString` snocByteString nbr d1
                        , nEdge = Bin (dropByteString (lengthOfByteString nbr + 1) br) ds
                        }
                 in Three n1 n2 ntip
  where
    pr = nPrefix node
-- * Validators for insertion in the distributed set

{- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
 done in the 'insertNode' function.
-}
{-# INLINEABLE mkInsertValidator #-}
mkInsertValidator :: Ds -> DsDatum -> DsRedeemer -> ScriptContext -> Bool
mkInsertValidator ds _dat red ctx =
  traceIfFalse "error 'mkInsertValidator' invalid insertion" $
    (== contNodes) $
      normalizeIbNodes $
        insertNode nStr ownNode
  where
    -- Aliases:
    info :: TxInfo
    info = scriptContextTxInfo ctx

    curSymb :: CurrencySymbol -- This uniquely identifies this tree.
    curSymb = dsSymbol ds

    nStr :: BuiltinByteString
    nStr = dsStr red

    -- Given a value, gets the 'TokenName's that correspond to the given
    -- 'CurrencySymbol' from 'dsSymbol'
    getTns :: Value -> [TokenName]
    getTns v = case AssocMap.toList <$> AssocMap.lookup curSymb (Value.getValue v) of
      Just kvs | all ((== 1) . snd) kvs -> map fst kvs
      _ -> traceError "error 'mkInsertValidator': 'getTns' failed"

    -- Given a TxOut, this will get (and check) if we have the 'TokenName' and
    -- required datum.
    getTxOutNodeInfo :: TxOut -> Node
    getTxOutNodeInfo txout =
      let err = "error 'mkInsertValidator': 'getTxOutNodeInfo' failed"
          tn = case getTns (txOutValue txout) of
            [tn'] -> tn'
            _ -> traceError err

          dhash = case txOutDatumHash txout of
            Just dhash' -> dhash'
            Nothing -> traceError err

          dn = case fmap getDatum (Contexts.findDatum dhash info) >>= PlutusTx.fromBuiltinData of
            Just dn' -> dn'
            Nothing -> traceError err
       in mkNode (unTokenName tn) dn

    ownNode :: Node
    ownNode = case Contexts.findOwnInput ctx of
      Just inp -> getTxOutNodeInfo $ txInInfoResolved inp
      Nothing -> traceError "error 'mkInsertValidator': 'ownNode' doesn't exist"

    contNodes :: Ib Node
    contNodes =
      normalizeIbNodes $ fromListIb $ map getTxOutNodeInfo $ Contexts.getContinuingOutputs ctx

    normalizeIbNodes = normalizeIbOn nPrefix

instance ValidatorTypes Ds where
  type DatumType Ds = DsDatum
  type RedeemerType Ds = DsRedeemer

-- | The typed validator script for the distributed set.
typedInsertValidator :: Ds -> TypedValidator Ds
typedInsertValidator ds =
  Scripts.mkTypedValidator @Ds
    ( $$(PlutusTx.compile [||mkInsertValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode ds
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DsDatum @DsRedeemer

-- | The regular validator script for the distributed set.
insertValidator :: Ds -> Validator
insertValidator = Scripts.validatorScript . typedInsertValidator

-- | The address for the distributed set.
insertAddress :: Ds -> Address
insertAddress = Scripts.validatorAddress . typedInsertValidator

-- * Minting Policy for Initializing the Distributed Set

-- | 'mkDsPolicy' needs to verify the following.  See Note [Node Representation].
mkDsPolicy :: DsMint -> () -> ScriptContext -> Bool
mkDsPolicy gds _red ctx = case inputs of
  -- We only allow consuming a single token of this currency symbol (or the
  -- initial transaction to consume).
  [i]
    -- This is the minting new tokens to grow the distributed set case.
    | Left txout <- i
      , [itn] <- getTns (txOutValue txout) ->
      traceIfFalse "error 'mkDsPolicy' bad extend" $
        let contTns =
              normalizeIb $
                fromListIb $
                  mapMaybe (\o -> if txOutAddress txout == txOutAddress o then case getTns (txOutValue o) of [tn] -> Just tn; _ -> Nothing else Nothing) $
                    txInfoOutputs info
         in normalizeIb (fromListIb (itn : minted)) == contTns
    -- This is the "initializing" the distributed set case.
    | Right _ <- i -> traceIfFalse "error 'mkDsPolicy' bad initialization" $ case minted of
      [mt] ->
        -- Then, we find the output which contains this token and
        -- verify that it has the datum for the right root of the
        -- distributed set
        case flip find (txInfoOutputs info) $ \o -> let tns = getTns (txOutValue o) in not (null tns) of
          Just txout -> fromMaybe False $ do
            dhash <- txOutDatumHash txout
            dn <- fmap getDatum (Contexts.findDatum dhash info) >>= PlutusTx.fromBuiltinData
            return $ rootNode == mkNode (unTokenName mt) dn
          _ -> False
      _ -> False
  _ -> traceError "error 'mkDsPolicy' illegal transaction inputs"
  where
    -- Aliases
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

    mint :: Value
    mint = txInfoMint info

    minted :: [TokenName]
    minted = getTns mint

    -- Helper functions
    getTns :: Value -> [TokenName]
    getTns v = case AssocMap.toList <$> AssocMap.lookup ownCurSymb (Value.getValue v) of
      Just kvs | all ((== 1) . snd) kvs -> map fst kvs
      _ -> []

    -- these are the inputs which either
    --
    --      * Left: spend an output which contains a currency of this token (implemented as @p1@); or
    --
    --      * Right: spend the distinguished address in 'DsMint' (implemented as @p2@)
    inputs :: [Either TxOut ()]
    inputs = flip mapMaybe (txInfoInputs info) $ \i ->
      let p1, p2 :: Maybe (Either TxOut ())
          p1 = case tns of
            Just [(_tn, amount)] | amount == 1 -> Just $ Left txout
            Just _ -> traceError "error 'mkDsPolicy' illegal spend"
            _ -> Nothing
          p2 =
            if txInInfoOutRef i == dsmTxOutRef gds
              then Just $ Right ()
              else Nothing

          txout = txInInfoResolved i
          tns = fmap AssocMap.toList $ AssocMap.lookup ownCurSymb $ getValue $ txOutValue txout
       in case p1 of
            Just _ -> p1
            Nothing -> p2

-- | 'dsPolicy' is the minting policy
dsPolicy :: DsMint -> MintingPolicy
dsPolicy dsm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dsm

-- | 'dsCurSymbol' is the currency symbol for the distributed set
dsCurSymbol :: DsMint -> CurrencySymbol
dsCurSymbol = Contexts.scriptCurrencySymbol . dsPolicy

{- Note [Comparison to Stick Breaking Set]
    This was based off of the [Stick Breaking
    Set](https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md)
    with some modifications of course. We'll first recall the data type for the nodes of
    the Stick Breaking Set and note some comparisions:

    Recall that the nodes in the Stick Breaking Set are defined as follows.

    > data Node = Node
    > { prefix :: BuiltinByteString
    > , leaves :: [BuiltinByteString], branches :: [BuiltinByteString]
    > }

    A set of 'Node's forms with a distinguished 'Node' called the /root/ is a /Stick Breaking Set/.

    The intuitive idea behind this is that:

        - 'prefix' (like in the implementation here) denotes the characters
        we've scanned so far to reach this node i.e., it provides a proof of
        what we've already seen to reach the current node.

        - 'leaves': given an BuiltinByteString @str@ in 'leaves', we say that
        @prefix ++ str@ is in the Stick Breaking Set.

        - 'branches': given an BuiltinByteString @str@ in 'branches', we say
        that there exists another node, say @node'@, such that @node'@ has the
        'prefix' field set to @prefix ++ str@ i.e., @str@ can be thought of as
        an edge to @node'@ from this current node.

    Example. Given a set of 'Node's as follows:
    >   root = Node { prefix = "a", leaves = [], branches = ["a","b","cc"]}
    >   node1 = Node { prefix = "aa", leaves = [""], branches = []}
    >   node2 = Node { prefix = "ab", leaves = [""], branches = []}
    >   node3 = Node { prefix = "acc", leaves = [""], branches = []}
    We may picture this as the graph for which the edges between nodes are
    strings as follows:
    >                              root
    >                         { prefix = "a" }
    >            "a"         /       |         \
    >          /-------------        | "b"      -----------\
    >         /                      |                      \ "cc"
    >       node1                  node2                   node3
    > { prefix = "aa"         { prefix = "aa"         { prefix = "cc"
    > , leaves = [""] }       , leaves = [""] }       , leaves = [""] }
    where "aa", "ab", "acc" are /in/ the set since @node1@ tells us @"aa" <> ""
    == "aa"@ and similiar reasoning for @node2@, and @node3@.

    Pros:
        (1) The overall space taken up by every node should be smaller than or
        equal to the implementation here since it the 'branches' may be
        arbitrary strings which can "factor out" common portions of strings
        inserted to be stored in the branch.

        (2) Usual advantages of trie data structures: Worst case lookup (for
        offchain code) is in big Oh of the length of the longest string.

    Cons:
        (1) Individual transactions can get too large for our purposes. In the
        trustless-sidechain, we are storing hashes of strings which has a size
        of 32 bytes (blake2 has a message digest of 256 bits), so in the worst
        case, we can have a 'Node' like with @prefix = ""@, @2^8@ (recall @2^8@
        is the size of the alphabet) leaves with a distinct first character,
        and no branches -- meaning this Node will take up

        >    2^8 leaves      32 bytes
        >   ------------ * -----------  =  8192 bytes
        >    1 Node          1 leaf

        so if a transaction insert another element at this Node, they must spend this Node
        and also produce another 'Node' with at least the same amount of data as this 'Node'.
        This means that the plutus transaction must take at least

        >    2 *  8192 bytes =  16384 bytes

        which according to
        [here](https://testnets.cardano.org/en/testnets/cardano/tools/plutus-fee-estimator/)
        the datum is included in the size of the transaction but transactions
        can be at most 16384 bytes i.e., this will exceed the maximum
        transaction size (note that this estimate doesn't even include extra
        costs from constructors and other required things from Plutus).

        Why is this bad? Well, this means that an adversary can be malicious
        and make transaction impossible to mint. This is more of a problem when
        the side-chain initially starts (since as the chain goes on, the leaves
        will be broken up and hence individual nodes will get smaller); BUT it
        is perfectly feasible for adversaries to do a small proof of work (See
        Note [Adversary Proof of Work Attack of Stick Breaking Set]) to insert things in the set with
        common prefixes that differ all differ in a bit of the same index.

        (2) Insertion code is a bit complex and requires scanning through all
        the leaves and branches calculating the longest common prefix. While
        this is upper bounded by by the size of the alphabet (recall this is
        @2^8@), [other data
        structures](https://github.com/Plutonomicon/plutonomicon/blob/main/assoc.md)
        boast an honest to goodness constant time insertion (provided you know
        where to insert it). This actualy isn't that bad though.

        (3) the optimization of having branches that are strings is mainly
        helpful when strings of long common prefixes are inserted. I don't
        think this case will be particularly common because: the strings are of
        a fixed rather small size 32 in the trustless-sidechain; and we may
        regard the strings as random inputs since they are hashes so the
        probability of strings sharing long common prefixes becomes
        exponentially small. I don't have a formal analysis, and I think the
        average case analysis of this is honestly quite
        tricky...

    So, I think the Cons (1) is enough of a reason to not use exactly the Stick
    Breaking Set's implementation. While this is very unlikely with honest
    participants and this becomes impossible as the set grows (since it will
    break up the node), an adversary in the beginning (see Note [Adversary
    Proof of Work Attack of Stick Breaking Set]) can actually exploit this and
    essentialyl "burn" other people's tokens forever (since they can never
    spend this transaction as it'll run over the budget).

    But the Stick Breaking Set had good ideas nonetheless -- a great place to
    start :D.
-}

{- Note [Adversary Proof of Work Attack of Stick Breaking Set]

    This is an attack on the Stick Breaking Set (see Note [Comparison to Stick
    Breaking Set]) where one can make a Node that is too large for anyone to
    spend. Note that this scenario occurs exactly when one inserts strings
    which all share a common prefix and all differ at the next character i.e.,
    when given strings like

    > str1 = a11 a12...a1k....
    > str2 = a21 a22...a2k....
    > str3 = a31 a32...a3k....
    > ...

    (note on notation: @a21@ denotes @str2@'s @1@st character)

    such that

        - @si@ for @s = 1,..@ and @i=1,..,k-1@ are all the same; and

        - @sk@ for @s = 1,..@ are all distinct. Note that by definition
        @k@ is the first index for which all these strings differ.

        - and the remaining characters may be arbitrary

    If we were just given this set, and inserting arbitrary strings, it would
    be very easy to exploit this attack. Thankfully in the trustless-sidechain,
    we are actually inserting blake2 hashes of some message into the set so it
    isn't that "easy" to just find a collection of strings like this since we
    assume that hash functions are preimage resistant. Hence, execute this
    attack the adversary would need to do some sort of "Proof of Work" brute
    force search to find such strings. We do an expected value analysis for
    finding a set of @2^8@ strings which satisfy the above requirements to
    execute this attack where we assume that each character has an equal
    probability to be hashed to and are independent (i.e., avalanche effect of
    hash functions).

    Let @X@ denote the random variable which denotes the number of guesses an
    adversary would have to put in the hash function in order to satisfied the
    above requirements.

    Consider some point in time when the @k@th character of @str1@ to
    @str(i-1)@ all differ, and each of those strings all share the same common
    prefix. Let @Xi@ be the random variable that denotes the number of guesses
    to make until the @k@th character from @stri@ differs from the previous
    strings (@str1@ to @str(i-1)@) AND @stri@ shares the same common prefix of
    the strings @str1@ to @str(i-1)@.
    This event occurs with probability (probability of the prefix being the
    same as the previous strings, and probability of choosing a different
    character from the previous strings)

    > (1 / (2^8)^(k-1)) * ( 1 - ( (i - 1) / 2^8 ) ) = ( 2^8 - i + 1 ) / 2^(8 k)

    which is a Geometrically Distributed Random variable so the expected value is

    > E[Xi] = 2^(8 k)  / ( 2^8 - i + 1 )                    [*]

    Clearly,

    > X = X1 + X2  + ... + X(2^8)

    so by Linearity of Expectation, it follows that

    > E[X] = E[X1 + X2  + ... + X(2^8)]
    >      = E[X1] + [X2]  + ... + E[X(2^8)]                  [linearity of expectation]
    >      = sum i=1 to 2^8 of E[Xi]
    >      = sum i=1 to 2^8 of 2^(8 k)  / ( 2^8 - i + 1 )     [by [*]]

    which as Haskell code is:

    > f k = sum $ map (\i -> 2^(8 * k) `div` ( 2^8 - i + 1)) $ [1..2^8]

    Let's recall what the function @f@ applied to @k@ says: given a prefix of
    size @0 <= k <= 32@, @f k@ computes the expected number of guesses an
    adversary would have to make to find @2^8@ strings that share a common
    prefix up to the @k-1@th character and are all distinct on the @k@th
    character.

    Now, when we have a prefix of size @k@ for a node, in the worst case when
    the 'leaves' field is filled with strings as long as possible which is @32
    - k@ (recall that in the trustless-sidechain, we are storing hashes that
    are 32 bytes long), since there the alphabet contains @2^8@ distinct
    characters, we get the worst case space requirements for a node is when the
    'leaves' field has @2^8@ strings of size @32 - k@ i.e., the worst case
    space of a node is given by

    > k + 2^8 * (32 - k)  bytes

    and so a transaction spending this node, would take at least 2x this, a
    spending transaction would take at least

    > 2 * (k + 2^8 * (32 - k))  bytes           [**]

    Recall from
    [here](https://testnets.cardano.org/en/testnets/cardano/tools/plutus-fee-estimator/)
    that this datum size [**] becomes problematic when it becomes larger than
    16384 bytes.
    Note that [**] is clearly decreasing (just take the derivative), and we may
    note that the size of the transaction in the worst case is only problematic
    for perhaps the first few values of @k@ -- see the below table:

        k | size of transaction in the worst case
       ------------------------------------------
        0 |                 16384
        1 |                 15873
        2 |                 15362
        3 |                 14851
        4 |                 14340
        5 |                 13829
        6 |                 13318
        7 |                 12807
        8 |                 12296

        (remaining values ommitted since they clearly fit within the
        transaction size limit)
    where we see that assuming a [typical script
    size](https://testnets.cardano.org/en/testnets/cardano/tools/plutus-fee-estimator/)
    of 4000-8000 bytes, we may go over the maximum transaction size limit for
    @k=0,1,2@ and it would be reasonable to include extra values of @k@ since
    we're not completely counting all things that contribute space.

    And from our result of the function @f@, we may associate how many guesses and adversary would have to make
    in order to build a Node with such space requirements as follows.
         Prefix of size k | Expected number of guesses the adversary must make
         ---------------------------------------------------------------------
         0                | 1466
         1                | 401255
         2                | 102749359
         3                | 26303861211
         4                | 6733788499037
         5                | 1723849855776909
         6                | 441305563078916342
    where if we assume 10^8 computations guesses per second (standard
    competitive programming rule of thumb), it is reasonable for an adversary
    to give strings that invoke the worst case Node size for prefixes of size
    @k=0,..,4@.

    Unfortunately for us, this isn't good -- it demonstrates that an adversary
    can actually invoke a worst case Node size and hence make it impossible for
    honest people to mint their tokens from the side chain if they
    unfortunately share the same prefix of the adversary.

    So what does this suggest? This isn't the right way to go! Indeed, this is
    the motivating reason for why we didn't go with this representation... with
    the representation here, the probability of breaking the leafs is
    exponentially small (as strings get larger).

 -}

{- Note [Comparison to a Linked List]
 An alternative would be to implement the distributed set would be to implement
 an ordered linked list on chain. As a sketch, you'd have something
 > ... ---> "aaa" ----> "caa" ---> ...
 and to insert a string like "baa", you'd consume the UTxOs with datum "aaa" and "caa" to produce
 > ... ---> "aaa" ----> "baa" ----> "caa" ---> ...

 Why did we not want to go this route? Well it actually seems pretty good --
 the only short coming is that the offchain code has no way of figuring out
 where to look when trying to find which UTXO can prove that an element is not
 in the set. So as this system runs on for a couple of decades, offchain would
 have to do a linear search of every single UTXO making this crippling slow as
 the years pass. To remedy this, one would have to implement an extra service
 to maintain an efficient query to the UTXOs onchain.

 So if someone wanted to participate in the trustless-sidechain, they would
 have to scan through the entire blockchain to build an trustless-sidechain
 UTXO set which would maintain the efficient query to these linked list nodes.

 Honestly, this idea isn't that bad in my opinion :)
 -}
