{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Implementation of a distributed set an on-chain distributed set to admit
 proof of a set membership
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
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutDatumHash, txOutValue),
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
import TrustlessSidechain.OnChain.Types (DsRedeemer (dsStr))
import Prelude qualified

-- | 'Byte' is an internal type alias to denote an element of a ByteString
type Byte = Integer

{- | 'NonEmpty' is an internal type to implicitly maintain the invariant the
 the list is non empty.
-}
type NonEmpty a = [a]

{- | 'DSDatum' is sits at the datum. We often just call this a Node. See Note
 [Node Representation] for more details
-}
data DsDatum = DsDatum
  { -- | 'True' iff 'dsPrefix' is considered an element of this set
    dsLeaf :: !Bool
  , -- | The edges to other nodes
    dsBranches :: Maybe (BuiltinByteString, NonEmpty Byte)
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

-- Note [Node Representation]
-- Implicitly, this forms an M-ary tree for which:
--
--      (1) There is a root node with 'prefix' as @""@.
--
--      (2) A leaf is a node for which 'branches' is 'Nothing'.
--
--      (3) If 'branches' is @Just (branchPrefix, ds)@, then for every @d@
--      in @ds@, there must exist a unique node which satisifies
--
--      > prefix = this prefix ++ branchPrefix ++ d
--
--
-- Definition.
--  We say that @str@ is in the distributed set iff there exists a node for
--  which 'leaf' is @True@ and @prefix@ is @str@.
--
-- Claim 1.
--  For every string @str@ not in the distributed set, there exists a unique node that satisfies the
--  following.
--
--      (a) 'prefix' is a prefix of @str@
--
--      (b) if @'prefix' == str@, then @'leaf'@ is false
--
--      (c) If @'branches' = Just (branchPrefix, ds)@, then for every @d@ in
--      @ds@, @branchPrefix ++ [d]@ cannot be a prefix of @drop (length prefix)
--      str@. See 'existsNextNode' for an implementation.
-- Proof.
--  The only complication is showing (c), but this follows from (3).

makeIsDataIndexed ''DsDatum [('DsDatum, 0)]
deriveJSON defaultOptions ''DsDatum

{- | 'hash' is an internal function to store the hash of the to put in the set.
 This is just a thin wrapper around the 'Builtins.blake2b_256' hash function

 TODO: okay we need to actually hash things in the script still.
-}
{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> BuiltinByteString
hash = Builtins.blake2b_256

newtype Ds = Ds
  { -- | 'dsSymbol' is the 'CurrencySymbol' for the minting policy that
    -- creates tokens which idenfity branches.
    dsSymbol :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

makeIsDataIndexed ''Ds [('Ds, 0)]
deriveJSON defaultOptions ''Ds
PlutusTx.makeLift ''Ds

-- | 'Node' is an internal data type of the tree node used in the validator.
data Node = Node
  { nPrefix :: !BuiltinByteString
  , nLeaf :: !Bool
  , nBranches :: Maybe (BuiltinByteString, NonEmpty Byte)
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

-- * Standard helper functions

-- | @'isPrefixOfByteString' str0 str1@ returns true iff @str0@ is a prefix of @str1@
{-# INLINEABLE isPrefixOfByteString #-}
isPrefixOfByteString :: BuiltinByteString -> BuiltinByteString -> Bool
isPrefixOfByteString str0 str1 = str0Len <= str1Len && str0 == takeByteString str0Len str1
  where
    str0Len, str1Len :: Integer
    str0Len = lengthOfByteString str0
    str1Len = lengthOfByteString str1

{- | This satisfies
 > 'normalizeOrder' = 'normalizeOrderBy' 'compare'
-}
{-# INLINEABLE normalizeOrder #-}
normalizeOrder :: Ord a => [a] -> [a]
normalizeOrder = normalizeOrderBy compare

{- | 'normalizeOrderBy' is used to order prefixes when building transactions.
 In particular, this is a wrapper for a sorting algorithm for which we know
 that the order after sorting is unique.

 We do a naive /O(n^2)/ sort to keep the script size down (inputs are small
 anyways).
-}
{-# INLINEABLE normalizeOrderBy #-}
normalizeOrderBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
normalizeOrderBy cmp = go
  where
    le :: a -> a -> Bool
    le a b = case a `cmp` b of
      GT -> False
      _ -> True

    isSorted :: [a] -> Bool
    isSorted (s0 : s1 : ss) = s0 `le` s1 && isSorted (s1 : ss)
    isSorted _ = True

    go :: [a] -> [a]
    go lst
      | isSorted lst = lst
      | otherwise = go $ bubble lst

    bubble :: [a] -> [a]
    bubble (s0 : s1 : ss)
      | s0 `le` s1 = s0 : bubble (s1 : ss)
      | otherwise = s1 : bubble (s0 : ss)
    bubble lst = lst

-- | 'uncons' copies the Prelude 'Prelude.uncons'.
{-# INLINEABLE uncons #-}
uncons :: [a] -> Maybe (a, [a])
uncons (a : as) = Just (a, as)
uncons _ = Nothing

-- | 'tailByteString' computes the tail of a 'BuiltinByteString'.
{-# INLINEABLE tailByteString #-}
tailByteString :: BuiltinByteString -> BuiltinByteString
tailByteString str
  | nullByteString str = traceError "error 'tailByteString': empty ByteString"
  | otherwise = dropByteString 1 str

-- | 'headByteString' computes the tail of a 'headByteString'.
{-# INLINEABLE headByteString #-}
headByteString :: BuiltinByteString -> Byte
headByteString str
  | nullByteString str = traceError "error 'headByteString': empty ByteString"
  -- this check is probably redundent because plutus will do it anyways / throw an error
  | otherwise = indexByteString str 0

-- | 'lastByteString' computes the bytestring that is the same except with the last element removed
{-# INLINEABLE initByteString #-}
initByteString :: BuiltinByteString -> BuiltinByteString
initByteString str
  | nullByteString str = traceError "error 'initByteString': empty initByteString"
  -- this check is probably redundent because plutus will do it anyways / throw an error
  | otherwise = takeByteString (lengthOfByteString str - 1) str

-- | 'lastByteString' returns the last character in the list.
{-# INLINEABLE lastByteString #-}
lastByteString :: BuiltinByteString -> Byte
lastByteString str
  | nullByteString str = traceError "error 'lastByteString': empty lastByteString"
  -- this check is probably redundent because plutus will do it anyways / throw an error
  | otherwise = indexByteString str (lengthOfByteString str - 1)

-- | 'singletonByteString' returns a bytestring which just contains the single byte provided.
{-# INLINEABLE singletonByteString #-}
singletonByteString :: Byte -> BuiltinByteString
singletonByteString b = consByteString b emptyByteString

-- | 'snocByteString' appends the character to the end of the string.
{-# INLINEABLE snocByteString #-}
snocByteString :: BuiltinByteString -> Byte -> BuiltinByteString
snocByteString str b = appendByteString str (singletonByteString b)

{-# INLINEABLE nullByteString #-}

-- | @'nullByteString' str@ returns 'True' iff @str@ has length @0@.
nullByteString :: BuiltinByteString -> Bool
nullByteString str = lengthOfByteString str == 0

instance Eq DsDatum where
  {-# INLINEABLE (==) #-}
  a == b =
    dsLeaf a == dsLeaf b
      && case (dsBranches a, dsBranches b) of
        (Nothing, Nothing) -> True
        (Just (pre0, ds0), Just (pre1, ds1)) ->
          pre0 == pre1 && ((==) `PlutusPrelude.on` normalizeOrder) ds0 ds1
        _ -> False

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b =
    nPrefix a == nPrefix b && nLeaf a == nLeaf b
      && case (nBranches a, nBranches b) of
        (Nothing, Nothing) -> True
        (Just (pre0, ds0), Just (pre1, ds1)) ->
          pre0 == pre1 && ((==) `PlutusPrelude.on` normalizeOrder) ds0 ds1
        _ -> False

makeIsDataIndexed ''Node [('Node, 0)]
deriveJSON defaultOptions ''Node

-- * Node

-- 'existsNextNode' tests if there is a node which is the "next node" (See Note
-- [Node Representation]) which may or may not provide proof of existence or
-- absence from the set.
--
-- Preconditions:
--  - @prefix node `isPrefixOfByteString` str@ must be true (i.e., you should check
--  this independently as this predicate is indeterminate otherwise).
existsNextNode :: BuiltinByteString -> Node -> Bool
existsNextNode str node =
  -- You need to check this guard on your own! The author wanted to avoid having
  -- this be a partial funciton..
  -- > | pre `isPrefixOfByteString` str =
  case nBranches node of
    Just (branchPrefix, ds) ->
      let strSuf' = dropByteString (lengthOfByteString branchPrefix) strSuf
       in branchPrefix `isPrefixOfByteString` strSuf
            && if not (nullByteString strSuf')
              then any (\d -> headByteString strSuf' == d) ds
              else False
    Nothing -> False
  where
    pre = nPrefix node
    strSuf = dropByteString (lengthOfByteString pre) str

{- | @'elemNode' str node@ returns True iff we can be certain that @node@ can
 prove that @str@ is in the set and otherwise throws an exception. See Note
 [Node Representation].
-}
elemNode :: BuiltinByteString -> Node -> Bool
elemNode str node = if nPrefix node == str && nLeaf node then True else traceError "inconclusive 'elemNode'"

{- | @'elemNode' str node@ returns True iff we can be certain that @node@ can
 prove that @str@ is in the set and otherwise throws an exception. See Note
 [Node Representation].
-}
notElemNode :: BuiltinByteString -> Node -> Bool
notElemNode str node = if (pre `isPrefixOfByteString` str) && (pre /= str || not (nLeaf node)) && not (existsNextNode str node) then True else traceError "inconclusive 'notElemNode'"
  where
    pre = nPrefix node

-- | 'lcp' computes the longest common prefix of two strings.
lcp :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString
lcp s t = takeByteString (go 0) s
  where
    go ! ix
      | ix >= lengthOfByteString s || ix >= lengthOfByteString t = ix
      | otherwise =
        if s `indexByteString` ix == t `indexByteString` ix
          then go (ix + 1)
          else ix

-- @'insertNode' str node@ returns the list of nodes which should replace
-- @node@ in order to insert @str@ in the tree.
--
-- Preconditions:
--
--      - @notElemNode str node@ must be True (otherwise, this returns
--      Nothing).
--
-- This is mostly just tedious case analysis.
insertNode :: BuiltinByteString -> Node -> Maybe [Node]
insertNode str node =
  flip (PlutusPrelude.bool Nothing) (isPrefixOfByteString (nPrefix node) str && notElemNode str node) $
    Just $
      if str == pre
        then [node {nLeaf = True}]
        else case nBranches node of
          -- Notes:
          --  @strSuf@ is non empty (otherwise we'd contradict @str /= pre@).
          Just (branchPrefix, ds) ->
            let -- Generates the nodes corresponding to the string.
                -- Assumes that
                -- > inp = p ++ inpSuf
                -- where @inpSuf = ic : inpSuf'@
                -- and there exists a unique node which satisfies
                -- > nPrefix = s
                -- > nBranches = Just (t, ic : _)
                -- where @s ++ t = p@.
                fromHeadStrNodes :: BuiltinByteString -> BuiltinByteString -> [Node]
                fromHeadStrNodes p inp =
                  let inpSuf = dropByteString (lengthOfByteString p) inp
                      inpSuf' = tailByteString inpSuf
                   in if nullByteString inpSuf'
                        then
                          [ Node
                              { nPrefix = inp
                              , nLeaf = True
                              , nBranches = Nothing
                              }
                          ]
                        else
                          [ Node
                              { nPrefix = snocByteString p $ headByteString inpSuf
                              , nLeaf = False
                              , nBranches = Just (initByteString inpSuf', [lastByteString inpSuf'])
                              }
                          , Node
                              { nPrefix = snocByteString p (headByteString inpSuf) `appendByteString` inpSuf' -- this can be just @inp@
                              , nLeaf = True
                              , nBranches = Nothing
                              }
                          ]

                fromHeadPreStr :: [Node]
                fromHeadPreStr = fromHeadStrNodes pre str
             in if nullByteString branchPrefix
                  then node {nBranches = Just (branchPrefix, headByteString strSuf : ds)} : fromHeadPreStr
                  else
                    let branchPrefixLcpStrSuf = branchPrefix `lcp` strSuf
                     in if nullByteString branchPrefixLcpStrSuf
                          then
                            [ node
                                { nBranches = Just (branchPrefixLcpStrSuf, [headByteString strSuf, headByteString branchPrefix])
                                }
                            , Node
                                { nPrefix = snocByteString pre $ headByteString branchPrefix
                                , nLeaf = False
                                , nBranches = Just (tailByteString branchPrefix, ds)
                                }
                            ]
                              ++ fromHeadPreStr
                          else
                            let strSuf' = dropByteString (lengthOfByteString branchPrefixLcpStrSuf) strSuf
                                branchPrefix' = dropByteString (lengthOfByteString branchPrefixLcpStrSuf) branchPrefix

                                fromHeadPreLcpStr = fromHeadStrNodes (pre `appendByteString` branchPrefixLcpStrSuf) str
                             in if nullByteString strSuf'
                                  then
                                    [ node
                                        { nBranches = Just (initByteString branchPrefixLcpStrSuf, [lastByteString branchPrefixLcpStrSuf])
                                        }
                                    , Node
                                        { nPrefix = pre `appendByteString` branchPrefixLcpStrSuf
                                        , nLeaf = True
                                        , nBranches = Just (branchPrefix', ds)
                                        }
                                    ]
                                  else
                                    if nullByteString branchPrefix'
                                      then
                                        node
                                          { nBranches = Just (branchPrefixLcpStrSuf, headByteString strSuf' : ds)
                                          } :
                                        fromHeadPreLcpStr
                                      else
                                        [ node
                                            { nBranches = Just (branchPrefixLcpStrSuf, [headByteString branchPrefix', headByteString strSuf'])
                                            }
                                        , Node
                                            { nPrefix = pre `appendByteString` snocByteString branchPrefixLcpStrSuf (headByteString branchPrefix')
                                            , nLeaf = False
                                            , nBranches = Just (tailByteString branchPrefix', ds)
                                            }
                                        ]
                                          ++ fromHeadPreLcpStr
          Nothing ->
            [ node {nBranches = Just (initByteString strSuf, [lastByteString strSuf])}
            , Node
                { nPrefix = pre `appendByteString` strSuf
                , nLeaf = True
                , nBranches = Nothing
                }
            ]
  where
    pre = nPrefix node
    strSuf = dropByteString (lengthOfByteString pre) str

{- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
 - done in the 'insertNode' function.
 -
 - We will note that there are alternative (probably better!) ways of doing
 - this. The author postulates that doing a modified DFS just to test if the
 - insertion is right (indeed, it is not unique) should suffice.
-}
mkInsertValidator :: Ds -> DsDatum -> DsRedeemer -> ScriptContext -> Bool
mkInsertValidator ds _dat red ctx =
  traceIfFalse "error 'mkInsertValidator' incorrect new nodes" $ case insertNode nStr ownNode of
    Nothing -> False
    Just nnodes -> normalizeNodes nnodes == nOwnNode : contNodes && normalizeOrder (map unTokenName mintedTns) == map nPrefix contNodes
  where
    -- Similar reasoning for testing if we minted exactly the tokens
    -- for the new nodes.

    -- Aliases:
    ------------------
    info :: TxInfo
    info = scriptContextTxInfo ctx

    curSymb :: CurrencySymbol -- This uniquely identifies this tree.
    curSymb = dsSymbol ds

    mint :: Value
    mint = txInfoMint info

    nStr :: BuiltinByteString
    nStr = dsStr red

    -- Given a value, gets the 'TokenName's that correspond to the given
    -- 'CurrencySymbol' from 'dsSymbol'
    getTns :: Value -> Maybe [TokenName]
    getTns = fmap AssocMap.keys . AssocMap.lookup curSymb . Value.getValue

    -- Given a TxOut, this will get (and check) if we have the 'TokenName' and
    -- required datum.
    getTxOutNodeInfo :: TxOut -> Node
    getTxOutNodeInfo txout = fromMaybe err $ do
      tn <-
        getTns (txOutValue txout) >>= \case
          [tn] -> return tn
          _ -> Nothing
      dhash <- txOutDatumHash txout
      d <- fmap getDatum (Contexts.findDatum dhash info) >>= PlutusTx.fromBuiltinData
      return $
        Node
          { nPrefix = unTokenName tn
          , nLeaf = dsLeaf d
          , nBranches = dsBranches d
          }
      where
        err :: a
        err = traceError "error 'mkInsertValidator' failed to query node info"

    ownNode :: Node
    ownNode =
      fromMaybe err $
        Contexts.findOwnInput ctx >>= \inp ->
          let txout = txInInfoResolved inp
           in return $ getTxOutNodeInfo txout
      where
        err = traceError "error 'mkInsertValidator' in querying Spending input"

    mintedTns :: [TokenName]
    mintedTns = normalizeOrder $ fromMaybe err $ getTns mint
      where
        err :: a
        err = traceError "error 'mkInsertValidator' no minted currency symbols"

    -- The continuing outputs with their 'TokenName' corresponding to the
    -- 'CurrencySymbol' from 'dsSymbol' AND the datum. This errors in the case
    -- that we have strctly more than 1 'TokenName' (this is the minting for
    -- the NFT) or there is no datum.
    --
    -- @nOwnNode@ is the new spent node to replace the node we are spending.
    nOwnNode :: Node
    contNodes :: [Node]
    (nOwnNode, contNodes) =
      fromMaybe err $
        uncons $
          normalizeNodes $
            fmap getTxOutNodeInfo $
              Contexts.getContinuingOutputs ctx
      where
        err :: a
        err = traceError "error 'mkInsertValidator' no continuing outputs"

    normalizeNodes = normalizeOrderBy (compare `PlutusPrelude.on` nPrefix)

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

-- * Initializing the Distributed Set

{- | 'DsMint' is the parameter for the minting policy. The
 minting policy realizes the following requirements:

      (1) Creates an NFT (in particular, it creates an 'CurrencySymbol')
      uniquely identifying THIS distributed set

      (2) This 'CurrencySymbol' allows minting of 'AssetClass'es with a
      'TokenName' which corresponds to a prefix of the distributed set to
      reach this branch. Note that 'dsmTxOutRef' is the 'TxOutRef' which is
      consumed initially to create the root of the tree.
-}
newtype DsMint = DsMint
  { dsmTxOutRef :: TxOutRef
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

makeIsDataIndexed ''DsMint [('DsMint, 0)]
deriveJSON defaultOptions ''DsMint
PlutusTx.makeLift ''DsMint

{- | 'mkDsPolicy' needs to verify the following. Either:

      (1) [Growing the Distributed Set] Checks that we are spending an
      'AssetClass' with *this* 'CurrencySymbol', and mints exactly one
      of whatever 'TokenName' it likes.

      (2) [Initializing the Distributed Set] spends the 'TxOutRef' given in
      the parameter.

  Note that the validator script does the heavy lifting to ensure that these
  tokens are minted and transferred in a controlled manner.

  The "use-case"  of this script would be to:

    (1) Mint an initial transaction, and pay to some validator script say
    @script@

    (2) Then, this minting policy will only succeed if there exists some
    @script'@ which has this token AND (by defn. of consensus rules) that
    @script'@ will succeed.

 Note that this minting policy does not guarantee that the @script'@ is the
 same validator as the original @script@! Hence, @script@ needs to be written
 carefully to ensure that this is the case for security.
-}
mkDsPolicy :: DsMint -> () -> ScriptContext -> Bool
mkDsPolicy gds _red ctx =
  traceIfFalse "error 'mkDsPolicy' missing transaction inputs" checkInputs
    && traceIfFalse "error 'mkDsPolicy' must mint exactly one TokenName" checkMintedAmount
  where
    -- Aliases
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

    mint :: Value
    mint = txInfoMint info

    -- Checks:
    checkInputs :: Bool
    checkInputs = flip any (txInfoInputs info) $ \i ->
      -- p1 correponds to (1), and p2 corresponds to (2).
      let p1, p2 :: Bool
          p1 = case fmap AssocMap.toList $ AssocMap.lookup ownCurSymb $ getValue $ txOutValue $ txInInfoResolved i of
            Just ((_, amount) : _) -> amount > 0
            _ -> False
          p2 = txInInfoOutRef i == dsmTxOutRef gds
       in p1 || p2

    checkMintedAmount :: Bool
    checkMintedAmount = flip (maybe False) (fmap AssocMap.toList $ AssocMap.lookup ownCurSymb $ getValue mint) $ \mintedtns ->
      let go :: [(TokenName, Integer)] -> Bool
          go [] = True
          go ((_mt, amount) : mts)
            | amount == 1 = go mts
            | otherwise = False
       in go mintedtns

distributedSetPolicy :: DsMint -> MintingPolicy
distributedSetPolicy dsm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dsm

distributedSetCurSymbol :: DsMint -> CurrencySymbol
distributedSetCurSymbol = Contexts.scriptCurrencySymbol . distributedSetPolicy

{-
{- Note [Comparison to Stick Breaking Set]
    This was based off of the [Stick Breaking
    Set](https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md)
    with some modifications of course. We'll first recall the data type for the nodes of
    the Stick Breaking Set and note some comparisions:

    Recall that the nodes in the Stick Breaking Set are defined as follows.

    > data Node = Node { prefix :: BuiltinByteString, leaves :: [BuiltinByteString], branches :: [BuiltinByteString]}

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
        Note [Adversary Proof of Work Attack]) to insert things in the set with
        common prefixes that differ all differ in a bit of the same index.

        (2) Insertion code is a bit complex and requires scanning through all the
        leaves and branches calculating the longest common prefix.
        While this is upper bounded by by the size of the alphabet (recall this
        is @2^8@), [other data
        structures](https://github.com/Plutonomicon/plutonomicon/blob/main/assoc.md)
        boast an honest to goodness constant time insertion (provided you know
        where to insert it).

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
    Proof of Work Attack]) can actually exploit this and essentialyl "burn"
    other people's tokens forever (since they can never spend this transaction
    as it'll run over the budget).

    But the Stick Breaking Set had good ideas nonetheless --  a great place to
    start :D.
-}

{- Note [Adversary Proof of Work Attack]

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

{-
Note [Maximum Transaction Size Estimate]

Here's an estimate of an upper bound the transaction size for our implementation:

Each output is about
> txOutOverHead := (7 * 32) bytes
since the 'ScriptContext' must include:

 - 'Address' (can be around 2 * 32 bytes)

 - 'CurrencySymbol'

 - 'TokenName'

 - 'DatumHash' (includes this twice: once in the 'TxOut' and the other to
 associate the datum with the hash)

And we'll add (+1) just for good measure when counting overhead for
constructors..

In the worst case, the original input (and output) is going to be
> txOutOverHead + 32 + 2^8 + (2^8 * 1)  bytes
since it must include

 - 'Address'

 - 'CurrencySymbol'

 - 'TokenName'

 - edges from every character (we include this quantity twice because of
 extra overhead of the list constructors, and we assume that a character
 takes 1 byte)

 - Datum hash twice

So in the worst case, when we insert a new string we have a new match which yields

> 2 * (txOutOverHead + 32 + 2^8 + 2^8*1) + 5 * txOutOverHead = 2656 bytes

where we note that routine inspection of the insertion code shows that we can
insert generating less than 5 nodes.

This is well under the 16384B transaction size limit..
-}

{-
 Remark: for testing the size the scripts, it's useful to have the following
 wrapper function for seralizing the script.
> import Ledger qualified as Plutus
> import Data.ByteString.Char8 qualified as BS8
> import Data.ByteString.Lazy qualified as LBS
> import Data.ByteString.Short qualified as SBS
> import Codec.Serialise (serialise)
> import Cardano.Api
> import Cardano.Api.Shelley (Address (..), PlutusScript (..))
>
> writeValidator :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
> writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getValidator
>
> writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
> writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

> writeValidator  "serializedScript" $ DS.insertValidator  (DS.Ds "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
> writeMintingPolicy  "serializedMinting" $ DS.distributedSetPolicy   (DS.DsMint  $ TxOutRef (Plutus.V1.Ledger.Api.TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") 0)
-}
-}
