{-# LANGUAGE BangPatterns #-}
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
import Plutus.V1.Ledger.Address qualified as Address
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef,
 )
import Plutus.V1.Ledger.Scripts (
  Datum (getDatum),
  ValidatorHash,
 )
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName (TokenName, unTokenName),
  Value (getValue),
 )
import PlutusPrelude qualified
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import Prelude qualified

{- | 'hash' is an internal function to store the hash of the to put in the set.
 This is just a thin wrapper around the 'Builtins.blake2b_256' hash function

 TODO: We need to actually hash things in the script still.
-}
{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> BuiltinByteString
hash = Builtins.blake2b_256

-- * Data types

{- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
 for the distributed set. (See Note [Data Structure Definition] and Note [Node
 Representation])
-}
newtype Ds = Ds
  { -- | 'dsConfCurrencySymbol' is the 'CurrencySymbol' which identifies a utxo
    -- with 'DsConfDatum'.
    dsConf :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

{- | 'DSDatum' is the datum in the distributed set. See: Note [Data Structure
 - Definition] and Note [Node Representation].
-}
data DsDatum = DsDatum
  { dsBreak :: Integer
  , dsZero :: Edge
  , dsOne :: Edge
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq DsDatum where
  {-# INLINEABLE (==) #-}
  a == b =
    dsBreak a == dsBreak b
      && dsZero a == dsZero b
      && dsOne a == dsOne b

{- | Prefix break (abbr. 'PBr') is an internal type alias for integers which
 are a prefix break. See 'lcp' for more details.
 Internally this has the invariant that it's in the range @[0,7)@
-}
type PBr = Integer

type Byte = Integer

{- | 'Edge' represents an edge in the graph. See: Note [Data Structure
 - Definition] and Note [Node Representation].
-}
data Edge
  = -- | @'Branch' bs pre@ is an edge @bs@ with @pr@ as the prefix length
    Bin BuiltinByteString PBr
  | -- | 'Tip' denotes a null edge
    Tip
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq Edge where
  Tip == Tip = True
  Bin bs0 br0 == Bin bs1 br1 = bs0 == bs1 && br0 == br1
  _ == _ = False

{- | 'Node' is an internal data type of the tree node used in the validator.
 See: Note [Data Structure Definition] and Note [Node Representation].
-}
data Node = Node
  { nPrefix :: BuiltinByteString
  , nBreak :: PBr
  , nZero :: Edge
  , nOne :: Edge
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b =
    nPrefix a == nPrefix b
      && nBreak a == nBreak b
      && nZero a == nZero b
      && nOne a == nOne b

{- | 'Ib' (abbr. "insert buffer") is the output of 'insertNode' which indicates
 the number of new nodes. In particular, 'insertNode' may either create 2 or
 3 new nodes.
-}
data Ib a
  = One a
  | Two a a
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic, Prelude.Foldable)

data PSuf a
  = LsbZero {pSuf :: a}
  | LsbOne {pSuf :: a}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Functor PSuf where
  fmap f = \case
    LsbZero a -> LsbZero (f a)
    LsbOne a -> LsbOne (f a)

instance Eq a => Eq (Ib a) where
  One a == One a' = a == a'
  Two a b == Two a' b' = a == a' && b == b'
  _ == _ = False

instance Functor Ib where
  fmap f = \case
    One a -> One (f a)
    Two a b -> Two (f a) (f b)

{- | 'DsConf' is used for the 'DatumType' and 'RedeemerType' for the utxo which
 holds the 'DsConfDatum'
-}
data DsConf

{- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
 minting policies needed by the distributed set.
-}
data DsConfDatum = DsConfDatum
  { dscPrefixPolicy :: CurrencySymbol
  , dscLeafPolicy :: CurrencySymbol
  , dscFUELPolicy :: CurrencySymbol
  }

{- | 'unsafeGetDatum' gets the datum sitting at a 'TxOut' and throws an error
 otherwise.
 TODO: Using inline datum is probably a good idea for this.
-}
unsafeGetDatum :: PlutusTx.UnsafeFromData a => TxInfo -> TxOut -> a
unsafeGetDatum info o
  | Just dhash <- txOutDatumHash o
    , Just bn <- Contexts.findDatum dhash info =
    PlutusTx.unsafeFromBuiltinData (getDatum bn)
  | otherwise = traceError "error 'unsafeGetDatum' failed"

{- | 'getConf' gets the config associated with a distributed set and throws an
 error if it does not exist.
-}
getConf :: CurrencySymbol -> TxInfo -> DsConfDatum
getConf currencySymbol info = go $ txInfoInputs info
  where
    -- TODO: this should be changed to a reference input
    go :: [TxInInfo] -> DsConfDatum
    go (t : ts) =
      case txInInfoResolved t of
        o -> case AssocMap.lookup currencySymbol $ getValue $ txOutValue o of
          Just _ -> unsafeGetDatum info o
          Nothing -> go ts
    go [] = traceError "error 'getConf' missing conf"

{- | 'DsConfMint' is the parameter for the NFT to initialize the distributed set.
 Note that 'ConfPolicy' allows one to mint a family of distinct NFTs
 from a single 'TxOutRef'.

 See 'mkDsConfPolicy' for more details.
-}
newtype DsConfMint = DsConfMint {dscmTxOutRef :: TxOutRef}

{- | 'DsMint' is the parameter for the minting policy.
 See Note [Node Representation] for more details.
-}
data DsMint = DsMint
  { -- | 'dsmValidatorHash' is the validator hash that the minting policy
    -- essentially "forwards" its checks to the validator.
    --
    --
    -- TODO: as an optimization, we can take the 'Address' as a parameter
    -- instead (since the offchain code will always immediately convert this
    -- into an 'Address').
    dsmValidatorHash :: ValidatorHash
  , -- | 'dsmConf' is the currency symbol to identify a utxo with 'DsConfDatum'
    dsmConf :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

makeIsDataIndexed ''Ds [('Ds, 0)]
deriveJSON defaultOptions ''Ds
PlutusTx.makeLift ''Ds

makeIsDataIndexed ''DsDatum [('DsDatum, 0)]
deriveJSON defaultOptions ''DsDatum

makeIsDataIndexed ''Edge [('Tip, 0), ('Bin, 1)]
deriveJSON defaultOptions ''Edge

makeIsDataIndexed ''Ib [('One, 0), ('Two, 1)]
deriveJSON defaultOptions ''Ib

makeIsDataIndexed ''PSuf [('LsbZero, 0), ('LsbOne, 1)]
deriveJSON defaultOptions ''PSuf

makeIsDataIndexed ''Node [('Node, 0)]
deriveJSON defaultOptions ''Node

makeIsDataIndexed ''DsMint [('DsMint, 0)]
deriveJSON defaultOptions ''DsMint
PlutusTx.makeLift ''DsMint

makeIsDataIndexed ''DsConfMint [('DsConfMint, 0)]
deriveJSON defaultOptions ''DsConfMint
PlutusTx.makeLift ''DsConfMint

makeIsDataIndexed ''DsConfDatum [('DsConfDatum, 0)]
deriveJSON defaultOptions ''DsConfDatum
PlutusTx.makeLift ''DsConfDatum

-- * Standard helper functions

{- | @'exps0To7' i@ is an internal function to which does computes @2^i@ for @0
 <= i < 7@.
-}
{-# INLINEABLE exps0To7 #-}
exps0To7 :: Integer -> Integer
-- For some reason, string literals don't seem to compile with Plutus, so we
-- can't do a lookup table as the following implementation:
-- > exps0To7 i = i `Builtins.indexByteString` "\001\002\004\008\016\032\064\128"
--
-- For now, we do a binary search which implements the mapping
-- > exps0To7 i
-- >    | i == 0 = 1
-- >    | i == 1 = 2
-- >    | i == 2 = 4
-- >    | i == 3 = 8
-- >    | i == 4 = 16
-- >    | i == 5 = 32
-- >    | i == 6 = 64
-- >    | i == 7 = 128
-- >    | otherwise = traceError "error 'exps0To7' non-exhaustive."
exps0To7 i
  | i < 4 = if i < 2 then if i < 1 then 1 else 2 else if i < 3 then 4 else 8
  | i < 6 = if i < 5 then 16 else 32
  | i < 7 = 64
  | otherwise = 128

{-# INLINEABLE testBit #-}
testBit :: Byte -> Integer -> Bool
testBit w ix = (w `Builtins.quotientInteger` exps0To7 ix) `Builtins.remainderInteger` 2 == 1

{-# INLINEABLE nullByteString #-}
nullByteString :: BuiltinByteString -> Bool
nullByteString bs = 0 == lengthOfByteString bs

{-# INLINEABLE initByteString #-}
initByteString :: BuiltinByteString -> BuiltinByteString
initByteString bs =
  (\len -> if len == 0 then traceError "error 'initByteString' empty" else sliceByteString 0 (len - 1) bs) $
    lengthOfByteString bs

{-# INLINEABLE singletonBytestring #-}
singletonBytestring :: Byte -> BuiltinByteString
singletonBytestring b = consByteString b emptyByteString

{-# INLINEABLE lastByteString #-}
lastByteString :: BuiltinByteString -> Byte
lastByteString bs =
  (\len -> if len == 0 then traceError "error 'lastByteString' empty" else indexByteString bs (len - 1)) $
    lengthOfByteString bs

{-# INLINEABLE headByteString #-}
headByteString :: BuiltinByteString -> Byte
headByteString bs =
  (\len -> if len == 0 then traceError "error 'headByteString' empty" else indexByteString bs 0) $
    lengthOfByteString bs

{-# INLINEABLE tailByteString #-}
tailByteString :: BuiltinByteString -> BuiltinByteString
tailByteString bs =
  (\len -> if len == 0 then traceError "error 'initByteString' empty" else sliceByteString 1 len bs) $
    lengthOfByteString bs

-- | 'snocByteString' appends the character to the end of the string.
{-# INLINEABLE snocByteString #-}
snocByteString :: BuiltinByteString -> Integer -> BuiltinByteString
snocByteString str b = appendByteString str (Builtins.consByteString b emptyByteString)

-- * Helper functions specific to this module

-- | @'lcpAppend` p br s@
{-# INLINEABLE lcpAppend #-}
lcpAppend :: BuiltinByteString -> PBr -> BuiltinByteString -> BuiltinByteString
lcpAppend p br s
  | br == 0 = p `appendByteString` s
  | otherwise = initByteString p `appendByteString` singletonBytestring (lastByteString p + headByteString s) `appendByteString` tailByteString s

-- | 'distinctLsb' returns true if the LSB of the arguments are distinct.
{-# INLINEABLE distinctLsb #-}
distinctLsb :: Byte -> Byte -> Bool
distinctLsb l r = l `Builtins.remainderInteger` 2 + r `Builtins.remainderInteger` 2 == 1

{- | @'lcpByte' l r@ returns common prefix of @l@ and @r@, the length of the
 common prefix (which is the index of the start of the suffix), the
 respective suffixes in a tuple if @l@ and @r@ are distinct.

 If @l@ and @r@ are not distinct, the behavior is undefined.
-}
{-# INLINEABLE lcpByte #-}
lcpByte :: Byte -> Byte -> (Byte, Integer, Byte, Byte)
lcpByte = go 0 0
  where
    go :: Byte -> Integer -> Byte -> Byte -> (Byte, Integer, Byte, Byte)
    go !accW !ix !l !r
      | not (distinctLsb l r) =
        go (accW + exps0To7 ix * (l `Builtins.remainderInteger` 2)) (ix + 1) (l `Builtins.quotientInteger` 2) (r `Builtins.quotientInteger` 2)
      | otherwise =
        (accW, ix, l * exps0To7 ix, r * exps0To7 ix)

-- @'lcp' s t@ returns the longest common prefix with the last byte having
-- everything outside the length (see next point) cleared, the length of the last byte
-- (between 0 and 7 inclusive) of the prefix (which is also the starting index
-- of the suffix), and the respective suffixes with the first byte having bits
-- [0, length of the last byte of the prefix) cleared.
--
-- Note that if the length of the last byte is 0, then the prefix and suffixes
-- have no bits cleared.
{-# INLINEABLE lcp #-}
lcp :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, Integer, BuiltinByteString, BuiltinByteString)
lcp s t = go 0
  where
    go :: Integer -> (BuiltinByteString, Integer, BuiltinByteString, BuiltinByteString)
    go !ix
      | ix >= lengthOfByteString s || ix >= lengthOfByteString t = aligned
      | otherwise =
        ( \si ti ->
            if si == ti
              then go (ix + 1)
              else case lcpByte si ti of
                (lp, lix, ssi, sti)
                  | lix == 0 -> aligned
                  | otherwise ->
                    ( takeByteString ix s `snocByteString` lp
                    , -- we take everything up to (but not including)
                      -- the distinct byte, and snoc the masked
                      -- distinct byte at the end
                      lix
                    , consByteString ssi $ dropByteString (ix + 1) s
                    , consByteString sti $ dropByteString (ix + 1) t
                    -- for the suffixes, we drop everything
                    -- (including the distinct byte), then prepend
                    -- the masked distinct byte to the front
                    --
                    -- note that the bits @[0, lix - 1] = [0, lix)@
                    -- are cleared.
                    )
        )
          (s `Builtins.indexByteString` ix)
          (t `Builtins.indexByteString` ix)
      where
        aligned =
          ( takeByteString ix s
          , 0
          , dropByteString ix s
          , dropByteString ix t
          )

{- | @'prefixOf' p br bs@

 Preconditions:

  - @bs@ is non empty

  - @0 <= br < 8@
-}
{-# INLINEABLE prefixOf #-}
prefixOf :: BuiltinByteString -> PBr -> BuiltinByteString -> Maybe (PSuf BuiltinByteString)
prefixOf p br bs
  | nullByteString p = return aligned
  | pLen <= bLen =
    if br == 0
      then
        PlutusPrelude.guard (p == takeByteString (lengthOfByteString p) bs)
          PlutusPrelude.$> aligned
      else
        ( \pebs l r ->
            if initByteString p == pebs
              then
                let go !ix !pb !bb
                      | ix < br =
                        if distinctLsb pb bb
                          then Nothing
                          else go (ix + 1) (pb `Builtins.quotientInteger` 2) (bb `Builtins.quotientInteger` 2)
                      | otherwise =
                        return $
                          if bb `Builtins.remainderInteger` 2 == 0
                            then LsbZero (bb * exps0To7 ix)
                            else LsbOne (bb * exps0To7 ix)
                 in case go 0 l r of
                      Just res ->
                        let k nsb = consByteString nsb $ dropByteString pLen bs
                         in case res of
                              LsbZero a -> Just (LsbZero (k a))
                              LsbOne a -> Just (LsbOne (k a))
                      Nothing -> Nothing
              else -- fmap (fmap (\nsb -> consByteString nsb $ dropByteString pLen bs )) $
              -- Maybe functor, PSuf functor
                Nothing
        )
          (takeByteString (pLen - 1) bs)
          (lastByteString p)
          (bs `indexByteString` (pLen - 1))
  | otherwise = Nothing
  where
    bLen = lengthOfByteString bs
    pLen = lengthOfByteString p

    aligned
      | headByteString bs `Builtins.remainderInteger` 2 == 0 = LsbZero (dropByteString pLen bs)
      | otherwise = LsbOne (dropByteString pLen bs)

-- * Helper functions related to 'Ib'

{-# INLINEABLE lengthIb #-}
lengthIb :: Ib a -> Integer
lengthIb =
  \case
    One _ -> 1
    Two _ _ -> 2

{- | 'fromListIb' converts a list into an 'Ib'. N.B. this function is partial
 and throws an error in the case that the list does not have 1 or 2 elements.
-}
{-# INLINEABLE fromListIb #-}
fromListIb :: [a] -> Ib a
fromListIb = \case
  [a] -> One a
  [a, b] -> Two a b
  _ -> traceError "error 'fromListIb' invalid list"

-- | 'toListIb'  converts an 'Ib' into a list (in order)
{-# INLINEABLE toListIb #-}
toListIb :: Ib a -> [a]
toListIb = \case
  Two a b -> [a, b]
  One a -> [a]

-- * Node related functions

-- | 'mkNode' is a wrapper to create a Node from a prefix and the datum.
{-# INLINEABLE mkNode #-}
mkNode :: BuiltinByteString -> DsDatum -> Node
mkNode str d =
  Node
    { nPrefix = str
    , nBreak = dsBreak d
    , nZero = dsZero d
    , nOne = dsOne d
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
    { nPrefix = emptyByteString
    , nBreak = 0
    , nZero = Tip
    , nOne = Tip
    }

{- | @'nextNodePrefix' str node@ finds the prefix of the next node (provided it
 exists) of @str@ after following the edges of @node@.

 Preconditions (you must check these yourself)

      - @prefix node `isPrefixOfByteString` str@
-}
nextNodePrefix :: BuiltinByteString -> Node -> Maybe (BuiltinByteString, PBr)
nextNodePrefix str node
  | nullByteString str = Nothing
  | otherwise =
    prefixOf (nPrefix node) (nBreak node) str >>= \case
      LsbZero pStrSuf -> bin pStrSuf $ nZero node
      LsbOne pStrSuf -> bin pStrSuf $ nOne node
  where
    bin :: BuiltinByteString -> Edge -> Maybe (BuiltinByteString, PBr)
    bin strSuf = \case
      Tip -> Nothing
      Bin suf sbr ->
        let nbs = lcpAppend (nPrefix node) (nBreak node) suf
         in prefixOf suf sbr strSuf
              >> PlutusPrelude.guard (not (nbs == str && sbr == 0))
              PlutusPrelude.$> (nbs, sbr)

-- @'insertNode' str node@ returns the nodes which should replace @node@ in
-- order to insert @str@ in the tree.
--
-- Preconditions:
--
--      - @'nextNodePrefix' str node@ must be 'Nothing'.
--
-- This is mostly just tedious case analysis.
{-# INLINEABLE insertNode #-}
insertNode :: BuiltinByteString -> Node -> Ib Node
insertNode str node
  | Just psuf <- prefixOf (nPrefix node) (nBreak node) str
    , suf' <- pSuf psuf =
    let bin :: BuiltinByteString -> PBr -> (Edge, Node)
        bin suf sbr = case lcp suf suf' of
          (nsuf, nbr, ssuf, ssuf') -> case if testBit (headByteString ssuf) nbr
            then ((ssuf', 0), (ssuf, sbr))
            else ((ssuf, sbr), (ssuf', 0)) of
            (zb, ob) ->
              ( Bin nsuf nbr
              , Node
                  { nPrefix = lcpAppend (nPrefix node) (nBreak node) nsuf
                  , nBreak = nbr
                  , nZero = uncurry Bin zb
                  , nOne = uncurry Bin ob
                  }
              )
     in case psuf of
          LsbZero _
            | Tip <- nZero node -> One node {nZero = Bin suf' 0}
            | Bin suf sbr <- nZero node
              , isNothing (prefixOf suf sbr suf') -> case bin suf sbr of
              (ne, nnode) -> Two node {nZero = ne} nnode
          LsbOne _
            | Tip <- nOne node -> One node {nOne = Bin suf' 0}
            | Bin suf sbr <- nOne node
              , isNothing (prefixOf suf sbr suf') -> case bin suf sbr of
              (ne, nnode) -> Two node {nOne = ne} nnode
          _ -> traceError err
  | otherwise = traceError err
  where
    err = "error 'insertNode' bad insertion"
-- * Validators for insertion in the distributed set

{- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
 done in the 'insertNode' function.
-}
{-# INLINEABLE mkInsertValidator #-}
mkInsertValidator :: Ds -> DsDatum -> () -> ScriptContext -> Bool
mkInsertValidator ds _dat _red ctx =
  ( \nStr ->
      ( \(!nNodes) ->
          -- the total number tokens which are prefixes is @1 + the number of
          -- minted tokens@ since we know that there is only one input with this
          -- token.
          --
          -- In erroneous cases, we return @-1@ which will always be @False@ in the
          -- above predicate
          let totalPrefixes :: Integer
              totalPrefixes = case AssocMap.lookup prefixCurrencySymbol minted of
                Nothing -> 1
                Just mp
                  | [(_, amt)] <- AssocMap.toList mp
                    , amt == 1 ->
                    2
                _ -> -1
           in traceIfFalse
                "error 'mkInsertValidator' bad insertion"
                ( contNodes == nNodes
                    -- The following check:
                    && totalPrefixes == lengthIb nNodes
                    -- takes @11 075 011 - 5 886 715 = 5 188 296@ mem execution units
                    -- (testing this with and without this one extra check) of the total
                    -- 10 000 000 mem execution units.
                    --
                    -- I'm fairly certain this isn't getting compiled properly and
                    -- it's recomputing @nNodes@ twice....
                )
                -- TODO: the following check should be moved to the leaf minting policy...
                -- > && traceIfFalse "error 'mkInsertValidator' bad leaf policy" (AssocMap.member (dscFUELPolicy conf) minted)
      )
        (insertNode nStr ownNode)
  )
    ( case AssocMap.lookup leafCurrencySymbol minted of
        Just mp
          | [(leaf, _amt)] <- AssocMap.toList mp ->
            -- TODO: the following check is completely optional actually..
            -- > , _amt == 1
            unTokenName leaf
        _ -> traceError "error 'mkInsertValidator' missing unique string to insert"
    )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    !ownInput = case Contexts.findOwnInput ctx of
      Just i -> i
      Nothing -> traceError "error 'mkInsertValidator': ownInput failed"

    minted :: Map CurrencySymbol (Map TokenName Integer)
    !minted = getValue (txInfoMint info)

    conf :: DsConfDatum
    !conf = getConf (dsConf ds) info

    prefixCurrencySymbol :: CurrencySymbol
    prefixCurrencySymbol = dscPrefixPolicy conf

    leafCurrencySymbol :: CurrencySymbol
    leafCurrencySymbol = dscLeafPolicy conf

    -- Given a value, gets the token name (of the "continuing" currency symbol)
    getPrefixTn :: Value -> TokenName
    getPrefixTn v
      | Just tns <- AssocMap.lookup prefixCurrencySymbol $ getValue v
        , [(tn, amt)] <- AssocMap.toList tns
        , amt == 1 =
        tn
    getPrefixTn _ = traceError "error 'mkInsertValidator': 'getPrefixTn' failed"

    -- Given a TxOut, this will get (and check) if we have the 'TokenName' and
    -- required datum.
    getTxOutNodeInfo :: TxOut -> Node
    getTxOutNodeInfo o = mkNode (unTokenName $ getPrefixTn $ txOutValue o) $ unsafeGetDatum info o

    ownNode :: Node
    ownNode = getTxOutNodeInfo $ txInInfoResolved ownInput

    normalizeIbNodes :: Ib Node -> Ib Node
    normalizeIbNodes = \case
      One a -> One a
      Two a b
        | nPrefix a < nPrefix b -> Two a b
        | otherwise -> Two b a

    contNodes :: Ib Node
    contNodes =
      normalizeIbNodes $
        fromListIb $
          ( \ownAddr ->
              let go :: [TxOut] -> [Node]
                  go [] = []
                  go (t : ts)
                    | txOutAddress t == ownAddr = getTxOutNodeInfo t : go ts
                    | otherwise = go ts
               in go (txInfoOutputs info)
          )
            $ txOutAddress (txInInfoResolved ownInput)

instance ValidatorTypes Ds where
  type DatumType Ds = DsDatum
  type RedeemerType Ds = ()

-- | The typed validator script for the distributed set.
typedInsertValidator :: Ds -> TypedValidator Ds
typedInsertValidator ds =
  Scripts.mkTypedValidator @Ds
    ( $$(PlutusTx.compile [||mkInsertValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode ds
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DsDatum @()

-- | The regular validator script for the distributed set.
insertValidator :: Ds -> Validator
insertValidator = Scripts.validatorScript . typedInsertValidator

-- | The validator hash for the distributed set.
insertValidatorHash :: Ds -> ValidatorHash
insertValidatorHash = Scripts.validatorHash . typedInsertValidator

-- | The address for the distributed set.
insertAddress :: Ds -> Address
insertAddress = Scripts.validatorAddress . typedInsertValidator

{- | 'mkDsConfValidator' is the script for which 'DsConfDatum' will be sitting
 at.
 mkDsConfValidator :: Ds -> DsConfDatum -> () -> ScriptContext -> Bool
-}
mkDsConfValidator :: Ds -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfValidator _ds _dat _red _ctx = ()

-- TODO: when we get reference inputs, we need to change this to the above line
-- of code to the following line of code
-- > mkDsConfValidator _ds dat _red _ctx = Builtins.error ()

-- | The regular validator script for the conf. of the distributed set.
dsConfValidator :: Ds -> Validator
dsConfValidator ds =
  Scripts.mkValidatorScript
    ( $$(PlutusTx.compile [||mkDsConfValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode ds
    )

{- | The validator hash for the conf. of the distributed set.

 TODO: do this properly by fetching the right package...
-}
dsConfValidatorHash :: Ds -> ValidatorHash
dsConfValidatorHash = Scripts.validatorHash . Scripts.unsafeMkTypedValidator . dsConfValidator

{- | The address for the conf. of the distributed set.

 TODO: do this properly by fetching the right package...
-}
dsConfAddress :: Ds -> Address
dsConfAddress = Scripts.validatorAddress . Scripts.unsafeMkTypedValidator . dsConfValidator

-- * Minting Policy for Initializing the Distributed Set

{- | 'mkDsConfPolicy' mints the nft which identifies the validator that stores
 the various minting policies that the distributed set depends on
-}
mkDsConfPolicy :: DsConfMint -> () -> ScriptContext -> Bool
mkDsConfPolicy dsc _red ctx =
  traceIfFalse "error 'mkDsConfPolicy' missing TxOutRef" spendsTxOutRef
    && traceIfFalse "error 'mkDsConfPolicy' illegal mint" mintingChecks
  where
    -- Aliases
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

    -- Checks
    spendsTxOutRef :: Bool
    spendsTxOutRef = isJust $ Contexts.findTxInByTxOutRef (dscmTxOutRef dsc) info

    mintingChecks :: Bool
    mintingChecks
      | Just tns <- AssocMap.lookup ownCurSymb $ getValue (txInfoMint info)
        , [(tn, amt)] <- AssocMap.toList tns
        , tn == dsConfTokenName
        , amt == 1 =
        True
      | otherwise = False

-- | 'dsConfTokenName' is the token name of the
dsConfTokenName :: TokenName
dsConfTokenName = TokenName emptyByteString

-- | 'dsConfPolicy' is the minting policy
dsConfPolicy :: DsConfMint -> MintingPolicy
dsConfPolicy dscm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsConfPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dscm

-- | 'dsConfCurSymbol' is the currency symbol for the distributed set
dsConfCurSymbol :: DsConfMint -> CurrencySymbol
dsConfCurSymbol = Contexts.scriptCurrencySymbol . dsConfPolicy

{- | 'mkDsLeafPolicy' provides evidence for the leaf that was just added. Note
 this simply forwards all checks to the validator.
-}
mkDsLeafPolicy :: DsMint -> () -> ScriptContext -> Bool
mkDsLeafPolicy dsm _red ctx =
  traceIfFalse "error 'mkDsPrefixPolicy' no validator found" $
    any go $ txInfoInputs $ scriptContextTxInfo ctx
  where
    go :: TxInInfo -> Bool
    go txin = txOutAddress (txInInfoResolved txin) == Address.scriptHashAddress (dsmValidatorHash dsm)

-- | 'mkDsPrefixPolicy' needs to verify the following.  See Note [Node Representation].
mkDsPrefixPolicy :: DsMint -> () -> ScriptContext -> Bool
mkDsPrefixPolicy _ _ _ = True

{-
mkDsPrefixPolicy dsm _red ctx = case ins of
    [_ownTn] -> True
    []
        |  -- If we are minting the NFT which configures everything, then we
           -- should mint only the empty prefix
         Just _ <- AssocMap.lookup (dsmConf dsm) $ getValue $ txInfoMint info
         -> case mintedTns of
            [tn] | unTokenName tn == nPrefix rootNode ->
                traceIfFalse "error 'mkDsPrefixPolicy' illegal outputs" $
                    case find (\txout -> txOutAddress txout == Address.scriptHashAddress (dsmValidatorHash dsm)) $ txInfoOutputs info of
                        Just txout -> isJust $ AssocMap.lookup ownCurrencySymbol $ getValue $ txOutValue txout
                        Nothing -> False
                -- TODO: check in the script output with the output that this
                -- ownCurrencySymbol is actually where it should be.. Actually,
                -- there's no need to do this -- we can verify this offchain
                -- since we may assume that all participants know the protocol
                -- ahead of time and may independently verify.
            _ ->  traceError "error 'mkDsPrefixPolicy' bad initial mint"
    _ -> traceError "error 'mkDsPrefixPolicy' bad inputs in transaction"
  where
    -- Aliases
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCurrencySymbol :: CurrencySymbol
    ownCurrencySymbol = Contexts.ownCurrencySymbol ctx

    -- determines the branches that we are consuming
    ins :: [TokenName]
    ins =
      let go [] = []
          go (t : ts)
            | txout <- txInInfoResolved t
              , txOutAddress txout == Address.scriptHashAddress (dsmValidatorHash dsm)
              , Just tns <- AssocMap.lookup ownCurrencySymbol $ getValue $ txOutValue txout
              , -- If it's more clear, we're checking the following condition:
                -- > [(tn,1)] <- AssocMap.toList tns
                -- In our case, it is implicit that there is exactly one
                -- TokenName and that there will be only one distinct TokenName.
               (tn,_amt) : _ <- AssocMap.toList tns
                = tn : go ts
            -- Need to keep recursing to ensure that this transaction
            -- is only spending one input
            | otherwise = go ts -- otherwise, we skip the element
       in go $ txInfoInputs info

    mintedTns :: [TokenName]
    mintedTns =
      case AssocMap.lookup ownCurrencySymbol $ getValue $ txInfoMint info of
        Just mp
          | vs <- AssocMap.toList mp
            , all ((== 1) . snd) vs ->
            map fst vs
        _ -> traceError "error 'mkDsPrefixPolicy': bad minted tokens"
    -}

-- | 'dsPrefixPolicy' is the minting policy for the prefixes of nodes
dsPrefixPolicy :: DsMint -> MintingPolicy
dsPrefixPolicy dsm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsPrefixPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dsm

{- | 'dsPrefixCurSymbol' is the currency symbol for prefixes of nodes in the
 distributed set
-}
dsPrefixCurSymbol :: DsMint -> CurrencySymbol
dsPrefixCurSymbol = Contexts.scriptCurrencySymbol . dsPrefixPolicy

-- | 'dsLeafPolicy' is the minting policy for the leaves just minted
dsLeafPolicy :: DsMint -> MintingPolicy
dsLeafPolicy dsm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsLeafPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dsm

{- | 'dsLeafCurSymbol' is the currency symbol for prefixes of nodes in the
 distributed set
-}
dsLeafCurSymbol :: DsMint -> CurrencySymbol
dsLeafCurSymbol = Contexts.scriptCurrencySymbol . dsLeafPolicy

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

    So what does this suggest? This isn't the right way to go. Indeed, this is
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
