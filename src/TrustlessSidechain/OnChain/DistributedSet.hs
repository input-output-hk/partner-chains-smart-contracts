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
import PlutusTx.Prelude
import Prelude qualified

-- * Data types

{- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
 for the distributed set. (See Note [How This All Works].
-}
newtype Ds = Ds
  { -- | 'dsConf' is the 'CurrencySymbol' which identifies the utxo
    -- with 'DsConfDatum'.
    dsConf :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

-- | 'DsDatum' is the datum in the distributed set. See: Note [How This All Works]
newtype DsDatum = DsDatum
  { dsNext :: BuiltinByteString
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving newtype (Eq)

{- | 'Node' is an internal data type of the tree node used in the validator.
 See: Note [How This All Works].
-}
data Node = Node
  { nKey :: BuiltinByteString
  , nNext :: BuiltinByteString
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b = nKey a == nKey b && nNext a == nNext b

{- | 'DsConf' is used for the 'DatumType' and 'RedeemerType' for the utxo which
 holds the 'DsConfDatum'
-}
data DsConf

{- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
 minting policies needed by the distributed set.
-}
data DsConfDatum = DsConfDatum
  { dscKeyPolicy :: CurrencySymbol
  , dscFUELPolicy :: CurrencySymbol
  }

{- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
 length "array" of how many new nodes (this is always 2, see 'lengthIb') are
 generated after inserting a into a node.
-}
data Ib a = Ib a a
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq a => Eq (Ib a) where
  {-# INLINEABLE (==) #-}
  Ib s t == Ib s' t' = s == s' && t == t'

instance Prelude.Foldable Ib where
  foldMap f (Ib a b) = f a Prelude.<> f b

{- | 'DsConfMint' is the parameter for the NFT to initialize the distributed
 set. See 'mkDsConfPolicy' for more details.
-}
newtype DsConfMint = DsConfMint {dscmTxOutRef :: TxOutRef}

{- | 'DsMint' is the parameter for the minting policy.
 See Note [How This All Works] for more details.
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

{- | 'unsafeGetDatum' gets the datum sitting at a 'TxOut' and throws an error
 otherwise.
 TODO: Using inline datum is probably a good idea for this.
-}
{-# INLINEABLE unsafeGetDatum #-}
unsafeGetDatum :: PlutusTx.UnsafeFromData a => TxInfo -> TxOut -> a
unsafeGetDatum info o
  | Just dhash <- txOutDatumHash o
    , Just bn <- Contexts.findDatum dhash info =
    PlutusTx.unsafeFromBuiltinData (getDatum bn)
  | otherwise = traceError "error 'unsafeGetDatum' failed"

{- | 'getConf' gets the config associated with a distributed set and throws an
 error if it does not exist.
-}
{-# INLINEABLE getConf #-}
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

makeIsDataIndexed ''Ds [('Ds, 0)]
deriveJSON defaultOptions ''Ds
PlutusTx.makeLift ''Ds

makeIsDataIndexed ''DsDatum [('DsDatum, 0)]
deriveJSON defaultOptions ''DsDatum

makeIsDataIndexed ''Node [('Node, 0)]
deriveJSON defaultOptions ''Node

makeIsDataIndexed ''DsMint [('DsMint, 0)]
deriveJSON defaultOptions ''DsMint
PlutusTx.makeLift ''DsMint

makeIsDataIndexed ''DsConfMint [('DsConfMint, 0)]
deriveJSON defaultOptions ''DsConfMint
PlutusTx.makeLift ''DsConfMint

makeIsDataIndexed ''Ib [('Ib, 0)]
deriveJSON defaultOptions ''Ib
PlutusTx.makeLift ''Ib

makeIsDataIndexed ''DsConfDatum [('DsConfDatum, 0)]
deriveJSON defaultOptions ''DsConfDatum
PlutusTx.makeLift ''DsConfDatum

{- Note [How This all Works]

    We briefly describe how this all works. We want the following operations:

        * A method to prove that a message digest (image of a hash function)
        has never been put in this set before, and

        * A method to insert such a message digest into the set.

     We realize this through an ordered linked list i.e., formally, given a set
     of message digests; we have a directed graph with

        * Nodes: message digests (we call these message digests a /key/)

        * Edges: there is an edge between nodes @a@ and @b@ iff @a < b@ and
        there does not exist a node @k@ such that @a < k@ and @k < b@.

    In code, we represent this graph with the 'Node' type, where 'nKey'
    identifies the node and the edge is represented with 'nEdge'.

    We say that a message digest @str@ is in the set, iff there exists a node
    @str@.

    Claim.
        Let @a@ and @b@ be nodes s.t. there exists an edge @a@ to @b@.
        Then, for every message digest @str@ s.t. @a < str@ and @str < b@,
        @str@ is not in the set.

    Sketch. Follows from the definition of an edge.

    Why do we care about this? This claim asserts that to show that @str@ is
    not already in the set, it suffices to find a node @a@ with an edge to @b@
    satisfying @a < str@ and @str < b@. Note that otherwise this is inconclusive.

    Also, since we are storing message digests (which are a fixed size and
    also finite in size), we know that there exists a lower and upper bound to
    the message digests. In particular, we are interested in storing message
    digests of blake2b_256 so

        * @""@ the empty string is a lower bound

        * @replicate 33 '\255'@ is an upper bound

    This means that we can have a root node as given in 'rootNode'.

    To insert a string @str@ (not already in the set) into the set, we need the
    greatest lower bound of @str@ in the set (think infimum) so pictorially, we
    have
    >
    >   str' ------> str''
    >
    where we necessarily have @str' < str < str''@ from our assumptions.
    Note that the place we wish to insert is unique, so what follows will be a
    well defined mapping.

    Then, we transform this into
    >
    >   str' ---> str ---> str''
    >
    and it's easy to see that the invariants of the graph are still satisfied,
    and by defn. @str@ is included in the set.

    Claim.
        This mapping maintains the required invariants of the graph.

    Claim.
        There is a unique node for an insertion of a string not already in
        the set.

    And really, that's it.. this isn't a super complicated way to do this....
    and probably this has made it overly complicated.

    Now, we discuss how we actually do this in the block chain.. lovely.. We
    store the message digest which identifies the node in a token name of a
    utxo, and we represent the edge (to another node) via storing the token
    name which identifies the other node in the datum of the utxo. It's really
    just that easy.

    Also, I should mention that we have some complexities with the validator
    succeeding iff the minting policy (which identifies the node -- see
    'mkDsKeyPolicy'). To achieve this, we store (at another utxo) a
    'DsConfDatum'  which stores the minting policy hash so that the validator
    can validate iff and the minting policy succeeds; and the minting policy is
    parameterized by the validator hash.

    There's a bit of an annoyance with "how do I initialize this thing," but
    it's easily resolved by minting an initial NFT (which pays to a utxo with
    'DsConfDatum'), which will identifies the 'DsConfDatum'. Then, this utxo
    will be used as a reference input for the validator so the validator knows
    the minting policy hash.

    So initially, given a utxo, we initialize the system as follows (left side
    is transaction inputs, and rightside are the outputs).

    >
    > utxo ---------------> utxo with
    >      ---------------> DsConfDatum
    >      --------------->     hash of minting policy of keys
    >      --------------->     [other hashes of minting policies can be included here as well]
    >      ---------------> utxo which represents the node
    >                           Node { nKey = "", nEdge = "\255...\255"}
    where we recall that @""@ is the lower bound of message digests, and
    @"\255...\255"@ is the upper bound of message digests

    Some notation is needed here. When we say
    > utxo which represents the node
    >   Node { nKey = "", nEdge = "\255...\255"}
    this means that there exists a utxo (sitting at the validator script
    'mkInsertValidator') with

        * Datum as @DsDatum {dsNext  = "\255...\255" }@  (i.e., the @nEdge@
        field)

        * A TokenName (with CurrencySymbol as 'mkDsKeyPolicy') @""@ (i.e., the
        @nKey@ field)

    It's clear how this defn. generalizes to other nodes.

    Then, to insert a string @str@, we follow the procedure above and build
    transactions that look like this

    >
    > utxo which represents:                                      utxo which represents
    > Node                                                          Node
    >   { nKey = str'                           ---consumed---->      { nKey = str'
    >   , nEdge = str'' }                       --------------->      , nEdge = str }
    >                                           --------------->
    >                                           --------------->   utxo which represents
    > utxo with [as a reference input]          --------------->    Node
    >   DsConfDatum                                                   { nKey = str
    >     hash of minting policy of keys                              , nEdge = str'' }
    >     [...]

    Wow! It's hopefully just that easy!
 -}

-- * Node related functions

-- | 'mkNode' is a wrapper to create a Node from a prefix and the datum.
{-# INLINEABLE mkNode #-}
mkNode :: BuiltinByteString -> DsDatum -> Node
mkNode str d =
  Node
    { nKey = str
    , nNext = dsNext d
    }

-- | 'rootNode' is the root node of every distributed set.
{-# INLINEABLE rootNode #-}
rootNode :: Node
rootNode =
  Node
    { nKey = emptyByteString
    , -- this is the lower bound of all values in the set.

      nNext =
        let dbl str = str `appendByteString` str
         in consByteString 255 (dbl (dbl (dbl (dbl (dbl (consByteString 255 emptyByteString))))))
        -- We'd really want to write something like this:
        -- > "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255"
        -- which is a 33 byte long string which is the max value.
        -- In particular, this is the upper bound of all values produced by the
        -- hash.
    }

{- | 'fromListIb lst' converts a list of length 2 into an 'Ib' and throws an
 exception otherwise.
-}
{-# INLINEABLE fromListIb #-}
fromListIb :: [a] -> Ib a
fromListIb = \case
  [a, b] -> Ib a b
  _ -> traceError "error 'fromListIb' bad list"

-- | 'lengthIb' always returns 2.
{-# INLINEABLE lengthIb #-}
lengthIb :: Ib a -> Integer
lengthIb _ = 2

-- * Validators for insertion in the distributed set

{-# INLINEABLE insertNode #-}
insertNode :: BuiltinByteString -> Node -> Maybe (Ib Node)
insertNode str node
  | nKey node < str && str < nNext node =
    Just $
      Ib node {nNext = str} Node {nKey = str, nNext = nNext node}
  | otherwise = Nothing

{- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
 done in the 'insertNode' function.
-}
{-# INLINEABLE mkInsertValidator #-}
mkInsertValidator :: Ds -> DsDatum -> () -> ScriptContext -> Bool
mkInsertValidator ds _dat _red ctx =
  ( \nStr ->
      let ownNode :: Node
          ownNode = getTxOutNodeInfo $ txInInfoResolved ownInput

          nNodes :: Ib Node
          !nNodes = case insertNode nStr ownNode of
            Just x -> x
            Nothing -> traceError "error 'mkInsertValidator' bad insertion"

          contNodes :: Ib Node
          contNodes =
            let normalizeIbNodes (Ib a b)
                  | nKey a < nKey b = Ib a b
                  | otherwise = Ib b a
             in normalizeIbNodes $
                  fromListIb $ case txOutAddress (txInInfoResolved ownInput) of
                    ownAddr ->
                      let go :: [TxOut] -> [Node]
                          go (t : ts)
                            | txOutAddress t == ownAddr = getTxOutNodeInfo t : go ts
                            | otherwise = go ts
                          go [] = []
                       in go (txInfoOutputs info)

          -- the total number tokens which are prefixes is @1 + the number of
          -- minted tokens@ since we know that there is only one input with this
          -- token.
          --
          -- In erroneous cases, we return @-1@ which will always be @False@ in the
          -- above predicate
          totalKeys :: Integer
          totalKeys = case AssocMap.lookup keyCurrencySymbol minted of
            Just mp
              | [(_, amt)] <- AssocMap.toList mp
                , amt == 1 ->
                2
            _ -> -1
       in traceIfFalse "error 'mkInsertValidator' bad insertion" (contNodes == nNodes && totalKeys == lengthIb nNodes)
            && traceIfFalse "error 'mkInsertValidator' missing FUEL mint" (AssocMap.member (dscFUELPolicy conf) minted)
  )
    ( case AssocMap.lookup keyCurrencySymbol minted of
        Just mp
          | [(leaf, amt)] <- AssocMap.toList mp
            , amt == 1 ->
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

    keyCurrencySymbol :: CurrencySymbol
    keyCurrencySymbol = dscKeyPolicy conf

    -- Given a value, gets the token name (of the "continuing" token name)
    getKeyTn :: Value -> BuiltinByteString
    getKeyTn v
      | Just tns <- AssocMap.lookup keyCurrencySymbol $ getValue v
        , [(tn, amt)] <- AssocMap.toList tns
        , amt == 1 =
        unTokenName tn
    getKeyTn _ = traceError "error 'mkInsertValidator': 'getKeyTn' failed"

    -- Given a TxOut, this will get (and check) if we have the 'TokenName' and
    -- required datum.
    getTxOutNodeInfo :: TxOut -> Node
    getTxOutNodeInfo o = mkNode (getKeyTn $ txOutValue o) $ unsafeGetDatum info o

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
 at. This will always return 'False'.
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

-- | 'mkDsKeyPolicy'.  See Note [How This All Works].
mkDsKeyPolicy :: DsMint -> () -> ScriptContext -> Bool
mkDsKeyPolicy dsm _red ctx = case ins of
  [_ownTn] -> True
  -- This is enough to imply that the validator succeeded. Since we know
  -- that all these tokens are paid to the original validator hash, if an
  -- input has this token, that means the validator has successffully
  -- validated. Woohoo!
  []
    | -- If we are minting the NFT which configures everything, then we
      -- should mint only the empty prefix
      Just _ <- AssocMap.lookup (dsmConf dsm) $ getValue $ txInfoMint info ->
      case mintedTns of
        [tn] | unTokenName tn == nKey rootNode ->
          traceIfFalse "error 'mkDsKeyPolicy' illegal outputs" $
            case find (\txout -> txOutAddress txout == Address.scriptHashAddress (dsmValidatorHash dsm)) $ txInfoOutputs info of
              Just txout -> isJust $ AssocMap.lookup ownCurrencySymbol $ getValue $ txOutValue txout
              Nothing -> False
        -- TODO: check in the script output with the output that this
        -- ownCurrencySymbol is actually where it should be.. Actually,
        -- there's no need to do this -- we can verify this offchain
        -- since we may assume that all participants know the protocol
        -- ahead of time and may independently verify.
        _ -> traceError "error 'mkDsKeyPolicy' bad initial mint"
  _ -> traceError "error 'mkDsKeyPolicy' bad inputs in transaction"
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
                (tn, _amt) : _ <- AssocMap.toList tns =
              tn : go ts
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
        _ -> traceError "error 'mkDsKeyPolicy': bad minted tokens"

-- | 'dsKeyPolicy' is the minting policy for the prefixes of nodes
dsKeyPolicy :: DsMint -> MintingPolicy
dsKeyPolicy dsm =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDsKeyPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode dsm

{- | 'dsKeyCurSymbol' is the currency symbol for prefixes of nodes in the
 distributed set
-}
dsKeyCurSymbol :: DsMint -> CurrencySymbol
dsKeyCurSymbol = Contexts.scriptCurrencySymbol . dsKeyPolicy

{- Note [Alternative Ways of Doing This]

 We actually did try some other ways of doing it, but none of them worked.  For
 reference, here's what we tried:

    * The [Stick Breaking
    Set](https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md)
    which had some obvious issues with datum sizes being too large which could
    be remedied by working at the bit level of having a binary tree with
    branches of 0 and 1.
    This had budget issues.

    * Variations of a Patricia Tree. This also had budget issues.
-}
