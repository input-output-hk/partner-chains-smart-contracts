{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Implementation of a set for on-chain proof of not in a set membership.
 We call this a *distributed set* since the set structure is distributed over
 many utxos in the block chain.
-}
module TrustlessSidechain.OnChain.DistributedSet (
  -- * Data types
  Ds (Ds, dsConf),
  DsDatum (DsDatum, dsNext),
  Node (Node, nKey, nNext),
  DsConf,
  DsConfDatum (DsConfDatum, dscKeyPolicy, dscFUELPolicy),
  DsConfMint (
    DsConfMint,
    dscmTxOutRef
  ),
  DsKeyMint (
    DsKeyMint,
    dskmValidatorHash,
    dskmConfCurrencySymbol
  ),
  Ib (Ib, unIb),

  -- * Helper functions for the data types
  mkNode,
  rootNode,
  fromListIb,
  lengthIb,
  insertNode,

  -- * Validators / minting policies
  insertValidator,
  insertValidatorHash,
  insertAddress,
  mkDsConfValidator,
  dsConfValidator,
  mkDsConfPolicy,
  dsConfTokenName,
  dsConfPolicy,
  dsConfCurrencySymbol,
  mkDsKeyPolicy,
  dsKeyPolicy,
  dsKeyCurrencySymbol,
  dsConfValidatorHash,

  -- * CTL serialisable validators / policies
  mkInsertValidatorUntyped,
  serialisableInsertValidator,
  mkDsConfValidatorUntyped,
  serialisableDsConfValidator,
  mkDsConfPolicyUntyped,
  serialisableDsConfPolicy,
  mkDsKeyPolicyUntyped,
  serialisableDsKeyPolicy,
) where

import PlutusTx.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Ledger.Address (scriptHashAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts (UntypedMintingPolicy, UntypedValidator, mkUntypedMintingPolicy, mkUntypedValidator)
import Plutus.V2.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (getDatum),
  Map,
  MintingPolicy,
  OutputDatum (NoOutputDatum, OutputDatumHash),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName, unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
  Validator,
  ValidatorHash,
  Value (getValue),
  mkMintingPolicyScript,
  mkValidatorScript,
 )
import Plutus.V2.Ledger.Api qualified as Api
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusPrelude qualified
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import Prelude qualified

-- copied directly from Ledger.Address, which for some reason no longer exports it
scriptAddress :: Validator -> Address
scriptAddress validator = Address (ScriptCredential (validatorHash validator)) Nothing

{- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
 for the distributed set. (See Note [How This All Works]. Moreover, this
 parameterizes the 'mkInsertValidator' and is used as the type which identifies
 the appropriate datum and redeemer type
-}
newtype Ds = Ds
  { -- | 'dsConf' is the 'CurrencySymbol' which identifies the utxo
    -- with 'DsConfDatum'.
    dsConf :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

--instance ValidatorTypes Ds where
--  type DatumType Ds = DsDatum
--  type RedeemerType Ds = ()

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

--instance ValidatorTypes DsConf where
--  type DatumType DsConf = DsConfDatum
--  type RedeemerType DsConf = ()

{- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
 minting policies needed by the distributed set.
-}
data DsConfDatum = DsConfDatum
  { dscKeyPolicy :: CurrencySymbol
  , dscFUELPolicy :: CurrencySymbol
  }

instance Eq DsConfDatum where
  a == b = dscKeyPolicy a == dscKeyPolicy b && dscFUELPolicy a == dscFUELPolicy b

{- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
 length "array" of how many new nodes (this is always 2, see 'lengthIb') are
 generated after inserting into a node.
-}
newtype Ib a = Ib {unIb :: (a, a)}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)
  deriving newtype (Eq)

instance Prelude.Foldable Ib where
  foldMap f (Ib (a, b)) = f a Prelude.<> f b

{- | 'DsConfMint' is the parameter for the NFT to initialize the distributed
 set. See 'mkDsConfPolicy' for more details.
-}
newtype DsConfMint = DsConfMint {dscmTxOutRef :: TxOutRef}

{- | 'DsKeyMint' is the parameter for the minting policy. In particular, the
 'TokenName' of this 'CurrencySymbol' (from 'mkDsKeyPolicy') stores the key of
 the token. See Note [How This All Works] for more details.
-}
data DsKeyMint = DsKeyMint
  { -- | 'dskmValidatorHash' is the validator hash that the minting policy
    -- essentially "forwards" its checks to the validator.
    --
    --
    -- TODO: as an optimization, we can take the 'Address' as a parameter
    -- instead (since the offchain code will always immediately convert this
    -- into an 'Address').
    dskmValidatorHash :: ValidatorHash
  , -- | 'dskmConfCurrencySymbol' is the currency symbol to identify a utxo with 'DsConfDatum'
    dskmConfCurrencySymbol :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

{- | 'unsafeGetDatum' gets the datum sitting at a 'TxOut' and throws an error
 otherwise.
 TODO: Using inline datum is probably a good idea for this.
-}
{-# INLINEABLE unsafeGetDatum #-}
unsafeGetDatum :: PlutusTx.UnsafeFromData a => TxInfo -> TxOut -> a
unsafeGetDatum info o = case txOutDatum o of
  OutputDatumHash dhash
    | Just bn <- Contexts.findDatum dhash info ->
      PlutusTx.unsafeFromBuiltinData (getDatum bn)
  --OutputDatum d   -> PlutusTx.unsafeFromBuiltinData d TODO
  NoOutputDatum -> traceError "error 'unsafeGetDatum' failed"
  _ -> traceError "error 'unsafeGetDatum' failed"

--  | Just bn <- txOutDatum o , Just bn <- Contexts.findDatum dhash info =
--    = PlutusTx.unsafeFromBuiltinData (getDatum bn)
--  | otherwise = traceError "error 'unsafeGetDatum' failed"

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

makeIsDataIndexed ''DsKeyMint [('DsKeyMint, 0)]
deriveJSON defaultOptions ''DsKeyMint
PlutusTx.makeLift ''DsKeyMint

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
        -- TODO:
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
  [a, b] -> Ib {unIb = (a, b)}
  _ -> traceError "error 'fromListIb' bad list"

-- | 'lengthIb' always returns 2.
{-# INLINEABLE lengthIb #-}
lengthIb :: Ib a -> Integer
lengthIb _ = 2

{- | @'insertNode' str node@ inserts returns the new nodes which should be
 created (in place of the old @node@) provided that @str@ can actually be
 inserted here. See Note [How This All Works].

 Note that the first projection of 'Ib' will always be the node which should
 replace @node@, which also should be the node which is strictly less than
 @str@. This property is helpful in 'mkInsertValidator' when verifying that the
 nodes generated are as they should be.
-}
{-# INLINEABLE insertNode #-}
insertNode :: BuiltinByteString -> Node -> Maybe (Ib Node)
insertNode str node
  | nKey node < str && str < nNext node =
    Just $
      Ib {unIb = (node {nNext = str}, Node {nKey = str, nNext = nNext node})}
  | otherwise = Nothing

{- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
 done in the 'insertNode' function.
-}
{-# INLINEABLE mkInsertValidator #-}
mkInsertValidator :: Ds -> DsDatum -> () -> ScriptContext -> Bool
mkInsertValidator ds _dat _red ctx =
  ( \nStr ->
      ( \nNodes ->
          let -- the "continuing" nodes...
              contNodes :: Ib Node
              contNodes =
                let normalizeIbNodes Ib {unIb = (a, b)}
                      | nKey a < nKey b = Ib {unIb = (a, b)}
                      | otherwise = Ib {unIb = (b, a)}
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
        ( fromMaybe
            (traceError "error 'mkInsertValidator' bad insertion")
            (insertNode nStr $ getTxOutNodeInfo (txInInfoResolved ownInput))
        )
  )
    ( case AssocMap.lookup keyCurrencySymbol minted of
        Just mp
          | [(leaf, amt)] <- AssocMap.toList mp
            , amt == 1 ->
            unTokenName leaf
        _ -> traceError "error 'mkInsertValidator' missing unique string to insert"
    )
  where
    -- if you're wondering why this is written in such an unreadable way, it's
    -- because (as I read it) plutus is by value language, and to ensure that
    -- things aren't recomputed, you can wrap things up with a lambda...
    -- otherwise, we run into budgeting issues...

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

{- | The typed validator script for the distributed set.
typedInsertValidator :: Ds -> TypedValidator Ds
-}
insertValidator :: Ds -> Validator
insertValidator ds =
  let wrap = mkUntypedValidator . mkInsertValidator
   in mkValidatorScript
        ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode ds)

--($$(PlutusTx.compile [||\d -> mkUntypedValidator (mkInsertValidator d)||]) `PlutusTx.applyCode` PlutusTx.liftCode ds)

-- | The validator hash for the distributed set.
insertValidatorHash :: Ds -> ValidatorHash
insertValidatorHash = validatorHash . insertValidator

-- | The address for the distributed set.
insertAddress :: Ds -> Address
insertAddress = scriptAddress . insertValidator

{- | 'mkDsConfValidator' is the script for which 'DsConfDatum' will be sitting
 at. This will always error.
-}
mkDsConfValidator :: Ds -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfValidator _ds _dat _red _ctx = ()

-- TODO: when we get reference inputs, we need to change the above line
-- of code to the following line of code
-- > mkDsConfValidator _ds _dat _red _ctx = Builtins.error ()

-- | The regular validator script for the conf. of the distributed set.
dsConfValidator :: Ds -> Validator
dsConfValidator ds =
  mkValidatorScript ($$(PlutusTx.compile [||mkDsConfValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ds)

{- | The validator hash for the conf. of the distributed set.

 TODO: do this properly by fetching the right package...
-}
dsConfValidatorHash :: Ds -> ValidatorHash
dsConfValidatorHash = validatorHash . dsConfValidator

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

{- | 'dsConfTokenName' is the token name of the NFT which identifies the utxo
 holding 'DsConfDatum'. We just leave this as the empty string since it
 doesn't matter
-}
dsConfTokenName :: TokenName
dsConfTokenName = TokenName emptyByteString

-- | 'dsConfPolicy' is the minting policy for distributed set
dsConfPolicy :: DsConfMint -> MintingPolicy
dsConfPolicy dscm =
  mkMintingPolicyScript
    ($$(PlutusTx.compile [||mkUntypedMintingPolicy . mkDsConfPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode dscm)

-- | 'dsConfCurrencySymbol' is the currency symbol for the distributed set
dsConfCurrencySymbol :: DsConfMint -> CurrencySymbol
dsConfCurrencySymbol = scriptCurrencySymbol . dsConfPolicy

-- | 'mkDsKeyPolicy'.  See Note [How This All Works].
mkDsKeyPolicy :: DsKeyMint -> () -> ScriptContext -> Bool
mkDsKeyPolicy dskm _red ctx = case ins of
  [_ownTn] -> True
  -- This is enough to imply that the validator succeeded. Since we know
  -- that all these tokens are paid to the original validator hash, if an
  -- input has this token, that means the validator has successffully
  -- validated. Woohoo!
  []
    | -- If we are minting the NFT which configures everything, then we
      -- should mint only the empty prefix
      AssocMap.member (dskmConfCurrencySymbol dskm) $ getValue $ txInfoMint info ->
      case mintedTns of
        [tn] | unTokenName tn == nKey rootNode ->
          traceIfFalse "error 'mkDsKeyPolicy' illegal outputs" $
            case find (\txout -> txOutAddress txout == scriptHashAddress (dskmValidatorHash dskm)) (txInfoOutputs info) of
              Just txout -> AssocMap.member ownCS $ getValue $ txOutValue txout
              Nothing -> False
        -- Note: Why don't we have to verify that the 'DsConf' validator has
        -- 'ownCS' stored in the 'DsConfDatum' field 'dscKeyPolicy'? This is
        -- because we assume that everyone knows the protocol to participate in
        -- this system (and the 'DsConf' validator cannot be changed), so
        -- everyone may independently verify offchain that the 'dscKeyPolicy'
        -- is as expected.
        _ -> traceError "error 'mkDsKeyPolicy' bad initial mint"
  _ -> traceError "error 'mkDsKeyPolicy' bad inputs in transaction"
  where
    -- Aliases
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCS :: CurrencySymbol
    ownCS = Contexts.ownCurrencySymbol ctx

    -- determines the branches that we are consuming
    ins :: [TokenName]
    ins =
      let go [] = []
          go (t : ts)
            | txout <- txInInfoResolved t
              , txOutAddress txout == scriptHashAddress (dskmValidatorHash dskm)
              , Just tns <- AssocMap.lookup ownCS $ getValue (txOutValue txout)
              , -- If it's more clear, we're checking the following condition:
                -- > [(tn,1)] <- AssocMap.toList tns
                -- In our case, it is implicit that there is exactly one
                -- 'TokenName' and that there will be only one distinct 'TokenName'.
                (tn, _amt) : _ <- AssocMap.toList tns =
              tn : go ts
            -- Need to keep recursing to ensure that this transaction
            -- is only spending one input
            | otherwise = go ts -- otherwise, we skip the element
       in go (txInfoInputs info)

    mintedTns :: [TokenName]
    mintedTns = case AssocMap.lookup ownCS $ getValue (txInfoMint info) of
      Just mp | vs <- AssocMap.toList mp, all ((== 1) . snd) vs -> map fst vs
      _ -> traceError "error 'mkDsKeyPolicy': bad minted tokens"

-- | 'dsKeyPolicy' is the minting policy for the prefixes of nodes
dsKeyPolicy :: DsKeyMint -> MintingPolicy
dsKeyPolicy dskm =
  let wrap = mkUntypedMintingPolicy . mkDsKeyPolicy
   in mkMintingPolicyScript ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode dskm)

{- | 'dsKeyCurrencySymbol' is the currency symbol for prefixes of nodes in the
 distributed set
-}
dsKeyCurrencySymbol :: DsKeyMint -> CurrencySymbol
dsKeyCurrencySymbol = scriptCurrencySymbol . dsKeyPolicy

{- Note [Alternative Ways of Doing This]
 We actually did try some other ways of doing it, but none of them worked.  For
 reference, here's what we tried:

    * The [Stick Breaking Set](https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md)
    which had some obvious issues with datum sizes being too large which could
    be remedied by working at the bit level of having a binary tree with
    branches of 0 and 1.
    This had budget issues.

    * Variations of a Patricia Tree. This also had budget issues.
-}

{- | 'mkInsertValidatorUntyped' creates an untyped 'mkInsertValidator' (this is
 needed for ctl)
-}
mkInsertValidatorUntyped :: BuiltinData -> UntypedValidator
mkInsertValidatorUntyped = mkUntypedValidator . mkInsertValidator . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableInsertValidator' is a serialisable version of the validator
 (this is needed for ctl)
-}
serialisableInsertValidator :: Script
serialisableInsertValidator =
  Api.fromCompiledCode $$(PlutusTx.compile [||mkInsertValidatorUntyped||])

{- | 'mkDsConfValidatorUntyped' creates an untyped 'mkDsConfValidator' (this is
 needed for ctl)
-}
mkDsConfValidatorUntyped :: BuiltinData -> UntypedValidator
mkDsConfValidatorUntyped = mkDsConfValidator . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsConfValidator' creates a serialisable version of the
 validator (this is needed for ctl)
-}
serialisableDsConfValidator :: Script
serialisableDsConfValidator =
  Api.fromCompiledCode $$(PlutusTx.compile [||mkDsConfValidatorUntyped||])

{- | 'mkDsConfPolicyUntyped' is an untyped version of 'mkDsConfPolicy' (this is
 needed for ctl)
-}
mkDsConfPolicyUntyped :: BuiltinData -> UntypedMintingPolicy
mkDsConfPolicyUntyped = mkUntypedMintingPolicy . mkDsConfPolicy . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsConfPolicy' creates a serialisable version of the minting
 policy (this is needed for ctl)
-}
serialisableDsConfPolicy :: Script
serialisableDsConfPolicy = Api.fromCompiledCode $$(PlutusTx.compile [||mkDsConfPolicyUntyped||])

{- | 'mkDsKeyPolicy' is an untyped version of 'mkDsKeyPolicy' (this is
 needed for ctl)
-}
mkDsKeyPolicyUntyped :: BuiltinData -> UntypedMintingPolicy
mkDsKeyPolicyUntyped = mkUntypedMintingPolicy . mkDsKeyPolicy . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsKeyPolicy' creates a serialisable version of the minting
 policy (this is needed for ctl)
-}
serialisableDsKeyPolicy :: Script
serialisableDsKeyPolicy = Api.fromCompiledCode $$(PlutusTx.compile [||mkDsKeyPolicyUntyped||])
