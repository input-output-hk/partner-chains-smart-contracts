{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Implementation of a set for on-chain proof of not in a set membership.
 We call this a *distributed set* since the set structure is distributed over
 many utxos in the block chain.
-}
module TrustlessSidechain.DistributedSet (
  -- * Data types
  Ds (..),
  DsDatum (..),
  Node (..),
  DsConfDatum (..),
  DsConfMint (..),
  DsKeyMint (..),
  Ib (..),

  -- * Helper functions for the data types
  mkNode,
  rootNode,
  fromListIb,
  lengthIb,
  insertNode,

  -- * Validators / minting policies
  mkInsertValidator,
  mkDsConfValidator,
  mkDsConfPolicy,
  dsConfTokenName,
  mkDsKeyPolicy,

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

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger.Address (scriptHashAddress)
import Plutus.Script.Utils.V2.Typed.Scripts (
  UntypedMintingPolicy,
  UntypedValidator,
  mkUntypedMintingPolicy,
  mkUntypedValidator,
 )
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  Map,
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName, unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
  ValidatorHash,
  Value (getValue),
 )
import Plutus.V2.Ledger.Api qualified as Api
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

{- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
 for the distributed set. (See Note [How This All Works]. Moreover, this
 parameterizes the 'mkInsertValidator' and is used as the type which identifies
 the appropriate datum and redeemer type
-}
newtype Ds = Ds
  { -- | The 'CurrencySymbol' which identifies the utxo with 'DsConfDatum'.
    -- |
    -- | @since Unreleased
    identitySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (FromData, ToData, UnsafeFromData)

-- | @since Unreleased
instance HasField "identitySymbol" Ds CurrencySymbol where
  {-# INLINE get #-}
  get (Ds x) = x
  {-# INLINE modify #-}
  modify f (Ds x) = Ds (f x)

-- | 'DsDatum' is the datum in the distributed set. See: Note [How This All Works]
newtype DsDatum = DsDatum
  { -- | @since Unreleased
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq, FromData, ToData, UnsafeFromData)

-- | @since Unreleased
instance HasField "next" DsDatum BuiltinByteString where
  {-# INLINE get #-}
  get (DsDatum x) = x
  {-# INLINE modify #-}
  modify f (DsDatum x) = DsDatum (f x)

{- | 'Node' is an internal data type of the tree node used in the validator.
 See: Note [How This All Works].
-}
data Node = Node
  { -- | @since Unreleased
    key :: BuiltinByteString
  , -- | @since Unreleased
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b =
    get @"key" a == get @"key" b
      && get @"next" a == get @"next" b

-- | @since Unreleased
instance HasField "key" Node BuiltinByteString where
  {-# INLINE get #-}
  get (Node x _) = x
  {-# INLINE modify #-}
  modify f (Node k n) = Node (f k) n

-- | @since Unreleased
instance HasField "next" Node BuiltinByteString where
  {-# INLINE get #-}
  get (Node _ x) = x
  {-# INLINE modify #-}
  modify f (Node k n) = Node k (f n)

-- | @since Unreleased
instance ToData Node where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Node {..}) = productToData2 key next

-- | @since Unreleased
instance FromData Node where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 Node

-- | @since Unreleased
instance UnsafeFromData Node where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 Node

{- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
 minting policies needed by the distributed set.
-}
data DsConfDatum = DsConfDatum
  { -- | @since Unreleased
    keyPolicy :: CurrencySymbol
  , -- | @since Unreleased
    fuelPolicy :: CurrencySymbol
  }

instance Eq DsConfDatum where
  {-# INLINEABLE (==) #-}
  a == b =
    get @"keyPolicy" a == get @"keyPolicy" b
      && get @"fuelPolicy" a == get @"fuelPolicy" b

-- | @since Unreleased
instance HasField "keyPolicy" DsConfDatum CurrencySymbol where
  {-# INLINE get #-}
  get (DsConfDatum x _) = x
  {-# INLINE modify #-}
  modify f (DsConfDatum kp fp) = DsConfDatum (f kp) fp

-- | @since Unreleased
instance HasField "fuelPolicy" DsConfDatum CurrencySymbol where
  {-# INLINE get #-}
  get (DsConfDatum _ x) = x
  {-# INLINE modify #-}
  modify f (DsConfDatum kp fp) = DsConfDatum kp (f fp)

-- | @since Unreleased
instance ToData DsConfDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsConfDatum {..}) = productToData2 keyPolicy fuelPolicy

-- | @since Unreleased
instance FromData DsConfDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsConfDatum

-- | @since Unreleased
instance UnsafeFromData DsConfDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsConfDatum

{- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
 length "array" of how many new nodes (this is always 2, see 'lengthIb') are
 generated after inserting into a node.
-}
newtype Ib a = Ib {unIb :: (a, a)}
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq)

instance TSPrelude.Foldable Ib where
  foldMap f (Ib (a, b)) = f a TSPrelude.<> f b

-- | @since Unreleased
instance (ToData a) => ToData (Ib a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Ib (x, y)) = productToData2 x y

-- | @since Unreleased
instance (PlutusTx.FromData a) => PlutusTx.FromData (Ib a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 (curry Ib)

-- | @since Unreleased
instance (PlutusTx.UnsafeFromData a) => PlutusTx.UnsafeFromData (Ib a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 (curry Ib)

{- | 'DsConfMint' is the parameter for the NFT to initialize the distributed
 set. See 'mkDsConfPolicy' for more details.
-}
newtype DsConfMint = DsConfMint
  { -- | @since Unreleased
    txOutRef :: TxOutRef
  }
  deriving newtype (PlutusTx.FromData, PlutusTx.ToData, PlutusTx.UnsafeFromData)

-- | @since Unreleased
instance HasField "txOutRef" DsConfMint TxOutRef where
  {-# INLINE get #-}
  get (DsConfMint x) = x
  {-# INLINE modify #-}
  modify f (DsConfMint x) = DsConfMint (f x)

{- | 'DsKeyMint' is the parameter for the minting policy. In particular, the
 'TokenName' of this 'CurrencySymbol' (from 'mkDsKeyPolicy') stores the key of
 the token. See Note [How This All Works] for more details.
-}
data DsKeyMint = DsKeyMint
  { -- | The validator hash that the minting policy
    -- | essentially "forwards" its checks to the validator.
    -- |
    -- | TODO: as an optimization, we can take the 'Address' as a parameter
    -- | instead (since the offchain code will always immediately convert this
    -- | into an 'Address').
    -- |
    -- | @since Unreleased
    validatorHash :: ValidatorHash
  , -- | The currency symbol to identify a utxo with 'DsConfDatum'
    -- |
    -- | @since Unreleased
    confCurrencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since Unreleased
instance HasField "validatorHash" DsKeyMint ValidatorHash where
  {-# INLINE get #-}
  get (DsKeyMint x _) = x
  {-# INLINE modify #-}
  modify f (DsKeyMint vh ccs) = DsKeyMint (f vh) ccs

-- | @since Unreleased
instance HasField "confCurrencySymbol" DsKeyMint CurrencySymbol where
  {-# INLINE get #-}
  get (DsKeyMint _ x) = x
  {-# INLINE modify #-}
  modify f (DsKeyMint vh ccs) = DsKeyMint vh (f ccs)

-- | @since Unreleased
instance ToData DsKeyMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsKeyMint {..}) =
    productToData2 validatorHash confCurrencySymbol

-- | @since Unreleased
instance FromData DsKeyMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsKeyMint

-- | @since Unreleased
instance UnsafeFromData DsKeyMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsKeyMint

{- | 'unsafeGetDatum' gets the datum sitting at a 'TxOut' and throws an error
 otherwise.
-}
{-# INLINEABLE unsafeGetDatum #-}
unsafeGetDatum :: PlutusTx.UnsafeFromData a => TxInfo -> TxOut -> a
unsafeGetDatum _info o = case txOutDatum o of
  -- Legacy code which used to accept a regular old datum.... we only allow
  -- inline datum now.
  -- > OutputDatumHash dhash
  -- >   | Just bn <- Contexts.findDatum dhash info ->
  -- >     PlutusTx.unsafeFromBuiltinData (getDatum bn)
  OutputDatum d -> PlutusTx.unsafeFromBuiltinData (getDatum d)
  _ -> traceError "error 'unsafeGetDatum' failed"

{- | 'getConf' gets the config associated with a distributed set and throws an
 error if it does not exist.
-}
{-# INLINEABLE getConf #-}
getConf :: CurrencySymbol -> TxInfo -> DsConfDatum
getConf currencySymbol info = go $ txInfoReferenceInputs info
  where
    go :: [TxInInfo] -> DsConfDatum
    go (t : ts) =
      case txInInfoResolved t of
        o -> case AssocMap.lookup currencySymbol $ getValue $ txOutValue o of
          Just _ -> unsafeGetDatum info o
          Nothing -> go ts
    go [] = traceError "error 'getConf' missing conf"

PlutusTx.makeLift ''DsKeyMint

PlutusTx.makeLift ''DsConfDatum

{- Note [How This all Works]
 See @docs/DistributedSet.md@.
 -}

-- | 'mkNode' is a wrapper to create a Node from a prefix and the datum.
{-# INLINEABLE mkNode #-}
mkNode :: BuiltinByteString -> DsDatum -> Node
mkNode str d =
  Node
    { key = str
    , next = get @"next" d
    }

-- | 'rootNode' is the root node of every distributed set.
{-# INLINEABLE rootNode #-}
rootNode :: Node
rootNode =
  Node
    { key = emptyByteString
    , -- this is the lower bound of all values in the set.

      next =
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
  | get @"key" node < str && str < get @"next" node =
    Just $
      Ib {unIb = (put @"next" str node, Node {key = str, next = get @"next" node})}
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
                      | get @"key" a < get @"key" b = Ib {unIb = (a, b)}
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
                && traceIfFalse "error 'mkInsertValidator' missing FUEL mint" (AssocMap.member (get @"fuelPolicy" conf) minted)
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
    !conf = getConf (get @"identitySymbol" ds) info

    keyCurrencySymbol :: CurrencySymbol
    keyCurrencySymbol = get @"keyPolicy" conf

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

{- | 'mkDsConfValidator' is the script for which 'DsConfDatum' will be sitting
 at. This will always error.
-}
mkDsConfValidator :: Ds -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfValidator _ds _dat _red _ctx = ()

-- TODO: when we get reference inputs, we need to change the above line
-- of code to the following line of code
-- > mkDsConfValidator _ds _dat _red _ctx = Builtins.error ()

{- | 'mkDsConfPolicy' mints the nft which identifies the utxo that stores
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
    spendsTxOutRef = isJust $ Contexts.findTxInByTxOutRef (get @"txOutRef" dsc) info

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

{-# INLINE mkDsKeyPolicy #-}

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
      AssocMap.member (get @"confCurrencySymbol" dskm) $ getValue $ txInfoMint info ->
      case mintedTns of
        [tn] | unTokenName tn == get @"key" rootNode ->
          traceIfFalse "error 'mkDsKeyPolicy' illegal outputs" $
            case find (\txout -> txOutAddress txout == scriptHashAddress (get @"validatorHash" dskm)) (txInfoOutputs info) of
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

    -- determines the nodes we are consuming
    ins :: [TokenName]
    ins =
      let go [] = []
          go (t : ts)
            | txout <- txInInfoResolved t
              , txOutAddress txout == scriptHashAddress (get @"validatorHash" dskm)
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
serialisableInsertValidator :: Versioned Script
serialisableInsertValidator = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkInsertValidatorUntyped||])) PlutusV2

{- | 'mkDsConfValidatorUntyped' creates an untyped 'mkDsConfValidator' (this is
 needed for ctl)
-}
mkDsConfValidatorUntyped :: BuiltinData -> UntypedValidator
mkDsConfValidatorUntyped = mkDsConfValidator . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsConfValidator' creates a serialisable version of the
 validator (this is needed for ctl)
-}
serialisableDsConfValidator :: Versioned Script
serialisableDsConfValidator = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkDsConfValidatorUntyped||])) PlutusV2

{- | 'mkDsConfPolicyUntyped' is an untyped version of 'mkDsConfPolicy' (this is
 needed for ctl)
-}
mkDsConfPolicyUntyped :: BuiltinData -> UntypedMintingPolicy
mkDsConfPolicyUntyped = mkUntypedMintingPolicy . mkDsConfPolicy . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsConfPolicy' creates a serialisable version of the minting
 policy (this is needed for ctl)
-}
serialisableDsConfPolicy :: Versioned Script
serialisableDsConfPolicy = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkDsConfPolicyUntyped||])) PlutusV2

{- | 'mkDsKeyPolicy' is an untyped version of 'mkDsKeyPolicy' (this is
 needed for ctl)
-}
mkDsKeyPolicyUntyped :: BuiltinData -> UntypedMintingPolicy
mkDsKeyPolicyUntyped = mkUntypedMintingPolicy . mkDsKeyPolicy . PlutusTx.unsafeFromBuiltinData

{- | 'serialisableDsKeyPolicy' creates a serialisable version of the minting
 policy (this is needed for ctl)
-}
serialisableDsKeyPolicy :: Versioned Script
serialisableDsKeyPolicy = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkDsKeyPolicyUntyped||])) PlutusV2
