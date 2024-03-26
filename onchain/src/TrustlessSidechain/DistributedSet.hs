{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of a set for on-chain proof of not in a set membership.
-- We call this a *distributed set* since the set structure is distributed over
-- many utxos in the block chain.
module TrustlessSidechain.DistributedSet (
  -- * Data types
  Ds (..),
  DsDatum (..),
  Node (..),
  DsConfDatum (..),
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

import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  Map,
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName, unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  ValidatorHash,
  Value (getValue),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  InitTokenAssetClass,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (
  oneTokenBurned,
 )

-- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
-- for the distributed set. (See Note [How This All Works]. Moreover, this
-- parameterizes the 'mkInsertValidator' and is used as the type which identifies
-- the appropriate datum and redeemer type
newtype Ds = Ds
  { -- | The 'CurrencySymbol' which identifies the utxo with 'DsConfDatum'.
    -- |
    -- | @since v4.0.0
    identitySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (FromData, ToData, UnsafeFromData)

-- | @since v4.0.0
makeHasField ''Ds

-- | 'DsDatum' is the datum in the distributed set. See: Note [How This All Works]
newtype DsDatum = DsDatum
  { -- | @since v4.0.0
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq, FromData, ToData, UnsafeFromData)

-- | @since v4.0.0
makeHasField ''DsDatum

-- | 'Node' is an internal data type of the tree node used in the validator.
-- See: Note [How This All Works].
data Node = Node
  { -- | @since v4.0.0
    key :: BuiltinByteString
  , -- | @since v4.0.0
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  a == b =
    get @"key" a == get @"key" b
      && get @"next" a == get @"next" b

-- | @since v4.0.0
makeHasField ''Node

-- | @since v4.0.0
instance ToData Node where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Node {..}) = productToData2 key next

-- | @since v4.0.0
instance FromData Node where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 Node

-- | @since v4.0.0
instance UnsafeFromData Node where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 Node

-- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
-- minting policies needed by the distributed set.
data DsConfDatum = DsConfDatum
  { -- | @since v4.0.0
    keyPolicy :: CurrencySymbol
  , -- | @since v4.0.0
    fuelPolicy :: CurrencySymbol
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

instance Eq DsConfDatum where
  {-# INLINEABLE (==) #-}
  a == b =
    get @"keyPolicy" a == get @"keyPolicy" b
      && get @"fuelPolicy" a == get @"fuelPolicy" b

-- | @since v4.0.0
makeHasField ''DsConfDatum

-- | @since v4.0.0
instance ToData DsConfDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsConfDatum {..}) = productToData2 keyPolicy fuelPolicy

-- | @since v4.0.0
instance FromData DsConfDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsConfDatum

-- | @since v4.0.0
instance UnsafeFromData DsConfDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsConfDatum

-- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
-- length "array" of how many new nodes (this is always 2, see 'lengthIb') are
-- generated after inserting into a node.
newtype Ib a = Ib {unIb :: (a, a)}
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq)

instance TSPrelude.Foldable Ib where
  foldMap f (Ib (a, b)) = f a TSPrelude.<> f b

-- | @since v4.0.0
instance (ToData a) => ToData (Ib a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Ib (x, y)) = productToData2 x y

-- | @since v4.0.0
instance (PlutusTx.FromData a) => PlutusTx.FromData (Ib a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 (curry Ib)

-- | @since v4.0.0
instance (PlutusTx.UnsafeFromData a) => PlutusTx.UnsafeFromData (Ib a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 (curry Ib)

-- | 'DsKeyMint' is the parameter for the minting policy. In particular, the
-- 'TokenName' of this 'CurrencySymbol' (from 'mkDsKeyPolicy') stores the key of
-- the token. See Note [How This All Works] for more details.
data DsKeyMint = DsKeyMint
  { -- | The validator hash that the minting policy
    -- | essentially "forwards" its checks to the validator.
    -- |
    -- | TODO: as an optimization, we can take the 'Address' as a parameter
    -- | instead (since the offchain code will always immediately convert this
    -- | into an 'Address').
    -- |
    -- | @since v4.0.0
    validatorHash :: ValidatorHash
  , -- | The currency symbol to identify a utxo with 'DsConfDatum'
    -- |
    -- | @since v4.0.0
    confCurrencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v4.0.0
makeHasField ''DsKeyMint

-- | @since v4.0.0
instance ToData DsKeyMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsKeyMint {..}) =
    productToData2 validatorHash confCurrencySymbol

-- | @since v4.0.0
instance FromData DsKeyMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsKeyMint

-- | @since v4.0.0
instance UnsafeFromData DsKeyMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsKeyMint

-- | 'unsafeGetDatum' gets the inline datum sitting at a 'TxOut' and throws an
-- error otherwise.
--
-- OnChain error descriptions:
--
--   ERROR-UNSAFE-GET-DATUM-01: tx output does not have an inline datum attached
--   to it.
{-# INLINEABLE unsafeGetDatum #-}
unsafeGetDatum :: PlutusTx.UnsafeFromData a => TxOut -> a
unsafeGetDatum o = case txOutDatum o of
  OutputDatum d -> PlutusTx.unsafeFromBuiltinData (getDatum d)
  _ -> traceError "ERROR-UNSAFE-GET-DATUM-01"

-- | 'getConf' gets the config associated with a distributed set and throws an
-- error if it does not exist.
--
-- OnChain error descriptions:
--
--   ERROR-DS-GET-CONF-01: could not find a valid distributed set configuration
--   in the provided transaction inputs.
{-# INLINEABLE getConf #-}
getConf :: CurrencySymbol -> TxInfo -> DsConfDatum
getConf currencySymbol info = go $ txInfoReferenceInputs info
  where
    go :: [TxInInfo] -> DsConfDatum
    go (t : ts) =
      case txInInfoResolved t of
        o -> case AssocMap.lookup currencySymbol $ getValue $ txOutValue o of
          Just _ -> unsafeGetDatum o
          Nothing -> go ts
    go [] = traceError "ERROR-DS-GET-CONF-01"

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

-- | 'fromListIb lst' converts a list of length 2 into an 'Ib' and throws an
-- exception otherwise.
--
-- OnChain error descriptions:
--
--   ERROR-DS-FROM-LIST-IB-01: provided list should contain exactly two elements,
--   but it was of different length.
{-# INLINEABLE fromListIb #-}
fromListIb :: [a] -> Ib a
fromListIb = \case
  [a, b] -> Ib {unIb = (a, b)}
  _ -> traceError "ERROR-DS-FROM-LIST-IB-01"

-- | 'lengthIb' always returns 2.
{-# INLINEABLE lengthIb #-}
lengthIb :: Ib a -> Integer
lengthIb _ = 2

-- | @'insertNode' str node@ inserts returns the new nodes which should be
-- created (in place of the old @node@) provided that @str@ can actually be
-- inserted here. See Note [How This All Works].
--
-- Note that the first projection of 'Ib' will always be the node which should
-- replace @node@, which also should be the node which is strictly less than
-- @str@. This property is helpful in 'mkInsertValidator' when verifying that
-- the nodes generated are as they should be.
{-# INLINEABLE insertNode #-}
insertNode :: BuiltinByteString -> Node -> Maybe (Ib Node)
insertNode str node
  | get @"key" node < str && str < get @"next" node =
    Just $
      Ib {unIb = (put @"next" str node, Node {key = str, next = get @"next" node})}
  | otherwise = Nothing

-- | 'mkInsertValidator' is rather complicated. Most of the heavy lifting is
-- done in the 'insertNode' function.
--
-- OnChain error descriptions:
--
--   ERROR-DS-INSERT-VALIDATOR-01: Inserted node different from continuing node.
--
--   ERROR-DS-INSERT-VALIDATOR-02: Incorrect number of distributed set key
--   tokens.  Should be 2.
--
--   ERROR-DS-INSERT-VALIDATOR-03: No FUEL tokens minted.
--
--   ERROR-DS-INSERT-VALIDATOR-04: Couldn't perform node insertion.
--
--   ERROR-DS-INSERT-VALIDATOR-05: Transaction doesn't mint exactly one key
--   token.
--
--   ERROR-DS-INSERT-VALIDATOR-06: Couldn't find currently validated input.
--   Should never happen.
--
--   ERROR-DS-INSERT-VALIDATOR-07: Value contains exactly one currency symbol,
--   but it does not match the keyCurrencySymbol.
--
--   ERROR-DS-INSERT-VALIDATOR-08: Value does not contain exactly one token.
--
--   ERROR-DS-INSERT-VALIDATOR-09: Value does contain exactly one currency
--   symbol, but it does not contain exactly one token name for that currency.
--
--   ERROR-DS-INSERT-VALIDATOR-10: Value does not contain exactly one currency
--   symbol, i.e. it either contains none or more than one.
{-# INLINEABLE mkInsertValidator #-}
mkInsertValidator :: Ds -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
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

              -- the total number distributed set key tokens in this
              -- transaction which is @1 + the number of minted tokens@ since
              -- we know that there is only one input with this token; and we
              -- mint the rest of the tokens
              --
              -- In erroneous cases, we return @-1@ which will always be
              -- @False@ in the below predicate
              totalKeys :: Integer
              totalKeys = case AssocMap.lookup keyCurrencySymbol minted of
                Just mp
                  | [(_, amt)] <- AssocMap.toList mp
                    , amt == 1 ->
                    2
                _ -> -1
           in traceIfFalse "ERROR-DS-INSERT-VALIDATOR-01" (contNodes == nNodes)
                && traceIfFalse "ERROR-DS-INSERT-VALIDATOR-02" (totalKeys == lengthIb nNodes)
                && traceIfFalse "ERROR-DS-INSERT-VALIDATOR-03" (AssocMap.member (get @"fuelPolicy" conf) minted)
      )
        ( fromMaybe
            (traceError "ERROR-DS-INSERT-VALIDATOR-04")
            (insertNode nStr $ getTxOutNodeInfo (txInInfoResolved ownInput))
        )
  )
    ( case AssocMap.lookup keyCurrencySymbol minted of
        Just mp
          | [(leaf, amt)] <- AssocMap.toList mp
            , amt == 1 ->
            unTokenName leaf
        _ -> traceError "ERROR-DS-INSERT-VALIDATOR-05"
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
      Nothing -> traceError "ERROR-DS-INSERT-VALIDATOR-06"

    minted :: Map CurrencySymbol (Map TokenName Integer)
    !minted = getValue (txInfoMint info)

    conf :: DsConfDatum
    !conf = getConf (get @"identitySymbol" ds) info

    keyCurrencySymbol :: CurrencySymbol
    keyCurrencySymbol = get @"keyPolicy" conf

    -- Given a value, gets the token name (of the "continuing" token name). Also
    -- verifies that the 'Value' is 'relatively small': namely, that it has only
    -- two 'CurrencySymbol's (Ada and the 'keyCurrencySymbol'), with only one
    -- 'TokenName' mapping for each.
    getKeyTn :: Value -> BuiltinByteString
    getKeyTn v = case AssocMap.toList . getValue $ v of
      -- Note from Koz: We assume that Ada comes first here. Furthermore, we
      -- also assume that the Ada 'entry' is special in that it's associated
      -- with one, and only one, 'TokenName'.
      [_, (cs, innerMap)] -> case AssocMap.toList innerMap of
        [(tn, amt)] ->
          if
              | cs /= keyCurrencySymbol ->
                traceError "ERROR-DS-INSERT-VALIDATOR-07"
              | amt /= 1 ->
                traceError "ERROR-DS-INSERT-VALIDATOR-08"
              | otherwise -> unTokenName tn
        -- Note from Koz: It would seem to be a better idea to fuse these two
        -- error cases together, but it is not so: we'd have to drag in some way
        -- of mapping AssocMap.toList over the entire 'outer map', which
        -- actually makes the whole function bigger by a non-trivial amount.
        -- Yes, it doesn't make much sense to me either.
        _ -> traceError "ERROR-DS-INSERT-VALIDATOR-09"
      _ -> traceError "ERROR-DS-INSERT-VALIDATOR-10"

    -- Given a TxOut, this will get (and check) if we have the 'TokenName' and
    -- required datum.
    getTxOutNodeInfo :: TxOut -> Node
    getTxOutNodeInfo o = mkNode (getKeyTn $ txOutValue o) $ unsafeGetDatum o

-- | 'mkDsConfValidator' is the script for which 'DsConfDatum' will be sitting
-- at. This will always error.
mkDsConfValidator :: Ds -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfValidator _ds _dat _red _ctx = Builtins.error ()

-- | 'mkDsConfPolicy' mints the nft which identifies the utxo that stores
-- the various minting policies that the distributed set depends on
--
-- OnChain error descriptions:
--
--   ERROR-DS-CONF-POLICY-01: The transaction doesn't spend init token.
--
--   ERROR-DS-CONF-POLICY-02: Invalid mint.  Transaction should mint only one
--   dsConfTokenName token, but it doesn't.
mkDsConfPolicy :: InitTokenAssetClass -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkDsConfPolicy itac _red ctx =
  traceIfFalse "ERROR-DS-CONF-POLICY-01" initTokenBurned
    && traceIfFalse "ERROR-DS-CONF-POLICY-02" mintingChecks
  where
    -- Aliases
    mint :: Value
    mint = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Unsafe.ownCurrencySymbol ctx

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

    mintingChecks :: Bool
    mintingChecks
      | Just tns <- AssocMap.lookup ownCurSymb $ getValue mint
        , [(tn, amt)] <- AssocMap.toList tns
        , tn == dsConfTokenName
        , amt == 1 =
        True
      | otherwise = False

-- | 'dsConfTokenName' is the token name of the NFT which identifies the utxo
-- holding 'DsConfDatum'. We just leave this as the empty string since it
-- doesn't matter
dsConfTokenName :: TokenName
dsConfTokenName = TokenName emptyByteString

{-# INLINE mkDsKeyPolicy #-}

-- | 'mkDsKeyPolicy'.  See Note [How This All Works].
--
-- OnChain error descriptions:
--
--   ERROR-DS-CONF-POLICY-01: Transaction does not send a minted token to
--   validator hash indicated by DsKeyMint.
--
--   ERROR-DS-CONF-POLICY-02: Transaction does not mint exactly one token
--
--   ERROR-DS-CONF-POLICY-03: Invalid transaction inputs that match neither
--   distributed set initialization nor insertion into the already existing set.
--
--   ERROR-DS-CONF-POLICY-04: Bad minted tokens.  Transaction should only mint
--   one own token.
mkDsKeyPolicy :: DsKeyMint -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkDsKeyPolicy dskm _red ctx = case ins of
  [_ownTn] -> True
  -- This is enough to imply that the validator succeeded. Since we know
  -- that all these tokens are paid to the original validator hash, if an
  -- input has this token, that means the validator has successffully
  -- validated. Woohoo!
  []
    | -- If we are minting the NFT which configures everything, then we
      -- should mint only the empty prefix
      AssocMap.member (get @"confCurrencySymbol" dskm) $ getValue $ Unsafe.decode $ Unsafe.txInfoMint info ->
      case mintedTns of
        [tn] | unTokenName tn == get @"key" rootNode ->
          traceIfFalse "ERROR-DS-CONF-POLICY-01" $
            case find
              (\txout -> Unsafe.decode (Unsafe.txOutAddress txout) == scriptHashAddress (get @"validatorHash" dskm))
              (Unsafe.txInfoOutputs info) of
              Just txout -> AssocMap.member ownCS $ getValue $ Unsafe.decode $ Unsafe.txOutValue txout
              Nothing -> False
        -- Note: Why don't we have to verify that the 'DsConf' validator has
        -- 'ownCS' stored in the 'DsConfDatum' field 'dscKeyPolicy'? This is
        -- because we assume that everyone knows the protocol to participate in
        -- this system (and the 'DsConf' validator cannot be changed), so
        -- everyone may independently verify offchain that the 'dscKeyPolicy'
        -- is as expected.
        _ -> traceError "ERROR-DS-CONF-POLICY-02"
  _ -> traceError "ERROR-DS-CONF-POLICY-03"
  where
    -- Aliases
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    ownCS :: CurrencySymbol
    ownCS = Unsafe.ownCurrencySymbol ctx

    -- determines the nodes we are consuming
    ins :: [TokenName]
    ins =
      let go :: [Unsafe.TxInInfo] -> [TokenName]
          go [] = []
          go (t : ts)
            | txout <- Unsafe.txInInfoResolved t
              , Unsafe.decode (Unsafe.txOutAddress txout) == scriptHashAddress (get @"validatorHash" dskm)
              , Just tns <- AssocMap.lookup ownCS $ getValue (Unsafe.decode $ Unsafe.txOutValue txout)
              , -- If it's more clear, we're checking the following condition:
                -- > [(tn,1)] <- AssocMap.toList tns
                -- In our case, it is implicit that there is exactly one
                -- 'TokenName' and that there will be only one distinct 'TokenName'.
                (tn, _amt) : _ <- AssocMap.toList tns =
              tn : go ts
            -- Need to keep recursing to ensure that this transaction
            -- is only spending one input
            | otherwise = go ts -- otherwise, we skip the element
       in go (Unsafe.txInfoInputs info)

    mintedTns :: [TokenName]
    mintedTns = case AssocMap.lookup ownCS $ getValue (Unsafe.decode $ Unsafe.txInfoMint info) of
      Just mp | vs <- AssocMap.toList mp, all ((== 1) . snd) vs -> map fst vs
      _ -> traceError "ERROR-DS-CONF-POLICY-04"

-- Note [Alternative Ways of Doing This]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We actually did try some other ways of doing it, but none of them worked.
-- For reference, here's what we tried:
--
--  * The [Stick Breaking Set](https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md)
--    which had some obvious issues with datum sizes being too large which could
--    be remedied by working at the bit level of having a binary tree with
--    branches of 0 and 1.  This had budget issues.
--
--  * Variations of a Patricia Tree. This also had budget issues.

-- | 'mkInsertValidatorUntyped' creates an untyped 'mkInsertValidator' (this is
-- needed for ctl)
mkInsertValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkInsertValidatorUntyped ds dat red ctx =
  check $
    mkInsertValidator
      (PlutusTx.unsafeFromBuiltinData ds)
      dat
      red
      (PlutusTx.unsafeFromBuiltinData ctx)

-- | 'serialisableInsertValidator' is a serialisable version of the validator
-- (this is needed for ctl)
serialisableInsertValidator :: Script
serialisableInsertValidator = fromCompiledCode $$(PlutusTx.compile [||mkInsertValidatorUntyped||])

-- | 'mkDsConfValidatorUntyped' creates an untyped 'mkDsConfValidator' (this is
-- needed for ctl)
mkDsConfValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfValidatorUntyped = mkDsConfValidator . PlutusTx.unsafeFromBuiltinData

-- | 'serialisableDsConfValidator' creates a serialisable version of the
-- validator (this is needed for ctl)
serialisableDsConfValidator :: Script
serialisableDsConfValidator = fromCompiledCode $$(PlutusTx.compile [||mkDsConfValidatorUntyped||])

-- | 'mkDsConfPolicyUntyped' is an untyped version of 'mkDsConfPolicy' (this is
-- needed for ctl)
mkDsConfPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsConfPolicyUntyped keyMint redeemer ctx =
  check $
    mkDsConfPolicy
      (PlutusTx.unsafeFromBuiltinData keyMint)
      redeemer
      (Unsafe.wrap ctx)

-- | 'serialisableDsConfPolicy' creates a serialisable version of the minting
-- policy (this is needed for ctl)
serialisableDsConfPolicy :: Script
serialisableDsConfPolicy = fromCompiledCode $$(PlutusTx.compile [||mkDsConfPolicyUntyped||])

-- | 'mkDsKeyPolicy' is an untyped version of 'mkDsKeyPolicy' (this is
-- needed for ctl)
mkDsKeyPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkDsKeyPolicyUntyped keyMint redeemer ctx =
  check $
    mkDsKeyPolicy
      (PlutusTx.unsafeFromBuiltinData keyMint)
      redeemer
      (Unsafe.wrap ctx)

-- | 'serialisableDsKeyPolicy' creates a serialisable version of the minting
-- policy (this is needed for ctl)
serialisableDsKeyPolicy :: Script
serialisableDsKeyPolicy = fromCompiledCode $$(PlutusTx.compile [||mkDsKeyPolicyUntyped||])
