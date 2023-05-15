-- | This module implements the required offchain functionality of the
-- | distributed set.
module TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , dsConf
  , DsDatum(DsDatum)
  , dsNext
  , DsConfDatum(DsConfDatum)
  , dscmTxOutRef
  , DsConfMint(DsConfMint)
  , DsKeyMint(DsKeyMint)
  , Node(Node)
  , Ib(Ib)
  , rootNode
  , mkNode
  , nodeToDatum
  , dsConfTokenName
  , insertValidator
  , dsConfValidator
  , dsConfPolicy
  , dsKeyPolicy
  , getDs
  , getDsKeyPolicy
  , findDsConfOutput
  , slowFindDsOutput
  , findDsOutput
  ) where

import Contract.Prelude

import Contract.Address (Address, NetworkId, getNetworkId)
import Contract.Address as Address
import Contract.AssocMap as AssocMap
import Contract.Log as Log
import Contract.Monad (Contract, liftContractM)
import Contract.Monad as Monad
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash
  )
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , outputDatumDatum
  )
import Contract.Utxos as Utxos
import Contract.Value (CurrencySymbol, TokenName, getTokenName, getValue)
import Contract.Value as Value
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), lift, runMaybeT)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.Utils.Logging as Logging
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )

-- * Types
-- For more information, see the on-chain Haskell file.

-- | `Ds` is the type which parameterizes the validator script for insertion in
-- | the distributed set.
newtype Ds = Ds CurrencySymbol

derive instance Generic Ds _

derive instance Newtype Ds _

derive newtype instance ToData Ds

derive newtype instance FromData Ds

-- | `dsConf` accesses the underlying `ByteArray` of `Ds`
dsConf ∷ Ds → CurrencySymbol
dsConf (Ds currencySymbol) = currencySymbol

-- | `DsDatum` is the datum for the validator script for insertion in the
-- | distributed set.
newtype DsDatum = DsDatum ByteArray

derive instance Generic DsDatum _

derive instance Newtype DsDatum _

derive newtype instance ToData DsDatum

derive newtype instance FromData DsDatum

-- | `dsNext` accesses the underlying `ByteArray` of `DsDatum`
dsNext ∷ DsDatum → ByteArray
dsNext (DsDatum byteArray) = byteArray

-- | `DsConfDatum` is the datum for the validator script which holds the
-- | configuration of the distributed set on chain i.e., this datum holds the
-- | necessary `CurrencySymbol`s for the functionality of the distributed set.
newtype DsConfDatum = DsConfDatum
  { dscKeyPolicy ∷ CurrencySymbol
  , dscFUELPolicy ∷ CurrencySymbol
  }

derive instance Generic DsConfDatum _

derive instance Newtype DsConfDatum _

instance FromData DsConfDatum where
  fromData (Constr n [ a, b ]) | n == BigNum.fromInt 0 =
    DsConfDatum <$>
      ({ dscKeyPolicy: _, dscFUELPolicy: _ } <$> fromData a <*> fromData b)
  fromData _ = Nothing

instance ToData DsConfDatum where
  toData (DsConfDatum { dscKeyPolicy, dscFUELPolicy }) =
    Constr (BigNum.fromInt 0) [ toData dscKeyPolicy, toData dscFUELPolicy ]

-- | `DsConfMint` is the type which paramaterizes the minting policy of the NFT
-- | which initializes the distributed set (i.e., the parameter for the
-- | minting policy that is the configuration of the distributed set).
newtype DsConfMint = DsConfMint TransactionInput

derive instance Generic DsConfMint _

derive instance Newtype DsConfMint _

derive newtype instance ToData DsConfMint

derive newtype instance FromData DsConfMint

-- | `dscmTxOutRef` accesses the underlying `TransactionInput` of `DsConfMint`
dscmTxOutRef ∷ DsConfMint → TransactionInput
dscmTxOutRef (DsConfMint transactionInput) = transactionInput

-- | `DsKeyMint` is the type which paramterizes the minting policy of the
-- | tokens which are keys in the distributed set.
newtype DsKeyMint = DsKeyMint
  { dskmValidatorHash ∷ ValidatorHash
  , dskmConfCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic DsKeyMint _

derive instance Newtype DsKeyMint _

instance FromData DsKeyMint where
  fromData (Constr n [ a, b ])
    | n == BigNum.fromInt 0 = DsKeyMint <$>
        ( { dskmValidatorHash: _, dskmConfCurrencySymbol: _ } <$> fromData a <*>
            fromData b
        )
  fromData _ = Nothing

instance ToData DsKeyMint where
  toData (DsKeyMint { dskmValidatorHash, dskmConfCurrencySymbol }) =
    Constr (BigNum.fromInt 0)
      [ toData dskmValidatorHash, toData dskmConfCurrencySymbol ]

-- | `Node` is an internal type to represent the nodes in the distributed set.
newtype Node = Node
  { nKey ∷ ByteArray
  , nNext ∷ ByteArray
  }

derive instance Generic Node _

derive instance Newtype Node _

-- | `mkNode` is a wrapper to create a Node from a string (a key) and the
-- | datum.
{-# INLINEABLE mkNode #-}
mkNode ∷ ByteArray → DsDatum → Node
mkNode str d =
  Node
    { nKey: str
    , nNext: dsNext d
    }

-- | Converts a `Node` to the correpsonding `DsDatum`
nodeToDatum ∷ Node → DsDatum
nodeToDatum (Node node) =
  DsDatum node.nNext

-- | `Ib` is the insertion buffer (abbr. Ib) is a fixed length array of how
-- | many new nodes (this is always 2, see `lengthIb`) are generated after
-- | inserting into a node.
newtype Ib a = Ib { unIb ∷ Tuple a a }

-- | `rootNode` is the initial node used when initializing the distributed set.
-- | It contains a min bound / max bound of the strings contained in the
-- | distributed set.
rootNode ∷ Node
rootNode = Node
  { nKey: ByteArray.byteArrayFromIntArrayUnsafe []
  , nNext: ByteArray.byteArrayFromIntArrayUnsafe (Array.replicate 33 255)
  -- Recall that blake2b_256 hashes are `256 bits = 32 bytes` long (8bits / byte),
  -- so an upper bound (ordering lexicographically -- the natural choice)
  -- is a list of just value 255 of length 33.
  -- TODO: actually, funny enough, we could choose the string
  -- ```
  -- [255, ... 255] ++ [0]
  -- ```
  -- where the `[255, ... 255]` is of length 32.
  -- This might be a bit more clean actually, since this really is
  -- supremum of the hashes as opposed to just an upper bound...
  -- BIG TODO: MAYBE WE CHANGE THIS LATER, AS IN I WOULD REALLY LIKE TO
  -- DO THIS CHANGE :^) but this would change the on chain code.

  -- And similarly, I suppose we could choose the infimum for the lower
  -- bound (this would be a minor essentially neglible performance
  -- penalty though) i.e, use the string
  -- ```
  -- [0,..,0]
  -- ```
  -- where `[0,..,0]` is of length 31.
  }

-- | `dsConfTokenName` is the `TokenName` for the token of the configuration.
-- | This doesn't matter, so we set it to be the empty string.
-- | Note: this corresponds to the Haskell function.
dsConfTokenName ∷ TokenName
dsConfTokenName = Unsafe.unsafePartial $ Maybe.fromJust $ Value.mkTokenName
  mempty

-- Note: this really *should* be safe to use the partial function here since the
-- empty TokenName is clearly a valid token. Clearly!

-- * Validator / minting policies

-- | `insertValidator` gets corresponding `insertValidator` from the serialized
-- | on chain code.
insertValidator ∷ Ds → Contract Validator
insertValidator ds = mkValidatorWithParams RawScripts.rawInsertValidator $ map
  toData
  [ ds ]

-- | `dsConfValidator` gets corresponding `dsConfValidator` from the serialized
-- | on chain code.
dsConfValidator ∷ Ds → Contract Validator
dsConfValidator ds = mkValidatorWithParams RawScripts.rawDsConfValidator $ map
  toData
  [ ds ]

-- | `dsConfPolicy` gets corresponding `dsConfPolicy` from the serialized
-- | on chain code.
dsConfPolicy ∷ DsConfMint → Contract MintingPolicy
dsConfPolicy dsm = mkMintingPolicyWithParams RawScripts.rawDsConfPolicy $ map
  toData
  [ dsm ]

-- | `dsKeyPolicy` gets corresponding `dsKeyPolicy` from the serialized
-- | on chain code.
dsKeyPolicy ∷ DsKeyMint → Contract MintingPolicy
dsKeyPolicy dskm = mkMintingPolicyWithParams RawScripts.rawDsKeyPolicy $ map
  toData
  [ dskm ]

-- | The address for the insert validator of the distributed set.
insertAddress ∷ NetworkId → Ds → Contract Address
insertAddress netId ds = do
  v ← insertValidator ds
  liftContractM "Couldn't derive distributed set insert validator address"
    $ Address.validatorHashEnterpriseAddress netId (Scripts.validatorHash v)

-- * ToData / FromData instances.
-- These should correspond to the on-chain Haskell types.

dsToDsKeyMint ∷ Ds → Contract DsKeyMint
dsToDsKeyMint ds = do
  insertValidator' ← insertValidator ds

  let insertValidatorHash = Scripts.validatorHash insertValidator'

  pure $ DsKeyMint
    { dskmValidatorHash: insertValidatorHash
    , dskmConfCurrencySymbol: dsConf ds
    }

-- | `insertNode str node` inserts returns the new nodes which should be
-- | created (in place of the old `node`) provided that `str` can actually be
-- | inserted here (i.e., `str` must be strictly between `nKey` and `nNext` of `node`).
-- |
-- | Note: the first projection of `Ib` will always be the node which should
-- | replace `node`, which also should be the node which is strictly less than
-- | `str`.
-- | Note: this copies the onchain Haskell function.
{-# INLINEABLE insertNode #-}
insertNode ∷ ByteArray → Node → Maybe (Ib Node)
insertNode str (Node node)
  | node.nKey < str && str < node.nNext =
      Just $
        Ib
          { unIb:
              ( Node (node { nNext = str }) /\ Node
                  { nKey: str, nNext: node.nNext }
              )
          }
  | otherwise = Nothing

-- | `getDs` grabs the `Ds` type given `TransactionInput`. Often, the
-- | `TransactionInput` should be the `genesisUtxo` of a given `SidechainParams`
getDs ∷ TransactionInput → Contract Ds
getDs txInput = do
  let
    msg = Logging.mkReport
      { mod: "DistributedSet", fun: "getDs" }

  dsConfPolicy' ← dsConfPolicy $ DsConfMint txInput
  dsConfPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsConfPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol dsConfPolicy'
  pure $ Ds dsConfPolicyCurrencySymbol

-- | `getDsKeyPolicy` grabs the key policy and currency symbol from the given
-- | `TransactionInput` (potentially throwing an error in the case that it is
-- | not possible). Often, the `TransactionInput` should be the `genesisUtxo`
-- | of a given `SidechainParams`.
getDsKeyPolicy ∷
  Ds →
  Contract
    { dsKeyPolicy ∷ MintingPolicy, dsKeyPolicyCurrencySymbol ∷ CurrencySymbol }
getDsKeyPolicy ds = do
  let
    msg = Logging.mkReport
      { mod: "DistributedSet", fun: "getDsKeyPolicy" }

  insertValidator' ← insertValidator ds

  let
    insertValidatorHash = Scripts.validatorHash insertValidator'
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConf ds
      }
  policy ← dsKeyPolicy dskm

  currencySymbol ←
    liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol policy

  pure { dsKeyPolicy: policy, dsKeyPolicyCurrencySymbol: currencySymbol }

-- | `findDsConfOutput` finds the (unique) utxo (as identified by an NFT) which
-- | holds the configuration of the distributed set.
findDsConfOutput ∷
  Ds →
  Contract
    { confRef ∷ TransactionInput
    , confO ∷ TransactionOutputWithRefScript
    , confDat ∷ DsConfDatum
    }
findDsConfOutput ds = do
  let msg = Logging.mkReport { mod: "DistributedSet", fun: "findDsConfOutput" }

  netId ← getNetworkId
  v ← dsConfValidator ds
  scriptAddr ←
    liftContractM
      "Couldn't derive distributed set configuration validator address"
      $ Address.validatorHashEnterpriseAddress netId (Scripts.validatorHash v)

  utxos ← Utxos.utxosAt scriptAddr

  out ←
    liftContractM
      (msg "Distributed Set config utxo does not contain oneshot token")
      $ Array.find
          ( \(_ /\ TransactionOutputWithRefScript o) → not $ null
              $ AssocMap.lookup (dsConf ds)
              $ getValue
                  (unwrap o.output).amount
          )
      $ Map.toUnfoldable utxos

  confDat ←
    liftContractM (msg "Couldn't find Distributed Set configuration datum")
      $ outputDatumDatum (unwrap (unwrap (snd out)).output).datum
      >>= (fromData <<< unwrap)
  pure
    { confRef: fst out
    , confO: snd out
    , confDat
    }

-- | `findDsOutput` finds the transaction which we must insert to
-- | (if it exists) for the distributed set from the given `TransactionInput`. It
-- | returns:
-- |
-- |    - the `TransactionInput` of the output to spend (i.e., the provided
-- |    input);
-- |    - the transaction output information;
-- |    - the datum at that utxo to spend;
-- |    - the `TokenName` of the key of the utxo we want to spend; and
-- |    - the new nodes to insert (after replacing the given node)
-- |
-- | See `slowFindDsOutput` for an alternative lookup function which is much
-- | slower!
findDsOutput ∷
  Ds →
  TokenName →
  TransactionInput →
  Contract
    { inUtxo ∷
        { nodeRef ∷ TransactionInput
        , oNode ∷ TransactionOutputWithRefScript
        , datNode ∷ DsDatum
        , tnNode ∷ TokenName
        }
    , nodes ∷ Ib Node
    }
findDsOutput ds tn txInput = do
  let msg = Logging.mkReport { mod: "DistributedSet", fun: "findDsOutput" }

  txOut ← Monad.liftedM (msg "failed to find provided distributed set UTxO") $
    Utxos.getUtxo txInput

  { dsKeyPolicyCurrencySymbol } ← getDsKeyPolicy ds

  --  Grab the datum
  dat ← liftContractM (msg "datum not a distributed set node")
    $ outputDatumDatum (unwrap txOut).datum
    >>= (fromData <<< unwrap)

  --  Validate that this is a distributed set node / grab the necessary
  -- information about it
  -- `tn'` is the distributed set node onchain.
  tn' ← do
    netId ← getNetworkId
    scriptAddr ← insertAddress netId ds

    unless
      (scriptAddr == (unwrap txOut).address)
      $ Monad.throwContractError
      $ msg "provided transaction is not at distributed set node address"

    keyNodeTn ← liftContractM
      (msg "missing token name in distributed set node")
      do
        tns ← AssocMap.lookup dsKeyPolicyCurrencySymbol $ getValue
          (unwrap txOut).amount
        Array.head $ AssocMap.keys tns

    pure keyNodeTn

  nodes ←
    Monad.liftContractM
      ( msg
          "invalid distributed set node provided \
          \(the provided node must satisfy `providedNode` < `newNode` < `next`) \
          \but got `providedNode` "
          <> show (getTokenName tn')
          <> ", `newNode` "
          <> show (getTokenName tn)
          <> ", and `next` "
          <> show (unwrap dat)
      ) $ insertNode (getTokenName tn) $ mkNode
      (getTokenName tn')
      dat

  pure
    { inUtxo:
        { nodeRef: txInput
        , oNode:
            TransactionOutputWithRefScript
              { output: txOut
              , scriptRef: Nothing
              -- There shouldn't be a script ref for this...
              }
        , datNode: dat
        , tnNode: tn'
        }
    , nodes
    }

-- | `slowFindDsOutput` finds the transaction which we must insert to
-- | (if it exists) for the distributed set. It returns:
-- |
-- |    - the `TransactionInput` of the output to spend;
-- |    - the transaction output information;
-- |    - the datum at that utxo to spend;
-- |    - the `TokenName` of the key of the utxo we want to spend; and
-- |    - the new nodes to insert (after replacing the given node)
-- |
-- | Note: this is linear in the size of the distributed set... one should maintain
-- | an efficient offchain index of the utxos, and set up the appropriate actions
-- | when the list gets updated by someone else.
slowFindDsOutput ∷
  Ds →
  TokenName →
  Contract
    ( Maybe
        { inUtxo ∷
            { nodeRef ∷ TransactionInput
            , oNode ∷ TransactionOutputWithRefScript
            , datNode ∷ DsDatum
            , tnNode ∷ TokenName
            }
        , nodes ∷ Ib Node
        }
    )
slowFindDsOutput ds tn = do
  Log.logWarn'
    "Finding the required distributed set node (this may take a while)..."

  netId ← getNetworkId
  scriptAddr ← insertAddress netId ds
  utxos ← Utxos.utxosAt scriptAddr
  go $ Map.toUnfoldable utxos

  where

  go utxos' =
    case Array.uncons utxos' of
      Nothing → pure Nothing
      Just { head: ref /\ TransactionOutputWithRefScript o, tail } →
        let
          c = runMaybeT do
            dskm ← lift $ dsToDsKeyMint ds
            policy ← lift $ dsKeyPolicy dskm

            currencySymbol ← hoistMaybe $ Value.scriptCurrencySymbol policy

            dat ← hoistMaybe $ outputDatumDatum (unwrap o.output).datum >>=
              (fromData <<< unwrap)

            tns ←
              hoistMaybe $ AssocMap.lookup currencySymbol
                $ getValue (unwrap o.output).amount

            tn' ← hoistMaybe $ Array.head $ AssocMap.keys tns

            nodes ← hoistMaybe $ insertNode (getTokenName tn) $ mkNode
              (getTokenName tn')
              dat

            pure $
              Just
                { inUtxo:
                    { nodeRef: ref, oNode: wrap o, datNode: dat, tnNode: tn' }
                , nodes
                }
        in
          c >>= case _ of
            Nothing → go tail
            Just r → pure $ r

hoistMaybe ∷
  ∀ (m ∷ Type → Type) (b ∷ Type).
  Applicative m ⇒
  Maybe b →
  MaybeT m b
hoistMaybe = MaybeT <<< pure
