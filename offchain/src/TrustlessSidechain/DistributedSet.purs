-- | This module implements the required offchain functionality of the
-- | distributed set.
module TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , dsConf
  , DsDatum(DsDatum)
  , dsNext
  , DsConfDatum(DsConfDatum)
  , DsKeyMint(DsKeyMint)
  , Node(Node)
  , Ib(Ib)
  , rootNode
  , mkNode
  , nodeToDatum
  , dsConfTokenName
  , insertValidator
  , dsConfValidator
  , dsConfCurrencyInfo
  , dsKeyPolicy
  , dsInitTokenName
  , getDs
  , getDsKeyPolicy
  , findDsConfOutput
  , slowFindDsOutput
  , findDsOutput
  , mintOneDsInitToken
  , burnOneDsInitToken
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Data.Map as Map

import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.ScriptLookups (ScriptLookups)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionInput (TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Cardano.Types.Value as Value
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Data.Array as Array
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logWarn') as Effect
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (getUtxo, utxosAt) as Effect
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
      ( NotFoundUtxo
      , ConversionError
      , InvalidData
      , GenericInternalError
      , DsInsertError
      )
  )
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  , mintOneInitToken
  )
import Cardano.Types.AssetName (AssetName, unAssetName)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address
  ( getCurrencyInfo
  , toAddress
  )
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( DsInsertValidator
      , DsKeyPolicy
      , DsConfPolicy
      , DsConfValidator
      )
  )
import Type.Row (type (+))
import Cardano.Types.Value (getMultiAsset)
import TrustlessSidechain.Utils.Asset (emptyAssetName, unsafeMkAssetName)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.OutputDatum (outputDatumDatum)

-- * Types
-- For more information, see the on-chain Haskell file.

-- | `Ds` is the type which parameterizes the validator script for insertion in
-- | the distributed set.
newtype Ds = Ds ScriptHash

derive instance Generic Ds _

derive instance Newtype Ds _

derive newtype instance ToData Ds

derive newtype instance FromData Ds

derive newtype instance Eq Ds

instance Show Ds where
  show = genericShow

-- | `dsConf` accesses the underlying `ByteArray` of `Ds`
dsConf ∷ Ds → ScriptHash
dsConf (Ds currencySymbol) = currencySymbol

-- | `DsDatum` is the datum for the validator script for insertion in the
-- | distributed set.
newtype DsDatum = DsDatum ByteArray

derive instance Generic DsDatum _

derive instance Newtype DsDatum _

derive newtype instance ToData DsDatum

derive newtype instance FromData DsDatum

derive newtype instance Eq DsDatum

instance Show DsDatum where
  show = genericShow

-- | `dsNext` accesses the underlying `ByteArray` of `DsDatum`
dsNext ∷ DsDatum → ByteArray
dsNext (DsDatum byteArray) = byteArray

-- | `DsConfDatum` is the datum for the validator script which holds the
-- | configuration of the distributed set on chain i.e., this datum holds the
-- | necessary `CurrencySymbol`s for the functionality of the distributed set.
newtype DsConfDatum = DsConfDatum
  { dscKeyPolicy ∷ ScriptHash
  , dscFUELPolicy ∷ ScriptHash
  }

derive instance Generic DsConfDatum _

derive instance Newtype DsConfDatum _

instance FromData DsConfDatum where
  fromData = productFromData2
    ( \x y → DsConfDatum
        { dscKeyPolicy: x
        , dscFUELPolicy: y
        }
    )

derive newtype instance Eq DsConfDatum

instance Show DsConfDatum where
  show = genericShow

instance ToData DsConfDatum where
  toData (DsConfDatum { dscKeyPolicy, dscFUELPolicy }) =
    productToData2 dscKeyPolicy dscFUELPolicy

-- | `DsKeyMint` is the type which paramterizes the minting policy of the
-- | tokens which are keys in the distributed set.
newtype DsKeyMint = DsKeyMint
  { dskmValidatorHash ∷ ScriptHash
  , dskmConfCurrencySymbol ∷ ScriptHash
  }

derive instance Generic DsKeyMint _

derive instance Newtype DsKeyMint _

instance FromData DsKeyMint where
  fromData = productFromData2
    ( \x y → DsKeyMint
        { dskmValidatorHash: x
        , dskmConfCurrencySymbol: y
        }
    )

instance ToData DsKeyMint where
  toData (DsKeyMint { dskmValidatorHash, dskmConfCurrencySymbol }) =
    productToData2 dskmValidatorHash dskmConfCurrencySymbol

derive newtype instance Eq DsKeyMint

instance Show DsKeyMint where
  show = genericShow

-- | `Node` is an internal type to represent the nodes in the distributed set.
newtype Node = Node
  { nKey ∷ ByteArray
  , nNext ∷ ByteArray
  }

derive instance Generic Node _

derive instance Newtype Node _

instance ToData Node where
  toData (Node { nKey, nNext }) =
    productToData2 nKey nNext

instance FromData Node where
  fromData = productFromData2 $ \nKey nNext →
    Node
      { nKey
      , nNext
      }

derive newtype instance Eq Node

instance Show Node where
  show = genericShow

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

-- | A name for the distributed set initialization token.  Must be unique among
-- | initialization tokens.
dsInitTokenName ∷ AssetName
dsInitTokenName = unsafeMkAssetName "DistributedSet InitToken"

-- | `dsConfTokenName` is the `TokenName` for the token of the configuration.
-- | This doesn't matter, so we set it to be the empty string.
-- | Note: this corresponds to the Haskell function.
dsConfTokenName ∷ AssetName
dsConfTokenName = emptyAssetName

-- Note: this really *should* be safe to use the partial function here since the
-- empty AssetName is clearly a valid token. Clearly!

-- * Validator / minting policies

-- | `insertValidator` gets corresponding `insertValidator` from the serialized
-- | on chain code.
insertValidator ∷ ∀ r. Ds → Run (EXCEPT OffchainError + r) PlutusScript
insertValidator ds = mkValidatorWithParams DsInsertValidator $ map
  toData
  [ ds ]

-- | `dsConfValidator` gets corresponding `dsConfValidator` from the serialized
-- | on chain code.
dsConfValidator ∷ ∀ r. Ds → Run (EXCEPT OffchainError + r) PlutusScript
dsConfValidator ds = mkValidatorWithParams DsConfValidator $ map
  toData
  [ ds ]

-- | `dsConfPolicy` gets corresponding `dsConfPolicy` from the serialized
-- | on chain code.
dsConfCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
dsConfCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: dsInitTokenName
      }
  getCurrencyInfo DsConfPolicy [ toData itac ]

-- | `dsKeyPolicy` gets corresponding `dsKeyPolicy` from the serialized
-- | on chain code.
dsKeyPolicy ∷ ∀ r. DsKeyMint → Run (EXCEPT OffchainError + r) PlutusScript
dsKeyPolicy dskm = mkMintingPolicyWithParams DsKeyPolicy [ toData dskm ]

-- | The address for the insert validator of the distributed set.
insertAddress ∷ ∀ r. Ds → Run (EXCEPT OffchainError + WALLET + r) Address
insertAddress ds = do
  v ← insertValidator ds
  toAddress (PlutusScript.hash v)

-- * ToData / FromData instances.
-- These should correspond to the on-chain Haskell types.

dsToDsKeyMint ∷ ∀ r. Ds → Run (EXCEPT OffchainError + r) DsKeyMint
dsToDsKeyMint ds = do
  insertValidator' ← insertValidator ds

  let insertValidatorHash = PlutusScript.hash insertValidator'

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
getDs ∷ ∀ r. SidechainParams → Run (EXCEPT OffchainError + r) Ds
getDs sp = do
  { currencySymbol } ← dsConfCurrencyInfo sp
  pure $ Ds currencySymbol

-- | `getDsKeyPolicy` grabs the key policy and currency symbol from the given
-- | `TransactionInput` (potentially throwing an error in the case that it is
-- | not possible). Often, the `TransactionInput` should be the `genesisUtxo`
-- | of a given `SidechainParams`.
getDsKeyPolicy ∷
  ∀ r.
  Ds →
  Run (EXCEPT OffchainError + r) CurrencyInfo
getDsKeyPolicy ds = do
  insertValidator' ← insertValidator ds

  let
    insertValidatorHash = PlutusScript.hash insertValidator'
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConf ds
      }
  mintingPolicy ← dsKeyPolicy dskm

  let currencySymbol = PlutusScript.hash mintingPolicy

  pure { mintingPolicy, currencySymbol }

-- | Build lookups and constraints to mint distributed set initialization token.
mintOneDsInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mintOneDsInitToken sp =
  mintOneInitToken sp dsInitTokenName

-- | Build lookups and constraints to burn distributed set initialization token.
burnOneDsInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
burnOneDsInitToken sp =
  burnOneInitToken sp dsInitTokenName

-- | `findDsConfOutput` finds the (unique) utxo (as identified by an NFT) which
-- | holds the configuration of the distributed set.
findDsConfOutput ∷
  ∀ r.
  Ds →
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { confRef ∷ TransactionInput
    , confO ∷ TransactionOutput
    , confDat ∷ DsConfDatum
    }
findDsConfOutput ds = do
  v ← dsConfValidator ds
  scriptAddr ← toAddress (PlutusScript.hash v)
  utxos ← Effect.utxosAt scriptAddr

  out ←
    Run.note
      ( ( NotFoundUtxo
            "Distributed Set config utxo does not contain oneshot token"
        )
      )
      $ Array.find
          ( \(_ /\ TransactionOutput {amount}) → not $ null
              $ Map.lookup (dsConf ds)
              $ unwrap
              $ Value.getMultiAsset amount
          )
      $ Map.toUnfoldable utxos

  confDat ←
    Run.note
      ( NotFoundUtxo "Distributed Set config utxo does not contain datum"
      )
      $ (unwrap (snd out)).datum >>= outputDatumDatum >>= fromData

  pure
    { confRef: fst out
    , confO: snd out
    , confDat
    }

-- | `findDsOutput` finds the transaction which we must insert to (if it exists)
-- | for the distributed set from the given `TransactionInput`. It returns:
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
  ∀ r.
  Ds →
  AssetName →
  TransactionInput →
  Run (APP + r)
    { inUtxo ∷
        { nodeRef ∷ TransactionInput
        , oNode ∷ TransactionOutput
        , datNode ∷ DsDatum
        , tnNode ∷ AssetName
        }
    , nodes ∷ Ib Node
    }
findDsOutput ds tn txInput = do
  txOut ←
    Effect.fromMaybeThrow
      (NotFoundUtxo "Failed to find provided distributed set UTxO") $
      Effect.getUtxo txInput

  { currencySymbol: dsKeyPolicyCurrencySymbol } ← getDsKeyPolicy ds

  --  Grab the datum
  dat ←
    Run.note
      (ConversionError "datum not a distributed set node")
      $ (unwrap txOut).datum >>= outputDatumDatum >>= fromData

  --  Validate that this is a distributed set node / grab the necessary
  -- information about it
  -- `tn'` is the distributed set node onchain.
  tn' ← do
    scriptAddr ← insertAddress ds

    unless
      (scriptAddr == (unwrap txOut).address)
      $ Run.throw
      $ GenericInternalError
      $ "Provided Distributed Set UTxO is not at the correct address. "
      <> "It should sent output to address "
      <> show (unwrap txOut).address
      <> " but sends to "
      <> show scriptAddr
      <> " instead."

    keyNodeTn ← Run.note
      ( InvalidData "missing token name in distributed set node"
      )
      do
        tns ← Map.lookup dsKeyPolicyCurrencySymbol $ unwrap $ getMultiAsset
          (unwrap txOut).amount
        Array.head $ Array.fromFoldable $ Map.keys tns

    pure keyNodeTn

  nodes ←
    Run.note
      ( DsInsertError
          ( "invalid distributed set node provided \
            \(the provided node must satisfy `providedNode` < `newNode` < `next`) \
            \but got `providedNode` "
              <> show (unAssetName tn')
              <> ", `newNode` "
              <> show (unAssetName tn)
              <> ", and `next` "
              <> show (unwrap dat)

          )
      ) $ insertNode (unAssetName tn) $ mkNode
      (unAssetName tn')
      dat

  pure
    { inUtxo:
        { nodeRef: txInput
        , oNode: txOut
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
  ∀ r.
  Ds →
  AssetName →
  Run (APP + r)
    ( Maybe
        { inUtxo ∷
            { nodeRef ∷ TransactionInput
            , oNode ∷ TransactionOutput
            , datNode ∷ DsDatum
            , tnNode ∷ AssetName
            }
        , nodes ∷ Ib Node
        }
    )
slowFindDsOutput ds tn = do
  Effect.logWarn'
    "Finding the required distributed set node (this may take a while)..."

  scriptAddr ← insertAddress ds
  utxos ← Map.toUnfoldable <$> Effect.utxosAt scriptAddr
  dskm ← dsToDsKeyMint ds
  policy ← dsKeyPolicy dskm

  let dsKeyCurSym = PlutusScript.hash policy

  go dsKeyCurSym utxos

  where

  go dsKeyCurSym utxos' =
    case Array.uncons utxos' of
      Nothing → pure Nothing
      Just { head: ref /\ o, tail } →
        do
          dsKey ← runMaybeT do
            dat ← hoistMaybe $ (unwrap o).datum >>= outputDatumDatum >>= fromData

            tns ←
              hoistMaybe $ Map.lookup dsKeyCurSym
                $ unwrap $ getMultiAsset (unwrap o).amount

            tn' ← hoistMaybe $ Array.head $ Array.fromFoldable $ Map.keys tns

            nodes ← hoistMaybe $ insertNode (unAssetName tn) $ mkNode
              (unAssetName tn')
              dat

            pure $
              Just
                { inUtxo:
                    { nodeRef: ref, oNode: o, datNode: dat, tnNode: tn' }
                , nodes
                }

          maybe (go dsKeyCurSym tail) pure dsKey

hoistMaybe ∷
  ∀ (m ∷ Type → Type) (b ∷ Type).
  Applicative m ⇒
  Maybe b →
  MaybeT m b
hoistMaybe = MaybeT <<< pure
