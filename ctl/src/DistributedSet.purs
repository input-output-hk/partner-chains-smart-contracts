-- | This module implements the required offchain functionality of the
-- distributed set.
module DistributedSet
  ( Ds(Ds)
  , DsDatum(DsDatum)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)
  , DsKeyMint(DsKeyMint)
  , Node(Node)
  , Ib(Ib)
  , rootNode
  , dsConfTokenName

  , insertValidator
  , dsConfValidator
  , dsConfPolicy
  , dsKeyPolicy
  , getDsKeyPolicy
  , findDsOutput
  ) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.Address (Address, NetworkId, getNetworkId)
import Contract.Address as Address
import Contract.AssocMap as AssocMap
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Monad as Monad
import Contract.PlutusData (PlutusData(..))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash)
import Contract.Scripts as Scripts
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction
  ( Language(PlutusV2)
  , TransactionInput
  , TransactionOutput
  , TransactionOutputWithRefScript(..)
  , outputDatumDatum
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, getTokenName)
import Contract.Value as Value
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import FromData (class FromData, fromData)
import Partial.Unsafe as Unsafe
import Plutus.Types.Value (getValue)
import RawScripts as RawScripts
import SidechainParams (SidechainParams(..))
import ToData (class ToData, toData)
import Utils.Logging as Logging

-- * Types
-- $types
-- For more information, see the on-chain Haskell file.

-- | 'Ds' is the type which parameterizes the validator script for insertion in
-- the distributed set.
newtype Ds = Ds { dsConf ∷ CurrencySymbol }

-- | 'DsDatum' is the datum for the validator script for insertion in the
-- distributed set.
newtype DsDatum = DsDatum { dsNext ∷ ByteArray }

-- | 'DsConfDatum' is the datum for the validator script which holds the
-- configuration of the distributed set on chain i.e., this datum holds the
-- necessary 'CurrencySymbol' for the functionality of the distributed set.
newtype DsConfDatum = DsConfDatum
  { dscKeyPolicy ∷ CurrencySymbol
  , dscFUELPolicy ∷ CurrencySymbol
  }

-- | 'DsConfMint' is the type which paramaterizes the minting policy of the NFT
-- which initializes the distribted set (i.e., the parameter for the configuration of the distributed set).
newtype DsConfMint = DsConfMint { dscmTxOutRef ∷ TransactionInput }

-- | 'DsKeyMint' is the type which paramterizes the minting policy of the
-- tokens which are keys in the distributed set.
newtype DsKeyMint = DsKeyMint
  { dskmValidatorHash ∷ ValidatorHash
  , dskmConfCurrencySymbol ∷ CurrencySymbol
  }

-- | 'Node' is an internal type to realize the nodes in the distributed set.
newtype Node = Node
  { nKey ∷ ByteArray
  , nNext ∷ ByteArray
  }

-- | 'mkNode' is a wrapper to create a Node from a prefix and the datum.
{-# INLINEABLE mkNode #-}
mkNode ∷ ByteArray → DsDatum → Node
mkNode str (DsDatum d) =
  Node
    { nKey: str
    , nNext: d.dsNext
    }

{- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
 length "array" of how many new nodes (this is always 2, see 'lengthIb') are
 generated after inserting into a node.
-}
newtype Ib a = Ib { unIb ∷ Tuple a a }

-- | 'rootNode' is the initial node used when initializing the distributed set.
-- It contains the min bound / max bound of the strings contained in the
-- distributed set.
rootNode ∷ Node
rootNode = Node
  { nKey: ByteArray.byteArrayFromIntArrayUnsafe []
  , nNext: ByteArray.byteArrayFromIntArrayUnsafe (Array.replicate 33 255)
  -- Recall that blake2b_256 hashes are @256 bits = 32 bytes@ long (8bits / byte),
  -- so an upper bound (ordering lexicographically -- the natural choice)
  -- is a list of just value 255 of length 33.
  -- TODO: actually, funny enough, we could choose the string
  -- > [255, ... 255] ++ [0]
  -- where the @[255, ... 255]@ is of length 32.
  -- This might be a bit more clean actually, since this really is
  -- supremum of the hashes as opposed to just an upper bound...
  -- BIG TODO: MAYBE WE CHANGE THIS LATER, AS IN I WOULD REALLY LIKE TO
  -- DO THIS CHANGE :^) but this would change the on chain code.

  -- And similarly, I suppose we could choose the infimum for the lower
  -- bound (this would be a minor essentially neglible performance
  -- penalty though) i.e, use the string
  -- > [0,..,0]
  -- where [0,..,0] is of length 31.
  }

-- | 'dsConfTokenName' is the TokenName for the token of the configuration.
-- This corresponds to the Haskell function.
dsConfTokenName ∷ TokenName
dsConfTokenName = Unsafe.unsafePartial $ Maybe.fromJust $ Value.mkTokenName
  mempty

-- Note: this really *should* be safe to use the partial function here since the
-- empty TokenName is clearly a valid token. Clearly!

derive instance Generic Ds _
derive instance Newtype Ds _
derive instance Generic DsDatum _
derive instance Newtype DsDatum _
derive instance Generic DsConfDatum _
derive instance Newtype DsConfDatum _
derive instance Generic DsConfMint _
derive instance Newtype DsConfMint _
derive instance Generic DsKeyMint _
derive instance Newtype DsKeyMint _
derive instance Generic Node _
derive instance Newtype Node _

-- * Validator / minting policies

-- | @'mkValidatorParams' hexScript params@ returns the 'Validator' of @hexScript@
-- with the script applied to @params@. This is a convenient alias
-- to help create the distributed set validators.
--
-- TODO: not too sure what this does in the case when @params@ is empty list?
-- Internally, this uses 'Contract.Scripts.applyArgs'.
mkValidatorParams ∷ String → Array PlutusData → Contract () Validator
mkValidatorParams hexScript params = do
  validatorBytes ← TextEnvelope.textEnvelopeBytes hexScript
    PlutusScriptV2
  let validatorUnapplied = wrap $ wrap $ validatorBytes /\ PlutusV2 ∷ Validator
  Monad.liftedE $ Scripts.applyArgs validatorUnapplied $ params

-- | @'mkMintingPolicyParams' hexScript params@ returns the 'MintingPolicy' of @hexScript@
-- with the script applied to @params@. This is a convenient alias
-- to help create the distributed set minting policies.
--
-- TODO: not too sure what this does in the case when @params@ is empty list?
-- Internally, this uses 'Contract.Scripts.applyArgs'.
mkMintingPolicyParams ∷ String → Array PlutusData → Contract () MintingPolicy
mkMintingPolicyParams hexScript params = do
  policyBytes ← TextEnvelope.textEnvelopeBytes hexScript PlutusScriptV2
  let policyUnapplied = wrap $ wrap $ policyBytes /\ PlutusV2 ∷ MintingPolicy
  Monad.liftedE $ Scripts.applyArgs policyUnapplied params

-- | 'insertValidator' gets corresponding 'insertValidator' from the serialized
-- on chain code.
insertValidator ∷ Ds → Contract () Validator
insertValidator ds = mkValidatorParams RawScripts.rawInsertValidator $ map
  toData
  [ ds ]

-- | 'dsConfValidator' gets corresponding 'dsConfValidator' from the serialized
-- on chain code.
dsConfValidator ∷ Ds → Contract () Validator
dsConfValidator ds = mkValidatorParams RawScripts.rawDsConfValidator $ map
  toData
  [ ds ]

-- | 'dsConfPolicy' gets corresponding 'dsConfPolicy' from the serialized
-- on chain code.
dsConfPolicy ∷ DsConfMint → Contract () MintingPolicy
dsConfPolicy dsm = mkMintingPolicyParams RawScripts.rawDsConfPolicy $ map toData
  [ dsm ]

-- | 'dsKeyPolicy' gets corresponding 'dsKeyPolicy' from the serialized
-- on chain code.
dsKeyPolicy ∷ DsKeyMint → Contract () MintingPolicy
dsKeyPolicy dskm = mkMintingPolicyParams RawScripts.rawDsKeyPolicy $ map toData
  [ dskm ]

-- | The address for the distributed set.
insertAddress ∷ NetworkId → Ds → Contract () Address
insertAddress netId ds = do
  v ← insertValidator ds
  liftContractM "Couldn't derive distributed set insert validator address"
    $ Address.validatorHashEnterpriseAddress netId (Scripts.validatorHash v)

-- * ToData / FromData instances.
-- These should correspond to the on-chain Haskell types.

instance FromData Ds where
  fromData (Constr n [ a ]) | n == zero = Ds <<< { dsConf: _ } <$> fromData a
  fromData _ = Nothing

instance ToData Ds where
  toData (Ds { dsConf }) = Constr zero [ toData dsConf ]

instance FromData DsDatum where
  fromData (Constr n [ a ]) | n == zero = DsDatum <<< { dsNext: _ } <$> fromData
    a
  fromData _ = Nothing

instance ToData DsDatum where
  toData (DsDatum { dsNext }) = Constr zero [ toData dsNext ]

instance FromData DsConfDatum where
  fromData (Constr n [ a, b ]) | n == zero =
    DsConfDatum <$>
      ({ dscKeyPolicy: _, dscFUELPolicy: _ } <$> fromData a <*> fromData b)
  fromData _ = Nothing

instance ToData DsConfDatum where
  toData (DsConfDatum { dscKeyPolicy, dscFUELPolicy }) = Constr zero
    [ toData dscKeyPolicy, toData dscFUELPolicy ]

instance FromData DsConfMint where
  fromData (Constr n [ a ])
    | n == zero = DsConfMint <<< { dscmTxOutRef: _ } <$> fromData a
  fromData _ = Nothing

instance ToData DsConfMint where
  toData (DsConfMint { dscmTxOutRef }) = Constr zero [ toData dscmTxOutRef ]

instance FromData DsKeyMint where
  fromData (Constr n [ a, b ])
    | n == zero = DsKeyMint <$>
        ( { dskmValidatorHash: _, dskmConfCurrencySymbol: _ } <$> fromData a <*>
            fromData b
        )
  fromData _ = Nothing

instance ToData DsKeyMint where
  toData (DsKeyMint { dskmValidatorHash, dskmConfCurrencySymbol }) = Constr zero
    [ toData dskmValidatorHash, toData dskmConfCurrencySymbol ]

dsToDsKeyMint ∷ Ds → Contract () DsKeyMint
dsToDsKeyMint ds = do
  insertValidator' ← insertValidator ds

  let insertValidatorHash = Scripts.validatorHash insertValidator'

  pure $ DsKeyMint
    { dskmValidatorHash: insertValidatorHash
    , dskmConfCurrencySymbol: (unwrap ds).dsConf
    }

{- | `'insertNode' str node` inserts returns the new nodes which should be
 created (in place of the old @node@) provided that @str@ can actually be
 inserted here. See Note [How This All Works].

 Note that the first projection of 'Ib' will always be the node which should
 replace @node@, which also should be the node which is strictly less than
 @str@. This property is helpful in 'mkInsertValidator' when verifying that the
 nodes generated are as they should be.
-}
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

-- | 'getDsKeyPolicy' grabs the committee hash policy and currency symbol
-- (potentially throwing an error in the case that it is not possible).
getDsKeyPolicy ∷
  SidechainParams →
  Contract ()
    { dsKeyPolicy ∷ MintingPolicy, dsKeyPolicyCurrencySymbol ∷ CurrencySymbol }
getDsKeyPolicy (SidechainParams sp) = do
  let
    msg = Logging.mkReport
      { mod: "DistributedSet", fun: "getDsKeyPolicy" }

  dsConfPolicy' ← dsConfPolicy $ DsConfMint
    { dscmTxOutRef: sp.genesisUtxo }
  dsConfPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsConfPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol dsConfPolicy'
  let ds = Ds { dsConf: dsConfPolicyCurrencySymbol }

  insertValidator' ← insertValidator ds

  let
    insertValidatorHash = Scripts.validatorHash insertValidator'
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConfPolicyCurrencySymbol
      }
  policy ← dsKeyPolicy dskm

  currencySymbol ←
    liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol policy

  pure { dsKeyPolicy: policy, dsKeyPolicyCurrencySymbol: currencySymbol }

{- | 'findDsOutput' finds the transaction which we must insert to
 (if it exists) for the distributed set. It returns
 the 'TransactionInput' of the output to spend, the transaction output information, the datum
 at that utxo to spend, and the 'TokenName' of the key of the utxo we want to
 spend; and finally the new nodes to insert (after replacing the given node)

 N.B. this is linear in the size of the distributed set... one should maintain
 an efficient offchain index of the utxos, and set up the appropriate actions
 when the list gets updated by someone else.
-}
findDsOutput ∷
  Ds →
  TokenName →
  Contract ()
    ( Maybe
        { inUtxo ∷
            { txIn ∷ TransactionInput
            , txOut ∷ TransactionOutput
            , dsDatum ∷ DsDatum
            , dsTokenName ∷ TokenName
            }
        , inputBuffer ∷ Ib Node
        }
    )
findDsOutput ds tn = do
  netId ← getNetworkId
  scriptAddr ← insertAddress netId ds
  utxos ← liftedM (msg "Cannot get Distributed Set UTxOs") (utxosAt scriptAddr)
  go $ Map.toUnfoldable utxos

  where

  msg = Logging.mkReport { mod: "DistributedSet", fun: "findDsOutput" }

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

            nnodes ← hoistMaybe $ insertNode (getTokenName tn) $ mkNode
              (getTokenName tn')
              dat

            pure $
              Just
                { inUtxo:
                    { txIn: ref, txOut: o.output, dsDatum: dat, dsTokenName: tn' }
                , inputBuffer: nnodes
                }
        in
          c >>= case _ of
            Nothing → go tail
            Just r → pure $ r

hoistMaybe ∷ ∀ m b. Applicative m ⇒ Maybe b → MaybeT m b
hoistMaybe = MaybeT <<< pure
