-- | This module implements the required offchain functionality of the
-- distributed set.
module DistributedSet
  ( Ds(Ds)
  , DsDatum(DsDatum)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)
  , DsKeyMint(DsKeyMint)
  , Node(Node)
  , rootNode
  , dsConfTokenName

  , insertValidator
  , dsConfValidator
  , dsConfPolicy
  , dsKeyPolicy
  ) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (PlutusData(..))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash)
import Contract.Scripts as Scripts
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction (Language(PlutusV2), TransactionInput)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.Array as Array
import Data.Maybe as Maybe
import FromData (class FromData, fromData)
import Partial.Unsafe as Unsafe
import RawScripts as RawScripts
import ToData (class ToData, toData)

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
dsKeyPolicy dskm = mkMintingPolicyParams RawScripts.rawDsConfPolicy $ map toData
  [ dskm ]

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
