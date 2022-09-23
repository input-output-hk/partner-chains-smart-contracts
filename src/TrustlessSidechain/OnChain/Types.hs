{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value (AssetClass, TokenName)
import Plutus.V2.Ledger.Contexts (TxOutRef)
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinByteString, Eq ((==)), Integer)
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.OffChain.Types (SidechainParams', SidechainPubKey)
import Prelude qualified

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MPTRootToken.
-}
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    mteIndex :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    mteAmount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    mteRecipient :: BuiltinByteString
  , -- | sidechain epoch for which merkle tree was created
    mteSidechainEpoch :: Integer
  , -- | 'mteHash' will be removed later TODO! Currently, we have this here to
    -- help test the system.
    mteHash :: BuiltinByteString
  }

makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide BuiltinByteString -- Recipient's sidechain address
  | -- | 'SideToMain' indicates that we wish to mint FUEL on the mainchain.
    -- So, this includes which transaction in the sidechain we are
    -- transferring over to the main chain (hence the 'MerkleTreeEntry'), and
    -- the proof tha this actually happened on the sidechain (hence the
    -- 'MerkleProof')
    SideToMain MerkleTreeEntry MerkleProof

data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    bprSpoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    bprSidechainPubKey :: SidechainPubKey
  , -- | Signature of the SPO
    bprSpoSignature :: Signature
  , -- | Signature of the SPO
    bprSidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    bprInputUtxo :: TxOutRef
  , -- | Owner public key hash
    bprOwnPkh :: PubKeyHash
  }
  deriving stock (Prelude.Show)

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: SidechainParams'
  , bprmSidechainPubKey :: SidechainPubKey
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: TxOutRef
  }
  deriving stock (Prelude.Show)

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = ()
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the mainchain.

PlutusTx.makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance ValidatorTypes FUELRedeemer where
  type RedeemerType FUELRedeemer = FUELRedeemer

{- | Datum for the committee hash. This /committee hash/ is used to verify
 signatures for sidechain to mainchain transfers. This is a hash of
 concatenated public key hashes of the committee members

 TODO: this isn't actually used to verify signatures in the FUEL minting /
 burning policies (perhaps this will be used in a later iteration)
-}
newtype UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash :: BuiltinByteString
  }

instance Eq UpdateCommitteeHashDatum where
  {-# INLINEABLE (==) #-}
  UpdateCommitteeHashDatum cmtHsh == UpdateCommitteeHashDatum cmtHsh' =
    cmtHsh == cmtHsh'

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashDatum [('UpdateCommitteeHashDatum, 0)]

{- | The Redeemer that is passed to the on-chain validator to update the
 committee
-}
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { -- | The current committee's signatures for the 'newCommitteeHash'
    committeeSignatures :: [BuiltinByteString]
  , -- | 'committeePubKeys' is the current committee public keys
    committeePubKeys :: [PubKey]
  , -- | 'newCommitteeHash' is the hash of the new committee
    newCommitteeHash :: BuiltinByteString
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashRedeemer [('UpdateCommitteeHashRedeemer, 0)]

{- | 'UpdatingCommitteeHash' is the type to associate the 'DatumType' and
 'RedeemerType' to the acutal types used at run time.
-}
data UpdatingCommitteeHash

instance ValidatorTypes UpdatingCommitteeHash where
  type DatumType UpdatingCommitteeHash = UpdateCommitteeHashDatum
  type RedeemerType UpdatingCommitteeHash = UpdateCommitteeHashRedeemer

-- | 'UpdateCommitteeHash' is used as the parameter for the contract.
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { -- | 'cToken' is the 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    cToken :: AssetClass
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''UpdateCommitteeHash
PlutusTx.makeIsDataIndexed ''UpdateCommitteeHash [('UpdateCommitteeHash, 0)]

-- | 'GenesisMintCommitteeHash' is used as the parameter for the minting policy
data GenesisMintCommitteeHash = GenesisMintCommitteeHash
  { -- | 'gcToken' is the token name of the NFT to start the committee hash
    gcToken :: TokenName
  , -- | 'TxOutRef' is the output reference to mint the NFT initially.
    gcTxOutRef :: TxOutRef
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''GenesisMintCommitteeHash

data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot :: BuiltinByteString
  , -- , lastMerkleRoot :: BuiltinByteString
    signatures :: [BuiltinByteString]
  , threshold :: Integer -- Natural: the number of committee pubkeys needed to sign off
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }

PlutusTx.makeIsDataIndexed ''SignedMerkleRoot [('SignedMerkleRoot, 0)]

instance ValidatorTypes SignedMerkleRoot where
  type RedeemerType SignedMerkleRoot = SignedMerkleRoot
