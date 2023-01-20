{- | "Ctl" defines a monad which allows one to conveniently call CLI ctl
 commands / generate required data.
-}
module Ctl where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
 )
import Crypto.Secp256k1 qualified as Secp256k1
import Plutus.V2.Ledger.Api (
  Address,
  TxId,
 )
import TrustlessSidechain.MerkleTree (MerkleProof, RootHash)
import TrustlessSidechain.Types (SidechainPubKey)

-- * Various product types to represent the parameters needed for the

-- corresponding ctl command

--  | 'Registration' provides a wrapper type for the parameters required
--  to register an spo
data Registration = Registration
  { rSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
  , rSidechainPrvKey :: Secp256k1.SecKey
  , rRegistrationUtxo :: TxId
  }

--  | 'Deregistration' provides a wrapper type for the parameters required
--  to deregister an spo
data Deregistration = Deregistration
  { drSpoPubKey :: VerKeyDSIGN Ed25519DSIGN
  }

--  | 'UpdateCommitteeHash' provides a wrapper type for the parameters required
--  to update the committee hash
data UpdateCommitteeHash = UpdateCommitteeHash
  { uchCurrentCommitteePrvKeys :: [Secp256k1.SecKey]
  , uchNewCommitteePubKeys :: [SidechainPubKey]
  , uchSidechainEpoch :: Integer
  , uchPreviousMerkleRoot :: Maybe RootHash
  }

--  | 'SaveRoot' provides a wrapper type for the parameters required
--  to save a merkle root
data SaveRoot = SaveRoot
  { srMerkleRoot :: RootHash
  , srCurrentCommitteePrivKeys :: [Secp256k1.SecKey]
  , srPreviousMerkleRoot :: Maybe RootHash
  }

--  | 'InitSidechain' provides a wrapper type for the parameters required
--  to inialise a sidechain
data InitSidechain = InitSidechain
  { isInitCommitteePubKeys :: [SidechainPubKey]
  , isSidechainEpoch :: Integer
  }

--  | 'Mint' provides a wrapper type for the parameters required
--  to claim tokens
data Mint = Mint
  { mAmount :: Integer
  , mRecipient :: Address
  , mMerkleProof :: MerkleProof
  , mIndex :: Integer
  , mPreviousMerkleRoot :: Maybe RootHash
  }

--  | 'Burn' provides a wrapper type for the parameters required
--  to burn tokens
data Burn = Burn
  { bAmount :: Integer
  , bRecipient :: Address
  }

class MonadCtl m where
  freshCommittee :: m [Secp256k1.SecKey]
  updateCommitteeHash :: UpdateCommitteeHash -> m TxId
  saveRoot :: UpdateCommitteeHash -> m TxId
