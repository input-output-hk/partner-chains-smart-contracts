module Options.Types (Options(..), Config(..), Endpoint(..)) where

import Contract.Prelude

import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Types.ByteArray (ByteArray)

type Options =
  { endpoint ∷ Endpoint
  , scParams ∷ SidechainParams
  , pSkey ∷ FilePath
  , stSkey ∷ Maybe FilePath
  }

type Config =
  { sidechainParameters ∷
      Maybe
        { chainId ∷ Maybe Int
        , genesisHash ∷ Maybe ByteArray
        , genesisMint ∷ Maybe TransactionInput
        , genesisUtxo ∷ Maybe TransactionInput
        }
  , paymentSigningKeyFile ∷ Maybe FilePath
  , stakeSigningKeyFile ∷ Maybe FilePath
  }

data Endpoint
  = MintAct { amount ∷ BigInt }
  | BurnAct { amount ∷ BigInt, recipient ∷ ByteArray }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ PubKey
      , spoSig ∷ Signature
      , sidechainSig ∷ Signature
      , inputUtxo ∷ TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey ∷ PubKey }
  | GetAddrs

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow
