module Options.Types (Options(..), Config(..), Endpoint(..)) where

import Contract.Prelude

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Transaction (TransactionInput)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)
import Types.ByteArray (ByteArray)

type Options =
  { scParams ∷ SidechainParams
  , skey ∷ FilePath
  , endpoint ∷ Endpoint
  }

type Config =
  { sidechainParameters ∷ Maybe SidechainParams
  , signingKeyFile ∷ Maybe FilePath
  }

data Endpoint
  = MintAct { amount ∷ Int }
  | BurnAct { amount ∷ Int, recipient ∷ ByteArray }
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
