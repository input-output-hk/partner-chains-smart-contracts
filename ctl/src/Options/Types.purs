module Options.Types (Options(..), Endpoint(..), ScParams(..)) where

import Contract.Prelude

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Transaction (TransactionInput)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)
import Types.ByteArray (ByteArray)

type Options a =
  { scParams ∷ a
  , skey ∷ FilePath
  , endpoint ∷ Endpoint
  }

data ScParams = Value SidechainParams | ConfigFile FilePath

derive instance Generic ScParams _
instance Show ScParams where
  show = genericShow

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
