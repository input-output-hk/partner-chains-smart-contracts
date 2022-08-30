module Options.Types (Options(..), Endpoint(..), ScParams(..)) where

import Contract.Prelude

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Transaction (TransactionInput)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)

type Options a =
  { scParams ∷ a
  , skey ∷ String
  , endpoint ∷ Endpoint
  }

data ScParams = Value SidechainParams | ConfigFile FilePath

derive instance Generic ScParams _
instance Show ScParams where
  show = genericShow

data Endpoint
  = MintAct { amount ∷ Int }
  | BurnAct { amount ∷ Int, recipient ∷ String }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ PubKey
      , spoSig ∷ Signature
      , sidechainSig ∷ Signature
      , inputUtxo ∷ TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey ∷ PubKey }

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow
