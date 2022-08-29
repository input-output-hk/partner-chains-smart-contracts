module Options.Types (Options(..), Endpoint(..)) where

import Aeson
import Contract.Aeson
import Contract.Prelude

import Aeson.Decode as Decode
import Aeson.Encode ((>$<))
import Aeson.Encode as Encode
import CommitteCandidateValidator (PubKey, Signature)
import Contract.Transaction (TransactionInput)
import Control.Lazy (defer)
import SidechainParams (SidechainParams)

type Options =
  { scParams :: SidechainParams
  , skey :: String
  , endpoint :: Endpoint
  }

data Endpoint
  = MintAct { amount :: Int }
  | BurnAct { amount :: Int, recipient :: String }
  | CommitteeCandidateReg
      { spoPubKey :: PubKey
      , sidechainPubKey :: PubKey
      , spoSig :: Signature
      , sidechainSig :: Signature
      , inputUtxo :: TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey :: PubKey }

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow
