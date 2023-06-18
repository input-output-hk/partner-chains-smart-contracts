module TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeCertificateMint(CommitteeCertificateMint)
  , CommitteeATMSParams(CommitteeATMSParams)
  , ATMSAggregateSignatures(Plain, Multisignature, PoK, Dummy)
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , toData
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Value (CurrencySymbol)
import Contract.Value (TokenName)
import Data.BigInt (BigInt)
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | `CommitteeCertificateMint` corresponds to the onchain type that is used to
-- | parameterize a committee certificate verification minting policy.
newtype CommitteeCertificateMint = CommitteeCertificateMint
  { committeeOraclePolicy ∷ CurrencySymbol
  , thresholdNumerator ∷ BigInt
  , thresholdDenominator ∷ BigInt
  }

instance ToData CommitteeCertificateMint where
  toData
    ( CommitteeCertificateMint
        { committeeOraclePolicy, thresholdNumerator, thresholdDenominator }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData committeeOraclePolicy
      , toData thresholdNumerator
      , toData thresholdDenominator
      ]

derive instance Generic CommitteeCertificateMint _

derive instance Newtype CommitteeCertificateMint _

-- | `CommitteeATMSParams` is a type to bundle up all the required data
-- | when building the transaction for a committee ATMS scheme (this is used
-- | only as a offchain parameter)
newtype CommitteeATMSParams aggregateSignature = CommitteeATMSParams
  {
    -- UTxO for the current committee as stored onchain (which should be
    -- uniquely identified by the token
    -- `CommitteeCertificateMint.committeeOraclePolicy`).
    currentCommitteeUtxo ∷
      { index ∷ TransactionInput
      , value ∷ TransactionOutputWithRefScript
      }
  , -- parameter for the onchain code
    committeeCertificateMint ∷ CommitteeCertificateMint
  , -- aggregateSignature for the below message
    aggregateSignature ∷ aggregateSignature
  ,
    -- the message that should be signed (note: this *must* be a token name
    -- so we have the usual size restrictions of a token name i.e., you
    -- probably want this to be the hash of the message you wish to sign)
    message ∷ TokenName
  }

derive instance Newtype (CommitteeATMSParams aggregateSignature) _

-- | `ATMSAggregateSignatures` is a sumtype which defines all the possible ATMS
-- | aggregate keys possible
data ATMSAggregateSignatures
  = Plain (Array (SidechainPublicKey /\ Maybe SidechainSignature))
  | Multisignature
  | PoK
  | Dummy
