module TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeCertificateMint(CommitteeCertificateMint)
  , CommitteeATMSParams(CommitteeATMSParams)
  , ATMSAggregateSignatures(Plain, Multisignature, PoK, Dummy)
  , ATMSKinds
      ( ATMSPlain
      , ATMSMultisignature
      , ATMSPoK
      , ATMSDummy
      )
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
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)
import Data.Show.Generic (genericShow)
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
-- | aggregate keys + signatures
data ATMSAggregateSignatures
  = Plain (Array (SidechainPublicKey /\ Maybe SidechainSignature))
  | Multisignature
  | PoK
  | Dummy

-- | `ATMSKinds` denotes all the kinds of ATMS schemes that
-- | will be hopefully supported  in the system.
-- | Note that these "correspond" in the obvious sense to
-- | `ATMSAggregateSignatures`.
-- |
-- | Notes:
-- |    - this is a type used only offchain, and is used for users to give
-- |    the ATMS mechanisms of the sidechain they are interacting with
-- |
-- |    - If we change this data type, then `TrustlessSidechain.Utils.Codecs`,
-- |    `TrustlessSidechain.Options.Parsers`, `TrustlessSidechain.Options.Specs`
-- |    all have user facing documentation that must be updated as well.
data ATMSKinds
  = ATMSPlain
  | ATMSMultisignature
  | ATMSPoK
  | ATMSDummy

derive instance Generic ATMSKinds _

derive instance Eq ATMSKinds

derive instance Ord ATMSKinds

instance Show ATMSKinds where
  show = genericShow
