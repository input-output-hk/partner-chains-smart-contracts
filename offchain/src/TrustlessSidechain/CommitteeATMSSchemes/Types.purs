module TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeCertificateMint(CommitteeCertificateMint)
  , CommitteeATMSParams(CommitteeATMSParams)
  , ATMSAggregateSignatures
      ( PlainEcdsaSecp256k1
      , PlainSchnorrSecp256k1
      , Multisignature
      , PoK
      , Dummy
      )
  , ATMSKinds
      ( ATMSPlainEcdsaSecp256k1
      , ATMSPlainSchnorrSecp256k1
      , ATMSMultisignature
      , ATMSPoK
      , ATMSDummy
      )
  ) where

import Contract.Prelude

import Contract.PlutusData (class FromData, class ToData)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.AssetName (AssetName)
import JS.BigInt (BigInt)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1
  ( SchnorrSecp256k1PublicKey
  , SchnorrSecp256k1Signature
  )

-- | `CommitteeCertificateMint` corresponds to the onchain type that is used to
-- | parameterize a committee certificate verification minting policy.
newtype CommitteeCertificateMint = CommitteeCertificateMint
  { thresholdNumerator ∷ BigInt
  , thresholdDenominator ∷ BigInt
  }

instance ToData CommitteeCertificateMint where
  toData
    ( CommitteeCertificateMint
        { thresholdNumerator, thresholdDenominator }
    ) =
    productToData2 thresholdNumerator thresholdDenominator

instance FromData CommitteeCertificateMint where
  fromData = productFromData2 $
    \thresholdNumerator
     thresholdDenominator → CommitteeCertificateMint
      { thresholdNumerator
      , thresholdDenominator
      }

derive instance Generic CommitteeCertificateMint _

derive instance Newtype CommitteeCertificateMint _

derive newtype instance Eq CommitteeCertificateMint

instance Show CommitteeCertificateMint where
  show = genericShow

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
      , value ∷ TransactionOutput
      }
  , -- parameter for the onchain code
    committeeCertificateMint ∷ CommitteeCertificateMint
  , -- aggregateSignature for the below message
    aggregateSignature ∷ aggregateSignature
  ,
    -- the message that should be signed (note: this *must* be a token name
    -- so we have the usual size restrictions of a token name i.e., you
    -- probably want this to be the hash of the message you wish to sign)
    message ∷ AssetName
  }

derive instance Newtype (CommitteeATMSParams aggregateSignature) _

-- | `ATMSAggregateSignatures` is a sumtype which defines all the possible ATMS
-- | aggregate keys + signatures
data ATMSAggregateSignatures
  = PlainEcdsaSecp256k1
      (Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature))
  | PlainSchnorrSecp256k1
      (Array (SchnorrSecp256k1PublicKey /\ Maybe SchnorrSecp256k1Signature))
  | Multisignature
  | PoK
  | Dummy

-- | `ATMSKinds` denotes all the kinds of ATMS schemes that
-- | will be hopefully supported in the system.
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
  = ATMSPlainEcdsaSecp256k1
  | ATMSPlainSchnorrSecp256k1
  | ATMSMultisignature
  | ATMSPoK
  | ATMSDummy

derive instance Generic ATMSKinds _

derive instance Eq ATMSKinds

derive instance Ord ATMSKinds

instance Show ATMSKinds where
  show = case _ of
    ATMSPlainEcdsaSecp256k1 → "plain-ecdsa-secp256k1"
    ATMSPlainSchnorrSecp256k1 → "plain-schnorr-secp256k1"
    ATMSMultisignature → "multisignature"
    ATMSPoK → "pok"
    ATMSDummy → "dummy"
