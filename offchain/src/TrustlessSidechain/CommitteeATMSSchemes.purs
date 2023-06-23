-- | `TrustlessSidechain.CommitteeATMSSchemes` is intended to be the
-- | interface to the ATMS schemes.
-- | Namely, this provides an interface for
-- |
-- |    - `TrustlessSidechain.CommitteePlainATMSPolicy`
-- |
-- |    - (and more to come!)
module TrustlessSidechain.CommitteeATMSSchemes
  ( atmsSchemeLookupsAndConstraints
  , atmsCommitteeCertificateVerificationMintingPolicy
  , atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind

  , module ExportCommitteeATMSSchemesTypes
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (MintingPolicy)
import Contract.TxConstraints (TxConstraints)
import Contract.Value
  ( CurrencySymbol
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures(Multisignature, PoK, Dummy, Plain)
  , ATMSKinds(ATMSPlain, ATMSMultisignature, ATMSPoK, ATMSDummy)
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures(Plain, Multisignature, PoK, Dummy)
  , ATMSKinds(ATMSPlain, ATMSMultisignature, ATMSPoK, ATMSDummy)
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  ) as ExportCommitteeATMSSchemesTypes
import TrustlessSidechain.CommitteePlainATMSPolicy as CommitteePlainATMSPolicy

-- | `atmsSchemeLookupsAndConstraints` returns the lookups and constraints
-- | corresponding to the given `ATMSSchemeParams`
atmsSchemeLookupsAndConstraints ∷
  CommitteeATMSParams ATMSAggregateSignatures →
  Contract
    { constraints ∷ TxConstraints Void Void
    , lookups ∷ ScriptLookups Void
    }
atmsSchemeLookupsAndConstraints atmsParams =
  case (unwrap atmsParams).aggregateSignature of
    Plain param → do
      CommitteePlainATMSPolicy.mustMintCommitteePlainATMSPolicy
        $ CommitteeATMSParams
            ((unwrap atmsParams) { aggregateSignature = param })
    -- TODO: fill these in later :^)
    Dummy → Monad.throwContractError "ATMS dummy not implemented yet"
    PoK → Monad.throwContractError "ATMS PoK not implemented yet"
    Multisignature → Monad.throwContractError
      "ATMS multisignature not implemented yet"

-- | `atmsCommitteeCertificateVerificationMintingPolicy` grabs
-- | the currency symbol / minting policy associated with the aggregate signature.
atmsCommitteeCertificateVerificationMintingPolicy ∷
  CommitteeCertificateMint →
  ATMSAggregateSignatures →
  Contract
    { committeeCertificateVerificationMintingPolicy ∷ MintingPolicy
    , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
    }
atmsCommitteeCertificateVerificationMintingPolicy ccm sig =
  atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind ccm $ case sig of
    Plain _ → ATMSPlain
    Dummy → ATMSDummy
    PoK → ATMSPoK
    Multisignature → ATMSMultisignature

-- | `atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind` is
-- | essentially `atmsCommitteeCertificateVerificationMintingPolicy` but with
-- | `ATMSKinds`.
atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind ∷
  CommitteeCertificateMint →
  ATMSKinds →
  Contract
    { committeeCertificateVerificationMintingPolicy ∷ MintingPolicy
    , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
    }
atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind ccm = case _ of
  ATMSPlain → do
    { committeePlainATMSPolicy
    , committeePlainATMSCurrencySymbol
    } ← CommitteePlainATMSPolicy.getCommitteePlainATMSPolicy ccm
    pure
      { committeeCertificateVerificationMintingPolicy: committeePlainATMSPolicy
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainATMSCurrencySymbol
      }
  ATMSDummy → Monad.throwContractError "ATMS dummy not implemented yet"
  ATMSPoK → Monad.throwContractError "ATMS PoK not implemented yet"
  ATMSMultisignature → Monad.throwContractError
    "ATMS multisignature not implemented yet"
