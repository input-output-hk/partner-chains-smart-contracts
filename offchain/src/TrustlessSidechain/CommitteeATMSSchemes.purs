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

  , module ExportCommitteePlainATMSPolicy
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
  ( ATMSAggregateSignatures(Plain, Multisignature, PoK, Dummy)
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures(Plain, Multisignature, PoK, Dummy)
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  ) as ExportCommitteeATMSSchemesTypes
import TrustlessSidechain.CommitteePlainATMSPolicy (CommitteePlainATMSParams)
import TrustlessSidechain.CommitteePlainATMSPolicy
  ( CommitteePlainATMSParams(CommitteePlainATMSParams)
  )
import TrustlessSidechain.CommitteePlainATMSPolicy
  ( CommitteePlainATMSParams(CommitteePlainATMSParams)
  ) as ExportCommitteePlainATMSPolicy
import TrustlessSidechain.CommitteePlainATMSPolicy as CommitteePlainATMSPolicy
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

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
        $ CommitteePlainATMSParams
        $ CommitteeATMSParams
            ((unwrap atmsParams) { aggregateSignature = param })
    -- TODO: fill these in later :^)
    Dummy → Monad.throwContractError "ATMS dummy not implemented yet"
    PoK → Monad.throwContractError "ATMS PoK not implemented yet"
    Multisignature → Monad.throwContractError
      "ATMS multisignature not implemented yet"

-- | `atmsCommitteeCertificateVerificationMintingPolicyCurrencySymbol` grabs
-- | the currency symbol / minting policy associated with the aggregate signature.
atmsCommitteeCertificateVerificationMintingPolicy ∷
  CommitteeCertificateMint →
  ATMSAggregateSignatures →
  Contract
    { committeeCertificateVerificationMintingPolicy ∷ MintingPolicy
    , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
    }
atmsCommitteeCertificateVerificationMintingPolicy ccm = case _ of
  Plain _ → do
    { committeePlainATMSPolicy
    , committeePlainATMSCurrencySymbol
    } ← CommitteePlainATMSPolicy.getCommitteePlainATMSPolicy ccm
    pure
      { committeeCertificateVerificationMintingPolicy: committeePlainATMSPolicy
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainATMSCurrencySymbol
      }
  Dummy → Monad.throwContractError "ATMS dummy not implemented yet"
  PoK → Monad.throwContractError "ATMS PoK not implemented yet"
  Multisignature → Monad.throwContractError
    "ATMS multisignature not implemented yet"
