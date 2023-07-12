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
  , toATMSAggregateSignatures

  , module ExportCommitteeATMSSchemesTypes
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (ByteArray)
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
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

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

-- | `toATMSAggregateSignatures` takes
-- |
-- |    - the ATMS scheme;
-- |
-- |    - the signers' public keys and their and their associated signatures
-- |    (if such a signature exists i.e., if they signed it);
-- |
-- | and either errors (if the provided public keys / signatures are invalid)
-- | or successfully returns the `ATMSAggregateSignatures` that is suitable for
-- | `atmsSchemeLookupsAndConstraints`
toATMSAggregateSignatures ∷
  { -- the atms kind
    atmsKind ∷ ATMSKinds
  , -- the committee's public keys and signature (if the signature is given)
    committeePubKeyAndSigs ∷ Array (ByteArray /\ Maybe ByteArray)
  } →
  Either String ATMSAggregateSignatures
toATMSAggregateSignatures { atmsKind, committeePubKeyAndSigs } =
  case atmsKind of
    ATMSPlain → map Plain $ flip traverse committeePubKeyAndSigs $
      \(pk /\ mSig) → do
        pk' ← case Utils.Crypto.sidechainPublicKey pk of
          Nothing → Left $ "invalid ECDSA SECP256k1 public key: " <> show pk
          Just pk' → Right pk'

        sig' ← case mSig of
          Nothing → Right Nothing
          Just sig → case Utils.Crypto.sidechainSignature sig of
            Nothing → Left $ "invalid ECDSA SECP256k1 signature: " <> show sig
            Just sig' → Right $ Just sig'

        pure $ pk' /\ sig'

    ATMSDummy → Left "ATMS dummy not implemented yet"
    ATMSPoK → Left "ATMS PoK not implemented yet"
    ATMSMultisignature → Left "ATMS multisignature not implemented yet"
