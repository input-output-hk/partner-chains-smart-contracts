-- | `TrustlessSidechain.CommitteeATMSSchemes` is intended to be the
-- | interface to the ATMS schemes.
-- | Namely, this provides an interface for
-- |
-- |    - `TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy`
-- |
-- |    - (and more to come!)
module TrustlessSidechain.CommitteeATMSSchemes
  ( atmsSchemeLookupsAndConstraints
  , atmsCommitteeCertificateVerificationMintingPolicy
  , atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
  , toATMSAggregateSignatures
  , aggregateATMSPublicKeys

  , module ExportCommitteeATMSSchemesTypes
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (PlutusData)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (MintingPolicy)
import Contract.TxConstraints (TxConstraints)
import Contract.Value
  ( CurrencySymbol
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures
      ( Multisignature
      , PoK
      , Dummy
      , PlainEcdsaSecp256k1
      , PlainSchnorrSecp256k1
      )
  , ATMSKinds
      ( ATMSPlainEcdsaSecp256k1
      , ATMSPlainSchnorrSecp256k1
      , ATMSMultisignature
      , ATMSPoK
      , ATMSDummy
      )
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint
  )
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures
      ( PlainEcdsaSecp256k1
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
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  ) as ExportCommitteeATMSSchemesTypes
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1 as SchnorrSecp256k1

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
    PlainEcdsaSecp256k1 param → do
      CommitteePlainEcdsaSecp256k1ATMSPolicy.mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy
        $ CommitteeATMSParams
            ((unwrap atmsParams) { aggregateSignature = param })
    PlainSchnorrSecp256k1 param → do
      CommitteePlainSchnorrSecp256k1ATMSPolicy.mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy
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
    PlainEcdsaSecp256k1 _ → ATMSPlainEcdsaSecp256k1
    PlainSchnorrSecp256k1 _ → ATMSPlainSchnorrSecp256k1
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
  ATMSPlainEcdsaSecp256k1 → do
    { committeePlainEcdsaSecp256k1ATMSPolicy
    , committeePlainEcdsaSecp256k1ATMSCurrencySymbol
    } ←
      CommitteePlainEcdsaSecp256k1ATMSPolicy.getCommitteePlainEcdsaSecp256k1ATMSPolicy
        ccm
    pure
      { committeeCertificateVerificationMintingPolicy:
          committeePlainEcdsaSecp256k1ATMSPolicy
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainEcdsaSecp256k1ATMSCurrencySymbol
      }
  ATMSPlainSchnorrSecp256k1 → do
    { committeePlainSchnorrSecp256k1ATMSPolicy
    , committeePlainSchnorrSecp256k1ATMSCurrencySymbol
    } ←
      CommitteePlainSchnorrSecp256k1ATMSPolicy.getCommitteePlainSchnorrSecp256k1ATMSPolicy
        ccm
    pure
      { committeeCertificateVerificationMintingPolicy:
          committeePlainSchnorrSecp256k1ATMSPolicy
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainSchnorrSecp256k1ATMSCurrencySymbol
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
    ATMSPlainEcdsaSecp256k1 → map PlainEcdsaSecp256k1
      $ flip traverse committeePubKeyAndSigs
      $
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

    ATMSPlainSchnorrSecp256k1 → map PlainSchnorrSecp256k1
      $ flip traverse committeePubKeyAndSigs
      $
        \(pk /\ mSig) → do
          pk' ← case SchnorrSecp256k1.parsePublicKey pk of
            Nothing → Left $ "invalid Schnorr SECP256k1 public key: " <> show pk
            Just pk' → Right pk'

          sig' ← case mSig of
            Nothing → Right Nothing
            Just sig → case SchnorrSecp256k1.parseSignature sig of
              Nothing → Left $ "invalid Schnorr SECP256k1 signature: " <> show sig
              Just sig' → Right $ Just sig'

          pure $ pk' /\ sig'

    ATMSDummy → Left "ATMS dummy not implemented yet"
    ATMSPoK → Left "ATMS PoK not implemented yet"
    ATMSMultisignature → Left "ATMS multisignature not implemented yet"

-- | `aggregateATMSPublicKeys` aggregates the public keys of an ATMS key for
-- | the given `ATMSKind`
aggregateATMSPublicKeys ∷
  { -- the atms kind
    atmsKind ∷ ATMSKinds
  , -- the committee's public keys and signature (if the signature is given)
    committeePubKeys ∷ Array ByteArray
  } →
  Either String PlutusData
aggregateATMSPublicKeys { atmsKind, committeePubKeys } =
  case atmsKind of
    ATMSPlainEcdsaSecp256k1 →
      map (PlutusData.toData <<< Utils.Crypto.aggregateKeys <<< map unwrap)
        $ flip traverse committeePubKeys
        $
          \pk → do
            pk' ← case Utils.Crypto.sidechainPublicKey pk of
              Nothing → Left $ "invalid ECDSA SECP256k1 public key: " <> show pk
              Just pk' → Right pk'
            pure $ pk'
    ATMSPlainSchnorrSecp256k1 →
      map (PlutusData.toData <<< Utils.Crypto.aggregateKeys <<< map unwrap)
        $ flip traverse committeePubKeys
        $
          \pk → do
            pk' ← case SchnorrSecp256k1.parsePublicKey pk of
              Nothing → Left $ "invalid Schnorr SECP256k1 public key: " <> show pk
              Just pk' → Right pk'
            pure $ pk'
    ATMSDummy → Left "ATMS dummy not implemented yet"
    ATMSPoK → Left "ATMS PoK not implemented yet"
    ATMSMultisignature → Left "ATMS multisignature not implemented yet"
