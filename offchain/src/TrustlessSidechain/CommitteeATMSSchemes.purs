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

import Contract.PlutusData (PlutusData)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Run (Run)
import Run.Except (EXCEPT, throw)
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
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
      ( VerificationError
      , NotImplemented
      )
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1 as SchnorrSecp256k1
import Type.Row (type (+))
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Effects.Log (LOG)

-- | `atmsSchemeLookupsAndConstraints` returns the lookups and constraints
-- | corresponding to the given `ATMSSchemeParams`
atmsSchemeLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  CommitteeATMSParams ATMSAggregateSignatures →
  Run (EXCEPT OffchainError + LOG + WALLET + TRANSACTION + r)
    { constraints ∷ TxConstraints
    , lookups ∷ ScriptLookups
    }
atmsSchemeLookupsAndConstraints sidechainParams atmsParams = do
  case (unwrap atmsParams).aggregateSignature of
    PlainEcdsaSecp256k1 param → do
      CommitteePlainEcdsaSecp256k1ATMSPolicy.mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy
        sidechainParams
        $ CommitteeATMSParams
            ((unwrap atmsParams) { aggregateSignature = param })
    PlainSchnorrSecp256k1 param → do
      CommitteePlainSchnorrSecp256k1ATMSPolicy.mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy
        { sidechainParams
        , committeeATMSParams:
            CommitteeATMSParams
              ((unwrap atmsParams) { aggregateSignature = param })
        }
    -- TODO: fill these in later :^)
    Dummy → throw $ NotImplemented "ATMS dummy not implemented yet"
    PoK → throw $ NotImplemented "ATMS PoK not implemented yet"
    Multisignature → throw $ NotImplemented
      "ATMS multisignature not implemented yet"

-- | `atmsCommitteeCertificateVerificationMintingPolicy` grabs
-- | the currency symbol / minting policy associated with the aggregate signature.
atmsCommitteeCertificateVerificationMintingPolicy ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , committeeCertificateMint ∷ CommitteeCertificateMint
  } →
  ATMSAggregateSignatures →
  Run (EXCEPT OffchainError + WALLET + r) CurrencyInfo
atmsCommitteeCertificateVerificationMintingPolicy params sig =
  atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind params $
    case sig of
      PlainEcdsaSecp256k1 _ → ATMSPlainEcdsaSecp256k1
      PlainSchnorrSecp256k1 _ → ATMSPlainSchnorrSecp256k1
      Dummy → ATMSDummy
      PoK → ATMSPoK
      Multisignature → ATMSMultisignature

-- | `atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind` is
-- | essentially `atmsCommitteeCertificateVerificationMintingPolicy` but with
-- | `ATMSKinds`.
atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , committeeCertificateMint ∷ CommitteeCertificateMint
  } →
  ATMSKinds →
  Run (EXCEPT OffchainError + WALLET + r) CurrencyInfo
atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind params = case _ of
  ATMSPlainEcdsaSecp256k1 →
    CommitteePlainEcdsaSecp256k1ATMSPolicy.committeePlainEcdsaSecp256k1ATMSCurrencyInfo
      params
  ATMSPlainSchnorrSecp256k1 →
    CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSCurrencyInfo
      params
  ATMSDummy → throw $ NotImplemented "ATMS dummy not implemented yet"
  ATMSPoK → throw $ NotImplemented "ATMS PoK not implemented yet"
  ATMSMultisignature → throw $ NotImplemented
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
  ∀ r.
  { -- the atms kind
    atmsKind ∷ ATMSKinds
  , -- the committee's public keys and signature (if the signature is given)
    committeePubKeyAndSigs ∷ Array (ByteArray /\ Maybe ByteArray)
  } →
  Run (EXCEPT OffchainError + r) ATMSAggregateSignatures
toATMSAggregateSignatures { atmsKind, committeePubKeyAndSigs } =
  case atmsKind of
    ATMSPlainEcdsaSecp256k1 → map PlainEcdsaSecp256k1
      $ flip traverse committeePubKeyAndSigs
      $
        \(pk /\ mSig) → do
          pk' ← case Utils.Crypto.ecdsaSecp256k1PubKey pk of
            Nothing → throw
              ( VerificationError $ "invalid ECDSA SECP256k1 public key: "
                  <> show pk
              )
            Just pk' → pure pk'

          sig' ← case mSig of
            Nothing → pure Nothing
            Just sig → case Utils.Crypto.ecdsaSecp256k1Signature sig of
              Nothing → throw
                ( VerificationError
                    $ "invalid ECDSA SECP256k1 signature: "
                    <> show sig
                )
              Just sig' → pure $ Just sig'

          pure $ pk' /\ sig'

    ATMSPlainSchnorrSecp256k1 → map PlainSchnorrSecp256k1
      $ flip traverse committeePubKeyAndSigs
      $
        \(pk /\ mSig) → do
          pk' ← case SchnorrSecp256k1.parsePublicKey pk of
            Nothing → throw
              ( VerificationError
                  $ "invalid Schnorr SECP256k1 public key: "
                  <> show pk
              )
            Just pk' → pure pk'

          sig' ← case mSig of
            Nothing → pure Nothing
            Just sig → case SchnorrSecp256k1.parseSignature sig of
              Nothing → throw
                ( VerificationError
                    $ "invalid Schnorr SECP256k1 signature: "
                    <> show sig
                )
              Just sig' → pure $ Just sig'

          pure $ pk' /\ sig'

    ATMSDummy → throw (NotImplemented "ATMS dummy")
    ATMSPoK → throw (NotImplemented "ATMS PoK")
    ATMSMultisignature → throw (NotImplemented "ATMS multisignature")

-- | `aggregateATMSPublicKeys` aggregates the public keys of an ATMS key for
-- | the given `ATMSKind`
aggregateATMSPublicKeys ∷
  ∀ r.
  { -- the atms kind
    atmsKind ∷ ATMSKinds
  , -- the committee's public keys and signature (if the signature is given)
    committeePubKeys ∷ Array ByteArray
  } →
  Run (EXCEPT OffchainError + r) PlutusData
aggregateATMSPublicKeys { atmsKind, committeePubKeys } =
  case atmsKind of
    ATMSPlainEcdsaSecp256k1 →
      map (PlutusData.toData <<< (unsafePartial Utils.Crypto.aggregateKeys) <<< map unwrap)
        $ flip traverse committeePubKeys
        $
          \pk → do
            pk' ← case Utils.Crypto.ecdsaSecp256k1PubKey pk of
              Nothing → throw
                ( VerificationError
                    $ "invalid ECDSA SECP256k1 public key: "
                    <> show pk
                )
              Just pk' → pure pk'
            pure $ pk'
    ATMSPlainSchnorrSecp256k1 →
      map (PlutusData.toData <<< (unsafePartial Utils.Crypto.aggregateKeys) <<< map unwrap)
        $ flip traverse committeePubKeys
        $
          \pk → do
            pk' ← case SchnorrSecp256k1.parsePublicKey pk of
              Nothing → throw
                ( VerificationError
                    $ "invalid Schnorr SECP256k1 public key: "
                    <> show pk
                )
              Just pk' → pure pk'
            pure $ pk'
    ATMSDummy → throw (NotImplemented "ATMS dummy")
    ATMSPoK → throw (NotImplemented "ATMS PoK")
    ATMSMultisignature → throw (NotImplemented "ATMS multisignature")
