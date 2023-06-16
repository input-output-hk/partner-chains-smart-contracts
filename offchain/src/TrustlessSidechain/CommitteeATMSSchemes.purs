-- | `TrustlessSidechain.CommitteeATMSSchemes` is intended to be the
-- | interface to the ATMS schemes.
-- | Namely, this provides an interface for
-- |
-- |    - `TrustlessSidechain.CommitteePlainATMSPolicy`
-- |
-- |    - (and more to come!)
module TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSSchemeParams
      ( Plain
      , Multisignature
      , Dummy
      , PoK
      )
  , atmsSchemeLookupsAndConstraints

  , module ExportCommitteePlainATMSPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import TrustlessSidechain.CommitteePlainATMSPolicy
  ( CommitteeCertificateMint(CommitteeCertificateMint)
  , CommitteePlainATMSParams(CommitteePlainATMSParams)
  ) as ExportCommitteePlainATMSPolicy
import TrustlessSidechain.CommitteePlainATMSPolicy (CommitteePlainATMSParams)
import TrustlessSidechain.CommitteePlainATMSPolicy as CommitteePlainATMSPolicy
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | `ATMSSchemeParams` is an offchain type which contains all the information
-- | to build transactions for an ATMS scheme.
data ATMSSchemeParams
  = Plain CommitteePlainATMSParams
  | Multisignature -- TODO not implemented yet.
  | Dummy -- TODO not implemented yet
  | PoK -- TODO not implemented yet

--  | `atmsSchemeLookupsAndConstraints` returns the lookups and constraints
-- | corresponding to the given `ATMSSchemeParams`
atmsSchemeLookupsAndConstraints ∷
  ATMSSchemeParams →
  Contract
    { constraints ∷ TxConstraints Void Void
    , lookups ∷ ScriptLookups Void
    }
atmsSchemeLookupsAndConstraints atmsScheme = case atmsScheme of
  Plain param → do
    CommitteePlainATMSPolicy.mustMintCommitteePlainATMSPolicy param

  -- TODO: fill these in later :^)
  Dummy → Monad.throwContractError "ATMS dummy not implemented yet"
  PoK → Monad.throwContractError "ATMS PoK not implemented yet"
  Multisignature → Monad.throwContractError
    "ATMS multisignature not implemented yet"
