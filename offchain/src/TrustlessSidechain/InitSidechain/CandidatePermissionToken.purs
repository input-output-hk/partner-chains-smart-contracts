module TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  ) where

import Contract.Prelude hiding (note)

import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Data.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken
  ( candidatePermissionInitTokenName
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Init (init)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import Type.Row (type (+))

initCandidatePermissionToken ∷
  ∀ r.
  SidechainParams →
  Maybe BigInt →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        }
    )
initCandidatePermissionToken
  sidechainParams
  initCandidatePermissionTokenMintInfo = do
  let
    run = init
      ( \op → balanceSignAndSubmit op
          <=< initCandidatePermissionTokenLookupsAndConstraints
            initCandidatePermissionTokenMintInfo
      )
      "Candidate permission token init"
      candidatePermissionInitTokenName

  -- Note: normally we would need to insert versioned scripts in this place -
  -- c.f. other initialization routines in the InitSidechain directory - but the
  -- candidate permission token mechanism does not version any validators or
  -- minting policies.

  -- JSTOLAREK: a temporary solution until #772 is properly fixed
  -- if not $ null scriptsInitTxId then do
  candidatePermissionTokenInitTxId ← run sidechainParams
  pure
    ( Just
        { initTransactionIds: [ candidatePermissionTokenInitTxId ]
        }
    )
  --  else pure Nothing

-- | `initCandidatePermissionTokenLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the candidiate permission tokens (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the candidiate permission tokens if
-- |     `initCandidatePermissionTokenMintInfo` is `Just` (otherwise returns empty)
initCandidatePermissionTokenLookupsAndConstraints ∷
  ∀ r.
  Maybe BigInt →
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCandidatePermissionTokenLookupsAndConstraints
  initCandidatePermissionTokenMintInfo
  sidechainParams =
  case initCandidatePermissionTokenMintInfo of
    Nothing → pure mempty
    Just amount → do
      CandidatePermissionToken.candidatePermissionTokenLookupsAndConstraints
        sidechainParams
        amount
