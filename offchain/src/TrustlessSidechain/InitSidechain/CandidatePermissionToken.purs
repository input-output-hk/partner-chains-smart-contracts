module TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  ) where

import Contract.Prelude hiding (note)

import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken
  ( candidatePermissionInitTokenName
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logInfo')
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Init (init)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import Type.Row (type (+))

initCandidatePermissionToken ::
  forall r.
  SidechainParams ->
  Maybe BigInt ->
  Run (APP + r) (Maybe TransactionHash)
initCandidatePermissionToken
  sidechainParams
  initCandidatePermissionTokenMintInfo = do

  -- Note: normally we would need to insert versioned scripts first - c.f. other
  -- initialization routines in the InitSidechain directory - but the candidate
  -- permission token mechanism does not version any validators or minting
  -- policies.  We can thus proceed directly to token minting, without the need
  -- for any extra checks.
  logInfo' "Attempting to mint candidate permission tokens from the init token"
  init
    ( \op -> balanceSignAndSubmit op
        <=< initCandidatePermissionTokenLookupsAndConstraints
          initCandidatePermissionTokenMintInfo
    )
    "Candidate permission token init"
    candidatePermissionInitTokenName
    sidechainParams

-- | `initCandidatePermissionTokenLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the candidiate permission tokens (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the candidiate permission tokens if
-- |     `initCandidatePermissionTokenMintInfo` is `Just` (otherwise returns empty)
initCandidatePermissionTokenLookupsAndConstraints ::
  forall r.
  Maybe BigInt ->
  SidechainParams ->
  Run (EXCEPT OffchainError + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
initCandidatePermissionTokenLookupsAndConstraints
  initCandidatePermissionTokenMintInfo
  sidechainParams =
  case initCandidatePermissionTokenMintInfo of
    Nothing -> pure mempty
    Just amount -> do
      CandidatePermissionToken.candidatePermissionTokenLookupsAndConstraints
        sidechainParams
        amount
