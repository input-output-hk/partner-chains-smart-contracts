module TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  , initCandidatePermissionTokenLookupsAndConstraints
  ) where

import Contract.Prelude hiding (note)

import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Data.Array ((:))
import Data.Maybe (isJust)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenMintInfo
  , candidatePermissionInitTokenName
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain.Init (init, insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getCandidatePermissionTokenPoliciesAndValidators
  )
import Type.Row (type (+))

initCandidatePermissionToken ∷
  ∀ r.
  SidechainParams →
  Maybe CandidatePermissionTokenMintInfo →
  ATMSKinds →
  Int →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        , sidechainParams ∷ SidechainParams
        , sidechainAddresses ∷ SidechainAddresses
        }
    )
initCandidatePermissionToken
  sidechainParams
  initCandidatePermissionTokenMintInfo
  initATMSKind
  version = do
  let
    run = init
      ( \op → balanceSignAndSubmit op
          <=< initCandidatePermissionTokenLookupsAndConstraints
            initCandidatePermissionTokenMintInfo
      )
      "Candidate permission token init"
      candidatePermissionInitTokenName

  scriptsInitTxId ← insertScriptsIdempotent
    getCandidatePermissionTokenPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  if not $ null scriptsInitTxId then do
    sidechainAddresses ←
      GetSidechainAddresses.getSidechainAddresses $
        SidechainAddressesEndpointParams
          { sidechainParams
          , atmsKind: initATMSKind
          , usePermissionToken: isJust
              initCandidatePermissionTokenMintInfo
          , version
          }
    candidatePermissionTokenInitTxId ← run sidechainParams
    pure
      ( Just
          { initTransactionIds: candidatePermissionTokenInitTxId : scriptsInitTxId
          , sidechainParams
          , sidechainAddresses
          }
      )
  else pure Nothing

-- | `initCandidatePermissionTokenLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the candidiate permission tokens (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the candidiate permission tokens if
-- |     `initCandidatePermissionTokenMintInfo` is `Just` (otherwise returns empty)
initCandidatePermissionTokenLookupsAndConstraints ∷
  ∀ r.
  Maybe CandidatePermissionTokenMintInfo →
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
    Just { candidatePermissionTokenAmount: amount } → do
      CandidatePermissionToken.candidatePermissionTokenLookupsAndConstraints
        sidechainParams
        amount
