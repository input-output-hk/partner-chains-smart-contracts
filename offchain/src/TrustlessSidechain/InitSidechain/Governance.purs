module TrustlessSidechain.InitSidechain.Governance
  ( initGovernance
  ) where

import Contract.Prelude hiding (note)

import Contract.Transaction (TransactionHash)
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug')
import TrustlessSidechain.InitSidechain.Init
  ( insertScriptsIdempotent
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning
  ( getGovernancePoliciesAndValidators
  )
import Type.Row (type (+))

initGovernance ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (APP + r)
    { scriptsInitTxIds ∷ Array TransactionHash
    }
initGovernance sidechainParams version = do
  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize Governance versioning scripts"

  scriptsInitTxIds ← insertScriptsIdempotent
    getGovernancePoliciesAndValidators
    sidechainParams
    version

  pure { scriptsInitTxIds }
