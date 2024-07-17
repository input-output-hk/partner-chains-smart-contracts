module TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  ) where

import Contract.Prelude hiding (note)

import Contract.Transaction (TransactionHash)
import Run (Run)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug')
import TrustlessSidechain.InitSidechain.Init
  ( insertScriptsIdempotent
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning
  ( getNativeTokenManagementPoliciesAndValidators
  )
import Type.Row (type (+))

initNativeTokenMgmt ∷
  ∀ r.
  SidechainParams →
  ATMSKinds →
  Int →
  Run (APP + r)
    { scriptsInitTxIds ∷ Array TransactionHash
    }
initNativeTokenMgmt sidechainParams initATMSKind version = do
  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize Native Token Management versioning scripts"
  scriptsInitTxIds ← insertScriptsIdempotent
    getNativeTokenManagementPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  pure { scriptsInitTxIds }
