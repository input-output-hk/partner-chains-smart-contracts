module TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
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
  ( getNativeTokenManagementPoliciesAndValidators
  )
import Type.Row (type (+))

initNativeTokenMgmt ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP + r)
    { scriptsInitTxIds :: Array TransactionHash
    }
initNativeTokenMgmt sidechainParams version = do
  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize Native Token Management versioning scripts"
  scriptsInitTxIds <- insertScriptsIdempotent
    getNativeTokenManagementPoliciesAndValidators
    sidechainParams
    version

  pure { scriptsInitTxIds }
