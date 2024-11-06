module TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  ) where

import Contract.Prelude hiding (note)

import Contract.Transaction (TransactionHash, TransactionInput)
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug')
import TrustlessSidechain.InitSidechain.Init
  ( insertScriptsIdempotent
  )
import TrustlessSidechain.Versioning
  ( getNativeTokenManagementPoliciesAndValidators
  )
import Type.Row (type (+))

initNativeTokenMgmt ::
  forall r.
  TransactionInput ->
  Run (APP + r)
    { scriptsInitTxIds :: Array TransactionHash
    }
initNativeTokenMgmt genesisUtxo = do
  -- Attempt to insert scripts into the versioning system
  logDebug' "Attempting to initialize Native Token Management versioning scripts"
  scriptsInitTxIds <- insertScriptsIdempotent
    getNativeTokenManagementPoliciesAndValidators
    genesisUtxo

  pure { scriptsInitTxIds }
