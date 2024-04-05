module TrustlessSidechain.InitSidechain.MerkleRoot where

import Contract.Prelude
  ( Maybe(..)
  , bind
  , discard
  , not
  , null
  , pure
  , ($)
  )
import Contract.Transaction (TransactionHash)
import Run (Run)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain.Init (insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning
  ( getMerkleRootPoliciesAndValidators
  )
import Type.Row (type (+))

-- | Insert versioned MerkleRootTokenPolicy and
-- | MerkleRootTokenValidator. Note this does not
-- | burn any init tokens other than for the
-- | version oracle.
initMerkleRoot ∷
  ∀ r.
  SidechainParams →
  ATMSKinds →
  Int →
  Run (APP + r)
    ( Maybe
        { initTransactionIds ∷ Array TransactionHash
        , sidechainParams ∷ SidechainParams
        , sidechainAddresses ∷ SidechainAddresses
        }
    )
initMerkleRoot
  sidechainParams
  initATMSKind
  version = do

  logDebug' "Attempting to initialize MerkleRoot versioning scripts"
  scriptsInitTxId ← insertScriptsIdempotent getMerkleRootPoliciesAndValidators
    sidechainParams
    initATMSKind
    version

  if not $ null scriptsInitTxId then do
    sidechainAddresses ←
      GetSidechainAddresses.getSidechainAddresses $
        SidechainAddressesEndpointParams
          { sidechainParams
          , atmsKind: initATMSKind
          , usePermissionToken: false
          , version
          }

    pure
      ( Just
          { initTransactionIds: scriptsInitTxId
          , sidechainParams
          , sidechainAddresses
          }
      )
  else do
    logInfo' "Versioning scripts for MerkleRoot have already been initialized"
    pure Nothing
