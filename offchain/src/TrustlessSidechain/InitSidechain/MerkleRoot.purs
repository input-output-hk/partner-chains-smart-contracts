module TrustlessSidechain.InitSidechain.MerkleRoot where

import Contract.Prelude (Maybe(..), bind, discard, pure, ($))
import Contract.Transaction (TransactionHash)
import Contract.Value (TokenName)
import Data.Unit (Unit, unit)
import Run (Run)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.DataStoragePolicy
  ( createDataStorage
  , mkDataStorageTokenName
  , retrieveDataStorage
  )
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug')
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain.Init (insertScriptsIdempotent)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning (getMerkleRootPoliciesAndValidators)
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
  exists ∷ Maybe Unit ← retrieveDataStorage dataStorageTokenName sidechainParams
  case exists of
    Nothing → do
      logDebug' "Attempting to initialize MerkleRoot versioning scripts"
      scriptsInitTxId ← insertScriptsIdempotent getMerkleRootPoliciesAndValidators
        sidechainParams
        initATMSKind
        version

      sidechainAddresses ←
        GetSidechainAddresses.getSidechainAddresses $
          SidechainAddressesEndpointParams
            { sidechainParams
            , atmsKind: initATMSKind
            , usePermissionToken: false
            , version
            }

      createDataStorage dataStorageTokenName sidechainParams unit

      pure
        ( Just
            { initTransactionIds: scriptsInitTxId
            , sidechainParams
            , sidechainAddresses
            }
        )
    Just _ → pure Nothing
  where
  dataStorageTokenName ∷ TokenName
  dataStorageTokenName = mkDataStorageTokenName "InitMklRtComp"
