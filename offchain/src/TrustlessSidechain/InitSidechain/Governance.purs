module TrustlessSidechain.InitSidechain.Governance
  ( initGovernance
  ) where

import Contract.Prelude hiding (note)

import Cardano.Types.BigInt as BigInt
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Contract.Transaction (TransactionHash)
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  , multisigGovPolicy
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(GovernancePolicy))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

initGovernance ::
  forall r.
  SidechainParams ->
  PaymentPubKeyHash ->
  Run (APP + r) TransactionHash
initGovernance sidechainParams govPubKeyHash = do
  plutusScript <- multisigGovPolicy $ MultiSigGovParams
    { requiredSignatures: BigInt.fromInt 1
    , governanceMembers: [ unwrap govPubKeyHash ]
    }
  lookupsAndConstraints <- Versioning.initializeVersionLookupsAndConstraints
    sidechainParams
    (Tuple GovernancePolicy plutusScript)
  balanceSignAndSubmit "Initialize Governance" lookupsAndConstraints
