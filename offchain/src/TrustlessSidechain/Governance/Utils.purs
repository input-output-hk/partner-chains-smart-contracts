module TrustlessSidechain.Governance.Utils
  ( approvedByGovernanceLookupsAndConstraints
  , updateGovernance
  ) where

import Contract.Prelude

import Cardano.Types.BigInt as BigInt
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.TransactionHash (TransactionHash)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints
  ( TxConstraints
  )
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
  )
import TrustlessSidechain.Governance (Governance(MultiSig))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  , multisigGovPolicy
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(GovernancePolicy))
import TrustlessSidechain.Versioning.Types (VersionOracle(VersionOracle))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

approvedByGovernanceLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
approvedByGovernanceLookupsAndConstraints sidechainParams = do
  ownPaymentPubKeyHash <- getOwnPaymentPubKeyHash

  governancePlutusScriptHash <- Versioning.getVersionedScriptHash
    sidechainParams
    (VersionOracle { scriptId: GovernancePolicy })

  (governanceRefTxInput /\ governanceRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      (VersionOracle { scriptId: GovernancePolicy })

  pure $ Governance.approvedByGovernanceLookupsAndConstraints
    ( MultiSig $ MultiSigGovParams
        { governanceMembers: [ unwrap ownPaymentPubKeyHash ]
        , requiredSignatures: BigInt.fromInt 1
        }
    )
    governancePlutusScriptHash
    governanceRefTxInput
    governanceRefTxOutput

updateGovernance ::
  forall r.
  SidechainParams ->
  PaymentPubKeyHash ->
  Run (APP + r)
    TransactionHash
updateGovernance sidechainParams newGovernancePubKeyHash = do
  plutusScript <- multisigGovPolicy $ MultiSigGovParams
    { requiredSignatures: BigInt.fromInt 1
    , governanceMembers: [ unwrap newGovernancePubKeyHash ]
    }

  lookupsAndConstraints <- Versioning.updateVersionLookupsAndConstraints
    sidechainParams
    GovernancePolicy
    plutusScript
  balanceSignAndSubmit "Update Governance" lookupsAndConstraints