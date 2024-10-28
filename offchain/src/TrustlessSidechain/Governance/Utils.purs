module TrustlessSidechain.Governance.Utils
  ( approveByGovernanceLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.Types.BigInt as BigInt
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints
  ( TxConstraints
  )
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
  )
import TrustlessSidechain.Governance (Governance(MultiSig))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(GovernancePolicy))
import TrustlessSidechain.Versioning.Types (VersionOracle(VersionOracle))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

approveByGovernanceLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
approveByGovernanceLookupsAndConstraints sidechainParams = do
  ownPaymentPubKeyHash <- getOwnPaymentPubKeyHash

  governancePlutusScriptHash <- Versioning.getVersionedScriptHash
    sidechainParams
    (VersionOracle { scriptId: GovernancePolicy })

  (governanceRefTxInput /\ governanceRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      (VersionOracle { scriptId: GovernancePolicy })

  pure $ Governance.approveByGovernanceLookupsAndConstraints
    ( MultiSig $ MultiSigGovParams
        { governanceMembers: [ unwrap ownPaymentPubKeyHash ]
        , requiredSignatures: BigInt.fromInt 1
        }
    )
    governancePlutusScriptHash
    governanceRefTxInput
    governanceRefTxOutput