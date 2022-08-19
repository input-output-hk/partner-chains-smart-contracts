module MPTRoot where
{-
import SidechainParams (SidechainParams)
import ScriptsFFI (fUELMintingPolicy)
import Data.BigInt as BigInt
import Contract.Prelude (class Generic, Unit, Void, bind, discard, negate, one, show, wrap, zero, (<$>), (<>), (=<<), (>>>))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractAffM, liftContractM, liftedE, liftedM)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy(..), PlutusScript(..), applyArgs)
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Value as Value
import Contract.Transaction ( awaitTxConfirmed , balanceAndSignTx , submit)
import Contract.TxConstraints as Constraints
import Contract.ScriptLookups as Lookups
import Contract.PlutusData (class ToData , toData , PlutusData(Constr))
import Contract.Address (PaymentPubKeyHash)

data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot       ∷ String
  , signatures       ∷ [String]
  , threshold        ∷ BigInt -- Natural: the number of committee pubkeys needed to sign off
  , committeePubKeys ∷ [PubKey] -- Public keys of all committee members
  }
derive instance Generic SignedMerkleRoot _
instance ToData SignedMerkleRoot where
  toData (SignedMerkleRoot s1 s2) = Constr zero [ toData s1 , toData s2 ]

newtype SaveRootParams = SaveRootParams
  { sidechainParams  ∷ SidechainParams
  , merkleRoot       ∷ String
  , signatures       ∷ [String]
  , threshold        ∷ BigInt 
  , committeePubKeys ∷ [PubKey] -- Public keys of all committee members
  }
derive instance Generic SaveRootParams _
derive instance Newtype SaveRootParams _
instance ToData SaveRootParams where
  toData (SaveRootParams { sidechainParams , merkleRoot , signatures , threshold , committeePubKeys })
    = Constr zero [ toData sidechainParams , toData merkleRoot , toData signatures
                  , toData threshold , toData committeePubKeys ]

rootTokenMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
rootTokenMintingPolicy sp = do
  mptRootMP ← (PlutusScript >>> MintingPolicy) <$> textEnvelopeBytes mPTRootMintingPolicy PlutusScriptV1
  liftedE (applyArgs mptRootMP [ toData sp ])

saveRoot ∷ SaveRootParams → Contract () ()
saveRoot SaveRootParams {sidechainParams, merkleRoot, threshold, signatures, committeePubKeys} = do
  rootTokenMP ← rootTokenMintingPolicy sidechainParams
  rootTokenCS ← liftContractAffM "Cannot get currency symbol" (Value.scriptCurrencySymbol rootTokenMP)
  let policy   = MintingPolicy.mintingPolicy sidechainParams
      value    = Value.singleton rootTokenCS (Value.TokenName merkleRoot) (BigInt.fromInt 1)
      redeemer = SignedMerkleRoot {merkleRoot, signatures, threshold, committeePubKeys}
      constraints ∷ Constraints.TxConstraints Void Void
      constraints = Constraint.mustMintValueWithRedeemer (wrap (toData redeemer)) value
                 <> Constraint.mustPayToOtherScript (Validator.hash sidechainParams) Ledger.unitDatum value
  ubTx ← liftedE (Lookups.mkUnbalancedTx (Lookups.mintingPolicy rootTokenMP) constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted saveRoot Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "saveRoot Tx submitted successfully!"

--saveRoot ∷ SaveRootParams → Contract () TrustlessSidechainSchema Text CardanoTx
--saveRoot SaveRootParams {sidechainParams, merkleRoot, threshold, signatures, committeePubKeys} = do
--  let policy = MintingPolicy.mintingPolicy sidechainParams
--      value = Value.singleton (Ledger.scriptCurrencySymbol policy) (Value.TokenName merkleRoot) 1
--      redeemer = Redeemer $ toBuiltinData SignedMerkleRoot {merkleRoot, signatures, threshold, committeePubKeys}
--  Contract.submitTxConstraintsWith @SignedMerkleRoot
--    (Constraint.mintingPolicy policy)
--    ( Constraint.mustMintValueWithRedeemer redeemer value
--        <> Constraint.mustPayToOtherScript (Validator.hash sidechainParams) Ledger.unitDatum value
--    )
-}
