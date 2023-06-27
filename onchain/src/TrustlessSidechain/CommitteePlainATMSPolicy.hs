{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | "TrustlessSidechain.CommitteePlainATMSPolicy" provides a token which verifies
 that the current committee has signed its token name with the plain (simply
 public key and signature concatenation) ATMS scheme.
-}
module TrustlessSidechain.CommitteePlainATMSPolicy where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value (CurrencySymbol, TokenName (..))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx (compile)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Trace qualified as Trace
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSPlainAggregatePubKey,
  ATMSPlainMultisignature (
    plainPublicKeys,
    plainSignatures
  ),
  CommitteeCertificateMint (
    committeeOraclePolicy,
    thresholdDenominator,
    thresholdNumerator
  ),
  SidechainPubKey (getSidechainPubKey),
  UpdateCommitteeDatum (aggregateCommitteePubKeys),
 )
import TrustlessSidechain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.Utils qualified as Utils (aggregateCheck, verifyMultisig)

{-# INLINEABLE mkMintingPolicy #-}

{- | 'mkMintingPolicy' verifies
      1. the provided committee in the redeemer matches the current committee
      stored onchain

      2. the token name of this token that is minted minted has been signed by
      the current committee
-}
mkMintingPolicy :: CommitteeCertificateMint -> ATMSPlainMultisignature -> ScriptContext -> Bool
mkMintingPolicy ccm atmspms ctx =
  traceIfFalse "error 'CommitteePlainATMSPolicy': current committee mismatch" isCurrentCommittee
    && traceIfFalse "error 'CommitteePlainATMSPolicy': committee signature invalid" signedByCurrentCommittee
  where
    info = scriptContextTxInfo ctx

    -- 1.
    isCurrentCommittee :: Bool
    isCurrentCommittee =
      Utils.aggregateCheck (plainPublicKeys atmspms) $
        aggregateCommitteePubKeys committeeDatum

    -- 2.
    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      Utils.verifyMultisig
        (getSidechainPubKey <$> plainPublicKeys atmspms)
        threshold
        (unTokenName uniqueMintedTokenName)
        (plainSignatures atmspms)

    threshold :: Integer
    threshold =
      -- Note [Threshold of Strictly More than Threshold Majority]
      --
      -- The spec wants us to have strictly more than numerator/denominator majority of the
      -- committee size. Let @n@ denote the committee size. To have strictly
      -- more than numerator/denominator majority, we are interested in the smallest integer that
      -- is strictly greater than @numerator/denominator*n@ which is either:
      --    1. if @numerator/denominator * n@ is an integer, then the smallest
      --    integer strictly greater than @numerator/denominator * n@ is
      --    @numerator/denominator * n + 1@.
      --
      --    2. if @numerator/denominator * n@ is not an integer, then the
      --    smallest integer is @ceil(numerator/denominator * n)@
      --
      -- We can capture both cases with the expression @floor((numerator * n)/denominator) + 1@
      -- via distinguishing cases (again) if @numerator/denominator * n@ is an integer.
      --
      --    1.  if @numerator/denominator * n@ is an integer, then
      --    @floor((numerator * n)/denominator) + 1 = (numerator *
      --    n)/denominator + 1@ is the smallest integer strictly greater than
      --    @numerator/denominator * n@ as required.
      --
      --    2.  if @numerator/denominator * n@ is not an integer, then
      --    @floor((numerator * n)/denominator)@ is the largest integer
      --    strictly smaller than @numerator/denominator *n@, but adding @+1@
      --    makes this smallest integer that is strictly larger than
      --    @numerator/denominator *n@ i.e., we have
      --    @ceil(numerator/denominator * n)@ as required.
      ( length (plainPublicKeys atmspms)
          `Builtins.multiplyInteger` thresholdNumerator ccm
          `Builtins.divideInteger` thresholdDenominator ccm
      )
        + 1

    committeeDatum :: UpdateCommitteeDatum ATMSPlainAggregatePubKey
    committeeDatum =
      let go :: [TxInInfo] -> UpdateCommitteeDatum ATMSPlainAggregatePubKey
          go (t : ts)
            | o <- txInInfoResolved t
              , amt <-
                  Value.valueOf
                    (txOutValue o)
                    (committeeOraclePolicy ccm)
                    UpdateCommitteeHash.initCommitteeHashMintTn
              , UpdateCommitteeHash.initCommitteeHashMintAmount == amt
              , -- We always expect this to be given as inline datum
                OutputDatum d <- txOutDatum o =
              IsData.unsafeFromBuiltinData $ getDatum d
            | otherwise = go ts
          go [] = traceError "error 'CommitteePlainATMSPolicy' no committee utxo given as reference input"
       in go $ txInfoReferenceInputs info ++ txInfoInputs info
    -- TODO probably should pass as redeemer whether we should look in
    -- reference inputs or regular inputs

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

    -- Grabs the unique token name (fails if this is not the case) of the this
    -- currency symbol that is minted.
    uniqueMintedTokenName :: TokenName
    uniqueMintedTokenName
      | Just tns <- AssocMap.lookup ownCurSymb $ Value.getValue (txInfoMint info)
        , [(tn, amt)] <- AssocMap.toList tns
        , amt > 0 =
        tn
      | otherwise = Trace.traceError "error 'CommitteePlainATMSPolicy': bad mint"

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkMintingPolicy . IsData.unsafeFromBuiltinData

serialisableMintingPolicy :: Versioned Script
serialisableMintingPolicy = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])) PlutusV2
