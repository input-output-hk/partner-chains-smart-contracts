{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | "TrustlessSidechain.CommitteeSignedToken" provides a token which verifies
 that the current committee has signed a given message hash.
-}
module TrustlessSidechain.CommitteeSignedToken where

import PlutusTx.Prelude

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value (CurrencySymbol, TokenName (..))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (..),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx (compile)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.Types (
  CommitteeSignedTokenMint (..),
  CommitteeSignedTokenRedeemer (..),
  SidechainParams (..),
  SidechainPubKey (getSidechainPubKey),
  UpdateCommitteeHashDatum (..),
 )
import TrustlessSidechain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.Utils qualified as Utils (aggregateCheck, verifyMultisig)

{-# INLINEABLE mkMintingPolicy #-}

{- | 'mkMintingPolicy' verifies
      1. the provided committee in the redeemer matches the current committee
      stored onchain

      2. the message provided is signed by the current committee

      3. the only currency symbol of this token that is minted has the
      provided 'cstrMessageHash' as its token name

      TODO: we could generalize this to mint multiple tokens to show that the
      committee has signed multiple things in the same transaction by:
          - storing all the message hashes sorted alphabetically in a list
          [as token names are stored alphabetically -- TODO check this]
          - storing all signatures corresponding to the previous messages
      in the redeemer, and its straightforward to modify this.
-}
mkMintingPolicy :: CommitteeSignedTokenMint -> CommitteeSignedTokenRedeemer -> ScriptContext -> Bool
mkMintingPolicy cstm cstr ctx =
  traceIfFalse "error 'CommitteeSignedTokenMint': current committee mismatch" isCurrentCommittee
    && traceIfFalse "error 'CommitteeSignedTokenMint': committee signature invalid" signedByCurrentCommittee
    && traceIfFalse "error 'CommitteeSignedTokenMint': invalid mint" mintingChecks
  where
    sc = cstmSidechainParams cstm
    info = scriptContextTxInfo ctx

    -- 1.
    isCurrentCommittee :: Bool
    isCurrentCommittee = Utils.aggregateCheck (cstrCurrentCommittee cstr) $ committeeHash committeeDatum

    -- 2.
    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      Utils.verifyMultisig
        (getSidechainPubKey <$> cstrCurrentCommittee cstr)
        threshold
        (unTokenName $ cstrMessageHash cstr)
        (cstrCurrentCommitteeSignatures cstr)

    -- 3.
    mintingChecks :: Bool
    mintingChecks
      | Just tns <- AssocMap.lookup ownCurSymb $ Value.getValue (txInfoMint info)
        , [(tn, amt)] <- AssocMap.toList tns
        , tn == cstrMessageHash cstr
        , amt > 0 =
        True
      | otherwise = False

    threshold :: Integer
    threshold =
      -- See
      --    Note [Threshold of Strictly More than Threshold Majority]
      -- in the module "TrustlessSidechain.UpdateCommitteeHash"
      ( length (cstrCurrentCommittee cstr)
          `Builtins.multiplyInteger` thresholdNumerator sc
          `Builtins.divideInteger` thresholdDenominator sc
      )
        + 1

    committeeDatum :: UpdateCommitteeHashDatum
    committeeDatum =
      let go :: [TxInInfo] -> UpdateCommitteeHashDatum
          go (t : ts)
            | o <- txInInfoResolved t
              , amt <-
                  Value.valueOf
                    (txOutValue o)
                    (cstmUpdateCommitteeHashCurrencySymbol cstm)
                    UpdateCommitteeHash.initCommitteeHashMintTn
              , UpdateCommitteeHash.initCommitteeHashMintAmount == amt
              , -- See Note [Committee Hash Inline Datum] in
                -- "TrustlessSidechain.UpdateCommitteeHash"
                OutputDatum d <- txOutDatum o =
              IsData.unsafeFromBuiltinData $ getDatum d
            | otherwise = go ts
          go [] = traceError "error 'CommitteeSignedTokenMint' no committee utxo given as reference input"
       in go $ txInfoReferenceInputs info

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkMintingPolicy . IsData.unsafeFromBuiltinData

serialisableMintingPolicy :: Versioned Script
serialisableMintingPolicy = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])) PlutusV2
