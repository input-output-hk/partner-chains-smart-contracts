{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | "TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy" provides a token which verifies
 that the current committee has signed its token name with the plain (simply
 public key and signature concatenation) ATMS scheme with SchnorrSecp256k1 signatures.
-}
module TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext,
  fromCompiledCode,
 )
import PlutusTx (compile)
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.CommitteePlainATMSPolicy qualified as CommitteePlainATMSPolicy
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (mkUntypedMintingPolicy)
import TrustlessSidechain.Types (
  ATMSPlainMultisignature,
  CommitteeCertificateMint,
 )

{-# INLINEABLE mkMintingPolicy #-}

{- | 'mkMintingPolicy' wraps
 'TrustlessSidechain.CommitteePlainATMSPolicy.mkMintingPolicy' and uses
 'verifySchnorrSecp256k1Signature' to verify the signatures
-}
mkMintingPolicy :: CommitteeCertificateMint -> ATMSPlainMultisignature -> ScriptContext -> Bool
mkMintingPolicy =
  CommitteePlainATMSPolicy.mkMintingPolicy
    verifySchnorrSecp256k1Signature

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = mkUntypedMintingPolicy . mkMintingPolicy . IsData.unsafeFromBuiltinData

serialisableMintingPolicy :: Script
serialisableMintingPolicy = fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
