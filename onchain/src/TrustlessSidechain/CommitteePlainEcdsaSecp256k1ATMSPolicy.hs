{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | "TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy" provides a token which verifies
 that the current committee has signed its token name with the plain (simply
 public key and signature concatenation) ATMS scheme with EcdsaSecp256k1 signatures.
-}
module TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext,
 )
import PlutusTx (compile)
import TrustlessSidechain.CommitteePlainATMSPolicy qualified as CommitteePlainATMSPolicy
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSPlainMultisignature,
  CommitteeCertificateMint,
 )
import TrustlessSidechain.Versioning (VersionOracleConfig)

{-# INLINEABLE mkMintingPolicy #-}

{- | 'mkMintingPolicy' wraps
 'TrustlessSidechain.CommitteePlainATMSPolicy.mkMintingPolicy' and uses
 'verifyEcdsaSecp256k1Signature' to verify the signatures
-}
mkMintingPolicy :: CommitteeCertificateMint -> VersionOracleConfig -> ATMSPlainMultisignature -> ScriptContext -> Bool
mkMintingPolicy =
  CommitteePlainATMSPolicy.mkMintingPolicy
    verifyEcdsaSecp256k1Signature

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped ccm versionOracleConfig =
  ScriptUtils.mkUntypedMintingPolicy $ (mkMintingPolicy (unsafeFromBuiltinData ccm) (unsafeFromBuiltinData versionOracleConfig))

serialisableMintingPolicy :: Versioned Script
serialisableMintingPolicy = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])) PlutusV2
