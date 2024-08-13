{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | "TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy" provides a token which verifies
-- that the current committee has signed its token name with the plain (simply
-- public key and signature concatenation) ATMS scheme with SchnorrSecp256k1 signatures.
module TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy (
  mkMintingPolicy,
  serialisableMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.CommitteePlainATMSPolicy qualified as CommitteePlainATMSPolicy
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSRedeemer,
  CommitteeCertificateMint,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning (VersionOracleConfig)

{-# INLINEABLE mkMintingPolicy #-}

-- | 'mkMintingPolicy' wraps
-- 'TrustlessSidechain.CommitteePlainATMSPolicy.mkMintingPolicy' and uses
-- 'verifySchnorrSecp256k1Signature' to verify the signatures
mkMintingPolicy :: CommitteeCertificateMint -> VersionOracleConfig -> ATMSRedeemer -> Unsafe.ScriptContext -> Bool
mkMintingPolicy =
  CommitteePlainATMSPolicy.mkMintingPolicy
    verifySchnorrSecp256k1Signature

mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped ccm versionOracleConfig redeemer ctx =
  check $
    mkMintingPolicy
      (unsafeFromBuiltinData ccm)
      (unsafeFromBuiltinData versionOracleConfig)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: Script
serialisableMintingPolicy = fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
