{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | "TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy" provides a token which verifies
-- that the current committee has signed its token name with the plain (simply
-- public key and signature concatenation) ATMS scheme with EcdsaSecp256k1 signatures.
module TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy (
  mkMintingPolicy,
  serialisableMintingPolicy,
) where

import PlutusLedgerApi.Common (
  SerialisedScript,
  serialiseCompiledCode
  )
import PlutusTx (compile)
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
-- 'verifyEcdsaSecp256k1Signature' to verify the signatures
mkMintingPolicy :: CommitteeCertificateMint -> VersionOracleConfig -> ATMSRedeemer -> Unsafe.ScriptContext -> Bool
mkMintingPolicy =
  CommitteePlainATMSPolicy.mkMintingPolicy
    verifyEcdsaSecp256k1Signature

mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped ccm versionOracleConfig redeemer ctx =
  check $
    mkMintingPolicy
      (unsafeFromBuiltinData ccm)
      (unsafeFromBuiltinData versionOracleConfig)
      (unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy = serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
