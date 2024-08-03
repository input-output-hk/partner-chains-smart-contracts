{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.MerkleRootTokenMintingPolicy (
  serialiseMte,
  serialiseMrimHash,
  mkMintingPolicy,
  mkMintingPolicyUntyped,
  serialisableMintingPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  LedgerBytes (LedgerBytes, getLedgerBytes),
  ScriptContext,
  TokenName (TokenName, unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutValue),
  Value (getValue),
  scriptContextTxInfo,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2.Contexts qualified as Contexts
import PlutusTx (compile)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    merkleRoot,
    previousMerkleRoot,
    sidechainParams
  ),
  MerkleTreeEntry,
  SidechainParams,
  SignedMerkleRootRedeemer,
 )
import TrustlessSidechain.Utils (mkUntypedMintingPolicy)
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  committeeCertificateVerificationPolicyId,
  getVersionedCurrencySymbol,
  getVersionedValidatorAddress,
  merkleRootTokenValidatorId,
 )

-- | 'serialiseMte' serialises a 'MerkleTreeEntry' with cbor via 'PlutusTx.Builtins.serialiseData'
{-# INLINEABLE serialiseMte #-}
serialiseMte :: MerkleTreeEntry -> BuiltinByteString
serialiseMte = Builtins.serialiseData . IsData.toBuiltinData

-- | 'serialiseMrimHash' is an alias for
-- > PlutusTx.Builtins.blake2b_256 . PlutusTx.Builtins.serialiseData . PlutusTx.IsData.Class.toBuiltinData
{-# INLINEABLE serialiseMrimHash #-}
serialiseMrimHash :: MerkleRootInsertionMessage -> LedgerBytes
serialiseMrimHash =
  LedgerBytes . Builtins.blake2b_256 . Builtins.serialiseData . IsData.toBuiltinData

-- | 'mkMintingPolicy' verifies the following
--
--   1. UTXO with the last Merkle root is referenced in the transaction.
--
--   2. the committee certificate verification minting policy asserts that
--      `MerkleRootInsertionMessage` has been signed, exactly one token is
--      minted, and At least one token is paid to 'validatorHash'
--
-- OnChain error descriptions:
--
--   ERROR-MERKLE-ROOT-POLICY-01: Previous merkle root not referenced.
--
--   ERROR-MERKLE-ROOT-POLICY-02: Transaction does not mint exactly one own
--   token.
--
--   ERROR-MERKLE-ROOT-POLICY-03: Committee certificate verification failed.
--
--   ERROR-MERKLE-ROOT-POLICY-04: Token not paid to correct validator address.
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> VersionOracleConfig -> SignedMerkleRootRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  sp
  versionOracleConfig
  smrr
  ctx =
    traceIfFalse "ERROR-MERKLE-ROOT-POLICY-01" p1
      && traceIfFalse "ERROR-MERKLE-ROOT-POLICY-02" p2
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx
      minted :: Value
      minted = txInfoMint info
      ownCurrencySymbol :: CurrencySymbol
      ownCurrencySymbol = Contexts.ownCurrencySymbol ctx

      merkleRootTokenValidatorAddress =
        getVersionedValidatorAddress
          versionOracleConfig
          ( VersionOracle
              { version = 1
              , scriptId = merkleRootTokenValidatorId
              }
          )
          ctx

      committeeCertificateVerificationPolicy =
        getVersionedCurrencySymbol
          versionOracleConfig
          ( VersionOracle
              { version = 1
              , scriptId = committeeCertificateVerificationPolicyId
              }
          )
          ctx

      -- Checks:
      -- @p1@, @p2@ correspond to verifications 1., 2. resp. in the
      -- documentation of this function.
      p1 = case get @"previousMerkleRoot" smrr of
        Nothing -> True
        Just (LedgerBytes tn) ->
          -- Checks if any of the reference inputs have at least 1 of the last
          -- merkle root.
          let go :: [TxInInfo] -> Bool
              go (txInInfo : rest) =
                Value.valueOf
                  (txOutValue (txInInfoResolved txInInfo))
                  ownCurrencySymbol
                  (TokenName tn)
                  > 0
                  || go rest
              go [] = False
           in go (txInfoReferenceInputs info)

      p2 = case AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue minted of
        Nothing -> False
        Just tns -> case AssocMap.toList tns of
          -- assert that there is a unique token name (and only one) minted of
          -- this currency symbol
          [(tn, amount)]
            | amount == 1 ->
              let msg =
                    MerkleRootInsertionMessage
                      { sidechainParams = sp
                      , merkleRoot = LedgerBytes $ unTokenName tn
                      , previousMerkleRoot = get @"previousMerkleRoot" smrr
                      }
               in traceIfFalse
                    "ERROR-MERKLE-ROOT-POLICY-03"
                    ( Value.valueOf
                        minted
                        committeeCertificateVerificationPolicy
                        (TokenName (getLedgerBytes (serialiseMrimHash msg)))
                        > 0
                    )
                    && traceIfFalse
                      "ERROR-MERKLE-ROOT-POLICY-04"
                      ( let go [] = False
                            go (txOut : txOuts) =
                              ( ( txOutAddress txOut == merkleRootTokenValidatorAddress
                                    && Value.valueOf (txOutValue txOut) ownCurrencySymbol tn
                                    > 0
                                )
                                  || go txOuts
                             )
                         in go $ txInfoOutputs info
                    )
          _ -> False

mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped sp versioningConfig =
  mkUntypedMintingPolicy $ mkMintingPolicy (IsData.unsafeFromBuiltinData sp) (IsData.unsafeFromBuiltinData versioningConfig)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
