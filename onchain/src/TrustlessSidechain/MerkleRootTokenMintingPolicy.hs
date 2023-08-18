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

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value (TokenName (TokenName, unTokenName), Value (getValue))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  LedgerBytes (LedgerBytes, getLedgerBytes),
  Script,
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  TxOut (txOutAddress, txOutValue),
  scriptContextTxInfo,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
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
  merkleRoot,
 )
import TrustlessSidechain.Versioning qualified as Versioning

-- | 'serialiseMte' serialises a 'MerkleTreeEntry' with cbor via 'PlutusTx.Builtins.serialiseData'
{-# INLINEABLE serialiseMte #-}
serialiseMte :: MerkleTreeEntry -> BuiltinByteString
serialiseMte = Builtins.serialiseData . IsData.toBuiltinData

{- | 'serialiseMrimHash' is an alias for
 > PlutusTx.Builtins.blake2b_256 . PlutusTx.Builtins.serialiseData . PlutusTx.IsData.Class.toBuiltinData
-}
{-# INLINEABLE serialiseMrimHash #-}
serialiseMrimHash :: MerkleRootInsertionMessage -> LedgerBytes
serialiseMrimHash =
  LedgerBytes . Builtins.blake2b_256 . Builtins.serialiseData . IsData.toBuiltinData

{- | 'mkMintingPolicy' verifies the following

      1. UTXO with the last Merkle root is referenced in the transaction.

      2.  the committee certificate verification minting policy asserts that
      `MerkleRootInsertionMessage` has been signed, exactly one token is minted,
      and At least one token is paid to 'validatorHash'
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> Versioning.VersionOracleConfig -> SignedMerkleRootRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  sp
  versionOracleConfig
  smrr
  ctx =
    traceIfFalse "error 'MerkleRootTokenMintingPolicy' previous merkle root not referenced" p1
      && traceIfFalse "error 'MerkleRootTokenMintingPolicy' bad mint" p2
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx
      minted :: Value
      minted = txInfoMint info
      ownCurrencySymbol :: CurrencySymbol
      ownCurrencySymbol = Contexts.ownCurrencySymbol ctx

      merkleRootTokenValidatorAddress =
        Versioning.getVersionedValidatorAddress versionOracleConfig (Versioning.VersionOracle {version = 1, scriptId = 2}) ctx
      committeeCertificateVerificationPolicy =
        Versioning.getVersionedCurrencySymbol versionOracleConfig (Versioning.VersionOracle {version = 1, scriptId = 18}) ctx

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
                    "error 'MerkleRootTokenMintingPolicy' committee certificate verification failed"
                    ( Value.valueOf
                        minted
                        committeeCertificateVerificationPolicy
                        (TokenName (getLedgerBytes (serialiseMrimHash msg)))
                        > 0
                    )
                    && traceIfFalse
                      "error 'MerkleRootTokenMintingPolicy' token not paid to correct validator address"
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

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped sp versioningConfig =
  ScriptUtils.mkUntypedMintingPolicy $ mkMintingPolicy (IsData.unsafeFromBuiltinData sp) (IsData.unsafeFromBuiltinData versioningConfig)

serialisableMintingPolicy :: Versioned Script
serialisableMintingPolicy =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])) PlutusV2

-- Helpers

{-# INLINE flattenValue #-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue = go . AssocMap.toList . getValue
  where
    go ::
      [(CurrencySymbol, AssocMap.Map TokenName Integer)] ->
      [(CurrencySymbol, TokenName, Integer)]
    go = \case
      [] -> []
      ((cs, innerMap) : xs) -> goInner cs xs . AssocMap.toList $ innerMap
    goInner ::
      CurrencySymbol ->
      [(CurrencySymbol, AssocMap.Map TokenName Integer)] ->
      [(TokenName, Integer)] ->
      [(CurrencySymbol, TokenName, Integer)]
    goInner cs carryBack = \case
      [] -> go carryBack
      ((tn, i) : xs) ->
        if i == 0
          then goInner cs carryBack xs
          else (cs, tn, i) : goInner cs carryBack xs
