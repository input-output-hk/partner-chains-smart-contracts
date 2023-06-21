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
import Ledger.Value (TokenName (TokenName), Value (getValue))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Address (addressCredential),
  Credential (ScriptCredential),
  CurrencySymbol,
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
  MerkleRootInsertionMessage (MerkleRootInsertionMessage),
  MerkleTreeEntry,
  SignedMerkleRootMint,
  mrimMerkleRoot,
  mrimPreviousMerkleRoot,
  smrmCommitteeCertificateVerificationCurrencySymbol,
  smrmValidatorHash,
 )

-- | 'serialiseMte' serialises a 'MerkleTreeEntry' with cbor via 'PlutusTx.Builtins.serialiseData'
{-# INLINEABLE serialiseMte #-}
serialiseMte :: MerkleTreeEntry -> BuiltinByteString
serialiseMte = Builtins.serialiseData . IsData.toBuiltinData

{- | 'serialiseMrimHash' is an alias for
 > PlutusTx.Builtins.blake2b_256 . PlutusTx.Builtins.serialiseData . PlutusTx.IsData.Class.toBuiltinData
-}
{-# INLINEABLE serialiseMrimHash #-}
serialiseMrimHash :: MerkleRootInsertionMessage -> BuiltinByteString
serialiseMrimHash = Builtins.blake2b_256 . Builtins.serialiseData . IsData.toBuiltinData

{- | 'mkMintingPolicy' verifies the following

      1. UTXO with the last Merkle root is referenced in the transaction.

      2.  the committee certificate verification minting policy asserts that
      `MerkleRootInsertionMessage` has been signed

      3. Exactly one token is minted

      TODO: the spec doesn't say this, but this is what the implementation
      does. Fairly certain this is what we want...

      4. At least one token is paid to 'smrmValidatorHash'
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SignedMerkleRootMint -> MerkleRootInsertionMessage -> ScriptContext -> Bool
mkMintingPolicy
  smrm
  mrim@MerkleRootInsertionMessage
    { mrimMerkleRoot
    , mrimPreviousMerkleRoot
    }
  ctx =
    traceIfFalse "error 'MerkleRootTokenMintingPolicy' previous merkle root not referenced" p1
      && traceIfFalse "error 'MerkleRootTokenMintingPolicy' committee certificate verification failed" p2
      && traceIfFalse "error 'MerkleRootTokenMintingPolicy' bad mint" p3
      && traceIfFalse "error 'MerkleRootTokenMintingPolicy' must pay to validator hash" p4
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx
      minted :: Value
      minted = txInfoMint info
      ownCurrencySymbol :: CurrencySymbol
      ownCurrencySymbol = Contexts.ownCurrencySymbol ctx
      ownTokenName :: TokenName
      ownTokenName = Value.TokenName mrimMerkleRoot

      -- Checks:
      -- @p1@, @p2@, @p3@, @p4@ correspond to verifications 1., 2., 3.,
      -- 4. resp. in the documentation of this function.
      p1 = case mrimPreviousMerkleRoot of
        Nothing -> True
        Just tn ->
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

      p2 =
        case AssocMap.lookup (smrmCommitteeCertificateVerificationCurrencySymbol smrm) $
          getValue minted of
          Nothing -> False
          Just tns -> case AssocMap.lookup
            (TokenName $ serialiseMrimHash mrim)
            tns of
            Just v | v > 0 -> True
            _ -> False

      p3 = case AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue minted of
        Nothing -> False
        Just tns -> case AssocMap.toList tns of
          [(tn, amount)] | tn == ownTokenName && amount == 1 -> True
          _ -> False

      p4 =
        let go [] = False
            go (txOut : txOuts) = case addressCredential (txOutAddress txOut) of
              ScriptCredential vh
                | vh == smrmValidatorHash smrm
                    && Value.valueOf (txOutValue txOut) ownCurrencySymbol ownTokenName
                    > 0 ->
                  True
              _ -> go txOuts
         in go $ txInfoOutputs info

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped =
  ScriptUtils.mkUntypedMintingPolicy
    . mkMintingPolicy
    . IsData.unsafeFromBuiltinData

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
