{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.InitToken (
  mkInitTokenPolicy,
  serialisableInitTokenPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs, txInfoMint),
  Value (getValue),
  fromCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  InitTokenRedeemer (BurnInitToken, MintInitToken),
  SidechainParams,
 )
import TrustlessSidechain.Utils (mkUntypedMintingPolicy)

-- | 'mkInitTokenPolicy' is a minting policy which allows to:
--
--     * mint any amount of tokens when genesis UTxO is being spent
--
--     * always burn tokens
--
--  OnChain error descriptions:
--
--    ERROR-INIT-TOKENS-01: genesis UTxO not consumed by the transaction
--
--    ERROR-INIT-TOKENS-02: attempting to mint new tokens, but we expect to only
--    burn tokens
--
--    ERROR-INIT-TOKENS-02: invalid script purpose.  Script can only be used for
--    minting/burning
mkInitTokenPolicy ::
  SidechainParams ->
  InitTokenRedeemer ->
  ScriptContext ->
  Bool
mkInitTokenPolicy sp MintInitToken (ScriptContext txInfo (Minting _)) =
  traceIfFalse "ERROR-INIT-TOKENS-01" isGenesisUtxoUsed
  where
    -- Ensure that the genesis UTxO is used by the transaction.
    isGenesisUtxoUsed :: Bool
    isGenesisUtxoUsed =
      get @"genesisUtxo" sp `elem` map txInInfoOutRef (txInfoInputs txInfo)
mkInitTokenPolicy _ BurnInitToken (ScriptContext txInfo (Minting cs)) =
  traceIfFalse "ERROR-INIT-TOKENS-02" (allBurned ownMintedAmounts)
  where
    -- What this transaction mints
    mintedValue = getValue (txInfoMint txInfo)

    -- Amounts of own tokens minted/burned by transaction
    ownMintedAmounts :: [Integer]
    ownMintedAmounts =
      maybe
        []
        (map snd . AssocMap.toList)
        (AssocMap.lookup cs mintedValue)

    -- Check that we only burn tokens
    allBurned :: [Integer] -> Bool
    allBurned [] = True
    allBurned (x : xs) = x < 0 || allBurned xs
mkInitTokenPolicy _ _ _ =
  traceError "ERROR-INIT-TOKENS-03"

mkInitTokenPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkInitTokenPolicyUntyped =
  mkUntypedMintingPolicy
    . mkInitTokenPolicy
    . PlutusTx.unsafeFromBuiltinData

serialisableInitTokenPolicy :: Script
serialisableInitTokenPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkInitTokenPolicyUntyped||])
