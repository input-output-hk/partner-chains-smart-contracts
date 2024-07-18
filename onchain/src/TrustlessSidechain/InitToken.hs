{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.InitToken (
  mkInitTokenPolicy,
  serialisableInitTokenPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (
  getValue,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  InitTokenRedeemer (BurnInitToken, MintInitToken),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

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
  Unsafe.SidechainParams ->
  InitTokenRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkInitTokenPolicy sp MintInitToken ctx
  | isJust . Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx =
      traceIfFalse "ERROR-INIT-TOKENS-01" isGenesisUtxoUsed
  where
    -- Ensure that the genesis UTxO is used by the transaction.
    isGenesisUtxoUsed :: Bool
    isGenesisUtxoUsed =
      Unsafe.genesisUtxo sp `elem` map Unsafe.txInInfoOutRef (Unsafe.txInfoInputs $ Unsafe.scriptContextTxInfo ctx)
mkInitTokenPolicy _ BurnInitToken ctx
  | Just cs <- Unsafe.decode <$> (Unsafe.getMinting . Unsafe.scriptContextPurpose $ ctx) =
      let
        -- What this transaction mints
        mintedValue = getValue (Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx)

        -- Amounts of own tokens minted/burned by transaction
        ownMintedAmounts :: [Integer]
        ownMintedAmounts =
          maybe
            []
            (map snd . AssocMap.toList)
            (AssocMap.lookup cs mintedValue)

        -- Check that we only burn tokens
        -- TODO replace with `all` (or `any`? why the `||`?)
        allBurned :: [Integer] -> Bool
        allBurned [] = True
        allBurned (x : xs) = x < 0 || allBurned xs
       in
        traceIfFalse "ERROR-INIT-TOKENS-02" (allBurned ownMintedAmounts)
mkInitTokenPolicy _ _ _ =
  traceError "ERROR-INIT-TOKENS-03"

mkInitTokenPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkInitTokenPolicyUntyped sp red ctx =
  check
    $ mkInitTokenPolicy
      (Unsafe.wrap sp)
      (PlutusTx.unsafeFromBuiltinData red)
      (Unsafe.wrap ctx)

serialisableInitTokenPolicy :: SerialisedScript
serialisableInitTokenPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkInitTokenPolicyUntyped||])
