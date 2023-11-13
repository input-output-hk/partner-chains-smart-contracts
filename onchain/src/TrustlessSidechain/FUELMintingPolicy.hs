{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.FUELMintingPolicy (
  fuelTokenName,
  mkMintingPolicy,
  mkMintingPolicyUntyped,
  serialisableMintingPolicy,
  serialisableBurningPolicy,
  bech32AddrToPubKeyHash,
) where

import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  PubKeyHash (PubKeyHash),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TokenName (TokenName, unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoReferenceInputs),
  TxOut (txOutValue),
  Value (getValue),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (divideInteger, modInteger)
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.MerkleRootTokenMintingPolicy qualified as MerkleRootTokenMintingPolicy
import TrustlessSidechain.MerkleTree (RootHash (RootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (mkUntypedMintingPolicy)
import TrustlessSidechain.Types (
  FUELMintingRedeemer (FUELBurningRedeemer, FUELMintingRedeemer),
  SidechainParams,
 )
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  dsKeyPolicyId,
  getVersionedCurrencySymbol,
  merkleRootTokenPolicyId,
 )

{- | 'fuelTokenName' is a constant for the token name of FUEL (the currency of
 the side chain).
-}
{-# INLINEABLE fuelTokenName #-}
fuelTokenName :: TokenName
fuelTokenName = TokenName "FUEL"

{- | 'mkMintingPolicy' verifies the following

  1. MerkleRootToken with the name of the Merkle root of the transaction
  calculated from the proof) can be found in the MerkleRootTokenValidator.

  N.B. this performs this verification on the FIRST merkle root of the
  MerkleRootToken it finds in the inputs. So, to ensure "predictable" behaviour
  [order of inputs in a transaction is not defined, but as of August 10, 2022
  there is a [CIP](https://github.com/cardano-foundation/CIPs/pull/231) to
  allow us to define it], the transaction should be constructed s.t. there is
  exactly only one input with an MerkleRootToken.

  2. chainId matches the minting policy id

  TODO: It doesn't do this yet? Honestly, I'm quite unsure what this means?

  3. amount matches the actual tx body contents

  4. The recipient has signed the transaction

  TODO: this isn't in the spec, but we have to do this to ensure that only the
  recipient is the one who is controlling where the FUEL goes i.e., so an
  adversary can't impersonate the recipient to steal their FUEL.

  OnChain errors

  ERROR-FUEL-MINTING-POLICY-01: this tx should mint negative amount of FUEL token

  ERROR-FUEL-MINTING-POLICY-02: inserting wrong distributed set element

  ERROR-FUEL-MINTING-POLICY-03: no Merkle root found

  ERROR-FUEL-MINTING-POLICY-04: tx not signed by recipient

  ERROR-FUEL-MINTING-POLICY-05: incorrect amount of FUEL minted

  ERROR-FUEL-MINTING-POLICY-06: merkle proof failed

  ERROR-FUEL-MINTING-POLICY-07: not inserting into distributed set

  ERROR-FUEL-MINTING-POLICY-08: illegal FUEL minting
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> VersionOracleConfig -> FUELMintingRedeemer -> ScriptContext -> Bool
mkMintingPolicy _ _ FUELBurningRedeemer (ScriptContext txInfo (Minting currSymbol)) =
  traceIfFalse "ERROR-FUEL-MINTING-POLICY-01" noTokensMinted
  where
    noTokensMinted :: Bool
    noTokensMinted =
      case AssocMap.lookup currSymbol $
        Value.getValue (txInfoMint txInfo) of
        Just tns -> AssocMap.all (< 0) tns
        _ -> traceError "ERROR-FUEL-MINTING-POLICY-01"
mkMintingPolicy _sp versioningConfig (FUELMintingRedeemer mte mp) ctx =
  let cborMte :: BuiltinByteString
      cborMte = MerkleRootTokenMintingPolicy.serialiseMte mte

      cborMteHashed = Builtins.blake2b_256 cborMte --  TODO: actually hash this later.
      dsInserted :: BuiltinByteString
      dsInserted
        | Just tns <- AssocMap.lookup dsKeyCurrencySymbol $ getValue minted
          , (tn, _amt) : _ <- AssocMap.toList tns =
          unTokenName tn
        | otherwise = traceError "ERROR-FUEL-MINTING-POLICY-02"

      merkleRoot :: RootHash
      merkleRoot =
        let go :: [TxInInfo] -> TokenName
            go (t : ts)
              | o <- txInInfoResolved t
                , Just tns <- AssocMap.lookup merkleRootTnCurrencySymbol $ getValue $ txOutValue o
                , [(tn, _amt)] <- AssocMap.toList tns =
                tn
              -- If it's more clear, the @[(tn,_amt)] <- AssocMap.toList tns@
              -- can be rewritten as
              -- > [(tn,amt)] <- AssocMap.toList tns, amt == 1
              -- where from
              -- 'TrustlessSidechain.MerkleRootTokenMintingPolicy.mkMintingPolicy'
              -- we can be certain there is only ONE distinct TokenName for
              -- each 'CurrencySymbol'
              --
              -- Actually, I suppose someone could mint multiple of the
              -- token, then collect them all in a single transaction..
              -- Either way, it doens't matter -- the existence of the token
              -- is enough to conclude that the current committee has signed
              -- it.
              | otherwise = go ts
            go [] = traceError "ERROR-FUEL-MINTING-POLICY-03"
         in RootHash $ LedgerBytes $ unTokenName $ go $ txInfoReferenceInputs info
   in traceIfFalse
        "ERROR-FUEL-MINTING-POLICY-04"
        (maybe False (Contexts.txSignedBy info) (bech32AddrToPubKeyHash (get @"recipient" mte)))
        && traceIfFalse "ERROR-FUEL-MINTING-POLICY-05" (fuelAmount == get @"amount" mte)
        && traceIfFalse "ERROR-FUEL-MINTING-POLICY-06" (MerkleTree.memberMp cborMte mp merkleRoot)
        && traceIfFalse "ERROR-FUEL-MINTING-POLICY-07" (dsInserted == cborMteHashed)
  where
    -- Aliases:
    info :: TxInfo
    info = scriptContextTxInfo ctx
    ownCurrencySymbol :: CurrencySymbol
    ownCurrencySymbol = Contexts.ownCurrencySymbol ctx
    merkleRootTnCurrencySymbol :: CurrencySymbol
    merkleRootTnCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = merkleRootTokenPolicyId})
        ctx
    dsKeyCurrencySymbol :: CurrencySymbol
    dsKeyCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = dsKeyPolicyId})
        ctx
    minted :: Value
    minted = txInfoMint info
    fuelAmount :: Integer
    fuelAmount
      | Just tns <- AssocMap.lookup ownCurrencySymbol $ getValue minted
        , [(tn, amount)] <- AssocMap.toList tns
        , tn == fuelTokenName =
        amount
      | otherwise = traceError "ERROR-FUEL-MINTING-POLICY-08"
mkMintingPolicy _ _ _ _ = False

-- ctl hack
-- https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutus-comparison.md#applying-arguments-to-parameterized-scripts
{-# INLINEABLE mkMintingPolicyUntyped #-}
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped sp versioningConfig = mkUntypedMintingPolicy $ mkMintingPolicy (unsafeFromBuiltinData sp) (unsafeFromBuiltinData versioningConfig)

serialisableMintingPolicy :: Script
serialisableMintingPolicy = fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

-- Burning policy defines the criteria for burrning FUEL Proxy token.
-- Currently there is no requirement that would prevent user from burning their
-- FUEL Proxy token, as long as they actually have one. This policy don't have
-- to check, wether user has FUEL Proxy tokens or not, because cardano itself
-- won't allow for burning tokens, that the user doesn't have.
{-# INLINEABLE mkBurningPolicy #-}
mkBurningPolicy :: SidechainParams -> () -> ScriptContext -> Bool
mkBurningPolicy _ () _ = True

{-# INLINEABLE mkBurningPolicyUntyped #-}
mkBurningPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurningPolicyUntyped = mkUntypedMintingPolicy . mkBurningPolicy . unsafeFromBuiltinData

serialisableBurningPolicy :: Script
serialisableBurningPolicy = fromCompiledCode $$(PlutusTx.compile [||mkBurningPolicyUntyped||])

{- | Deriving the public key hash from a bech32 binary
 -   For more details on the bech32 format refer to https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019
 -   TODO: In later versions, we can use bytewise primitives
-}
{-# INLINEABLE bech32AddrToPubKeyHash #-}
bech32AddrToPubKeyHash :: LedgerBytes -> Maybe PubKeyHash
bech32AddrToPubKeyHash (LedgerBytes addr) =
  let header = indexByteString addr 0 `divideInteger` 16
   in if header `modInteger` 2 == 0
        then Just $ PubKeyHash $ sliceByteString 1 28 addr
        else Nothing
