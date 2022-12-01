{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (divideInteger, modInteger)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (RootHash (RootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain.Types (SidechainParams (genesisMint))
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain), MerkleTreeEntry (mteAmount, mteRecipient))

{- | 'FUELMint' is the data type to parameterize the minting policy. See
 'mkMintingPolicy' for details of why we need the datum in 'FUELMint'
-}
data FUELMint = FUELMint
  { -- 'fmMptRootTokenValidator' is the hash of the validator script
    -- which /should/ have a token which has the merkle root in the token
    -- name. See 'TrustlessSidechain.OnChain.MPTRootTokenValidator' for
    -- details.
    -- > fmMptRootTokenValidator :: ValidatorHash
    -- N.B. We don't need this! We're really only interested in the token,
    -- and indeed; anyone can pay a token to this script so there really
    -- isn't a reason to use this validator script as the "identifier" for
    -- MPTRootTokens.

    -- | 'fmMptRootTokenCurrencySymbol' is the 'CurrencySymbol' of a token
    -- which contains a merkle root in the 'TokenName'. See
    -- 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy' for details.
    fmMptRootTokenCurrencySymbol :: CurrencySymbol
  , -- | 'fmSidechainParams' is the sidechain parameters
    fmSidechainParams :: SidechainParams
  , -- | 'fmDsKeyCurrencySymbol' is th currency symbol for the tokens which
    -- hold the key for the distributed set. In particular, this allows the
    -- FUEL minting policy to verify if a string has /just been inserted/ into
    -- the distributed set.
    fmDsKeyCurrencySymbol :: CurrencySymbol
  }

PlutusTx.makeLift ''FUELMint
PlutusTx.makeIsDataIndexed ''FUELMint [('FUELMint, 0)]

{- | 'fuelTokenName' is a constant for the token name of FUEL (the currency of
 the side chain).
-}
{-# INLINEABLE fuelTokenName #-}
fuelTokenName :: TokenName
fuelTokenName = TokenName "FUEL"

{- | 'mkMintingPolicy' verifies the following

  1. MPTRootToken with the name of the Merkle root of the transaction
  calculated from the proof) can be found in the MPTRootTokenValidator.

  N.B. this performs this verification on the FIRST merkle root of the
  MPTRootToken it finds in the inputs. So, to ensure "predictable" behaviour
  [order of inputs in a transaction is not defined, but as of August 10, 2022
  there is a [CIP](https://github.com/cardano-foundation/CIPs/pull/231) to
  allow us to define it], the transaction should be constructed s.t. there is
  exactly only one input with an MPTRootToken.

  2. chainId matches the minting policy id

  TODO: It doesn't do this yet? Honestly, I'm quite unsure what this means?

  3. amount matches the actual tx body contents

  4. The recipient has signed the transaction

  TODO: this isn't in the spec, but we have to do this to ensure that only the
  recipient is the one who is controlling where the FUEL goes i.e., so an
  adversary can't impersonate the recipient to steal their FUEL.
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: FUELMint -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy fm mode ctx = case mode of
  MainToSide _ ->
    traceIfFalse "Can't burn a positive amount" (fuelAmount < 0)
  SideToMain mte mp ->
    let cborMte :: BuiltinByteString
        cborMte = MPTRootTokenMintingPolicy.serialiseMte mte

        cborMteHashed = Builtins.blake2b_256 cborMte --  TODO: actually hash this later.
        dsInserted :: BuiltinByteString
        dsInserted
          | Just tns <- AssocMap.lookup dsKeyCurrencySymbol $ getValue minted
            , (tn, _amt) : _ <- AssocMap.toList tns =
            unTokenName tn
          | otherwise = traceError "error 'FUELMintingPolicy' inserting wrong distributed set element"

        merkleRoot :: RootHash
        merkleRoot =
          let go :: [TxInInfo] -> TokenName
              go (t : ts)
                | o <- txInInfoResolved t
                  , Just tns <- AssocMap.lookup mptRootTnCurrencySymbol $ getValue $ txOutValue o
                  , [(tn, _amt)] <- AssocMap.toList tns =
                  tn
                -- If it's more clear, the @[(tn,_amt)] <- AssocMap.toList tns@
                -- can be rewritten as
                -- > [(tn,amt)] <- AssocMap.toList tns, amt == 1
                -- where from
                -- 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.mkMintingPolicy'
                -- we can be certain there is only ONE distinct TokenName for
                -- each 'CurrencySymbol'
                --
                -- Actually, I suppose someone could mint multiple of the
                -- token, then collect them all in a single transaction..
                -- Either way, it doens't matter -- the existence of the token
                -- is enough to conclude that the current committee has signed
                -- it.
                | otherwise = go ts
              go [] = traceError "error 'FUELMintingPolicy' no Merkle root found"
           in RootHash $ unTokenName $ go $ txInfoReferenceInputs info
     in traceIfFalse
          "error 'FUELMintingPolicy' tx not signed by recipient"
          (maybe False (Contexts.txSignedBy info) (bech32AddrToPubKeyHash (mteRecipient mte)))
          &&
          -- TODO: remove the oneshot minting policy later... yeah..
          --
          -- Why do we even have it? Well because the IOG people like using it
          -- to test for now, even though it's not what we will use later and
          -- the distributed set replaces it.
          ( case genesisMint $ fmSidechainParams fm of
              Nothing ->
                traceIfFalse "error 'FUELMintingPolicy' incorrect amount of FUEL minted" (fuelAmount == mteAmount mte)
                  && traceIfFalse
                    "error 'FUELMintingPolicy' merkle proof failed"
                    (MerkleTree.memberMp cborMte mp merkleRoot)
                  && traceIfFalse
                    "error 'FUELMintingPolicy' not inserting into distributed set"
                    (dsInserted == cborMteHashed)
              Just gutxo ->
                traceIfFalse "Oneshot Mintingpolicy utxo not present" $
                  let -- One shot minting policy checks.
                      hasUTxO :: TxOutRef -> Bool
                      hasUTxO utxo = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info
                   in hasUTxO gutxo
          )
  where
    -- Aliases:
    info = scriptContextTxInfo ctx
    ownCurrencySymbol = Contexts.ownCurrencySymbol ctx
    mptRootTnCurrencySymbol = fmMptRootTokenCurrencySymbol fm
    dsKeyCurrencySymbol = fmDsKeyCurrencySymbol fm
    minted = txInfoMint info

    fuelAmount :: Integer
    fuelAmount
      | Just tns <- AssocMap.lookup ownCurrencySymbol $ getValue minted
        , [(tn, amount)] <- AssocMap.toList tns
        , tn == fuelTokenName =
        amount
      | otherwise = traceError "error 'FUELMintingPolicy' illegal FUEL minting"

-- ctl hack
-- https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutus-comparison.md#applying-arguments-to-parameterized-scripts
{-# INLINEABLE mkMintingPolicyUntyped #-}
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = mkUntypedMintingPolicy . mkMintingPolicy . unsafeFromBuiltinData

serialisableMintingPolicy :: Script
serialisableMintingPolicy = fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

{- | Deriving the public key hash from a bech32 binary
 -   For more details on the bech32 format refer to https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019
 -   TODO: In later versions, we can use bytewise primitives
-}
{-# INLINEABLE bech32AddrToPubKeyHash #-}
bech32AddrToPubKeyHash :: BuiltinByteString -> Maybe PubKeyHash
bech32AddrToPubKeyHash addr =
  let header = indexByteString addr 0 `divideInteger` 16
   in if header `modInteger` 2 == 0
        then Just $ PubKeyHash $ sliceByteString 1 28 addr
        else Nothing
