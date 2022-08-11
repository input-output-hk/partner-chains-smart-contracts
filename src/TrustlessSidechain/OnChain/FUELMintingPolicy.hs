{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Ledger (
  MintingPolicy,
  ScriptContext (scriptContextTxInfo),
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Script
import Plutus.V1.Ledger.Contexts (TxInInfo (txInInfoResolved), TxInfo (txInfoInputs, txInfoMint, txInfoOutputs), TxOut (txOutValue), TxOutRef)
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.Tx qualified as Tx
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (unTokenName), Value (getValue))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (RootHash (RootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain.Types (
  SidechainParams (
    genesisMint
  ),
 )
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy (MerkleTreeEntry (MerkleTreeEntry, mteAmount, mteIndex, mteRecipient, mteSidechainEpoch))
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

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
  }

PlutusTx.makeLift ''FUELMint

{- | 'fuelTokenName' is a constant for the token name of FUEL (the currency of
 the side chain).
-}
fuelTokenName :: TokenName
fuelTokenName = "FUEL"

{- | 'mkMintingPolicy' verifies the following

  1. MPTRootToken with the name of the Merkle root of the transaction
  calculated from the proof) can be found in the MPTRootTokenValidator.

  N.B. this performs this verification on the FIRST merkle root of the
  MPTRootToken it finds in the inputs. So, to ensure "predictable" behaviour
  [order of inputs in a transaction is not defined, but as of August 10, 2022
  there is a [CIP](https://github.com/cardano-foundation/CIPs/pull/231) to
  allow us to define it], the transaction should be constructed s.t. there is
  exactly only one input with an MPTRootToken.

  2. chainId the minting policy id

  TODO: It doesn't do this yet? Honestly, I'm quite unsure what this means?

  3. recipient and amount matches the actual tx body contents

  TODO: I assume this is checked when verifying this is in the merkle root? See
  @tx@ for some more details -- this needs to be fixed later.

  4. The recipient recieves the amount minted in one transaction output.

  TODO: this isn't in the spec, but the following note will make this clear why
  we should have this.

  N.B. This guarantees that the recipient will receive the entire amount they
  minted (i.e., transferred over from the main chain). Technically, this would
  reject more transactions than necessary (e.g. someone could want to have this
  pay half of their FUEL to one transaction and another the rest to another
  transaction output), but then to support this we would have to verify
  uniqueness of the recipient as well. Hence, we disallow such types of
  transactions in general. [this is open for discussion!]
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: FUELMint -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy fm mode ctx = case mode of
  MainToSide _ ->
    traceIfFalse "Can't burn a positive amount" (fuelAmount < 0)
  SideToMain mp ->
    traceIfFalse "Can't mint a negative amount" (fuelAmount > 0)
      && traceIfFalse "error 'mkMintingPolicy' merkle proof failed" (MerkleTree.memberMp tx mp merkleRoot)
      && traceIfFalse "Oneshot Mintingpolicy utxo not present" oneshotMintAndUTxOPresent
  where
    -- Aliases:
    info = scriptContextTxInfo ctx
    ownCurrencySymbol = Contexts.ownCurrencySymbol ctx
    mptRootTnCurrencySymbol = fmMptRootTokenCurrencySymbol fm
    sc = fmSidechainParams fm
    minted = txInfoMint info

    fuelAmount :: Integer
    fuelAmount
      | Just tns <- AssocMap.lookup ownCurrencySymbol $ getValue minted
        , [(tn, amount)] <- AssocMap.toList tns
        , tn == fuelTokenName =
        amount
      | otherwise = traceError "error 'mkMintingPolicy' illegal FUEL minting"

    merkleRoot :: RootHash
    merkleRoot =
      -- N.B. we could use 'PlutusTx.Foldable.asum' but I'm fairly certain
      -- the strictness of Plutus won't be what we want i.e., it would always
      -- do a linear scan EVEN if it could terminate early.
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
            | otherwise = go ts
          go [] = traceError "error 'mkMintingPolicy' missing MPTRootToken"
       in RootHash $ unTokenName $ go $ txInfoInputs info

    recipient :: PubKeyHash
    recipient =
      let go :: [TxOut] -> PubKeyHash
          go (o : os)
            | Just r <- Tx.txOutPubKey o
              , Just tns <- AssocMap.lookup ownCurrencySymbol $ getValue $ txOutValue o
              , (_tn, amt) : _ <- AssocMap.toList tns
              , -- If it's more clear, we can write the above line as
                -- > [ (tn, amt) ] <- AssocMap.toList tns, tn == fuelTokenName
                -- but the extra checks are implicit by construction.
                amt == fuelAmount =
              r
            | otherwise = go os
          go [] = traceError "error 'mkMintingPolicy' no recipient found"
       in go $ txInfoOutputs info

    tx :: BuiltinByteString
    tx =
      MPTRootTokenMintingPolicy.encodeMerkleTreeEntry
        -- TODO: fill this in with proper data later
        MerkleTreeEntry
          { mteIndex = 0
          , mteAmount = fuelAmount
          , mteRecipient = getPubKeyHash recipient
          , mteSidechainEpoch = 0
          }

    -- Checks:
    hasUTxO :: TxOutRef -> Bool
    hasUTxO utxo = any (\i -> Ledger.txInInfoOutRef i == utxo) $ txInfoInputs info

    oneshotMintAndUTxOPresent :: Bool
    oneshotMintAndUTxOPresent = maybe True hasUTxO $ genesisMint sc

mintingPolicy :: FUELMint -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(PlutusTx.compile [||Script.wrapMintingPolicy . mkMintingPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
