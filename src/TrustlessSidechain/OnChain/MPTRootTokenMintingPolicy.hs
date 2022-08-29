{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import Ledger (
  MintingPolicy,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInfo (TxInfo),
 )
import Ledger qualified
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Bytes (getLedgerBytes)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (MerkleTreeEntry (mteHash), SignedMerkleRoot (SignedMerkleRoot, committeePubKeys, merkleRoot, signatures, threshold))
import TrustlessSidechain.OnChain.Utils qualified as Utils

{- | 'serialiseMte' serialises a 'MerkleTreeEntry' with cbor.

 TODO: it doesn't encode the 'MerkleTreeEntry' to @cbor@. We would like to
 use something like
 [PlutusTx.serialiseData](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Builtins.hs#L373)
 but for some reason it doesn't exist in the package plutus-tx for the
 version that we are using?

 It appears that we are using
 > plutus-tx                         >= 0.1.0 && < 0.2,
 which doesn't have our desired function, but version 1.0.0.0 does have it.

 While we wait, we /could/ actually reimplement such functionality onchain
 (but it would be very slow and expensive probably). See package plutus-core
 in module @PlutusCore.Data@
-}
serialiseMte :: MerkleTreeEntry -> BuiltinByteString
serialiseMte = mteHash

{- | 'mkMintingPolicy' verifies the following

      1. UTXO with the last Merkle root is referenced in the transaction.

      TODO: The spec mentions this, but this currently doesn't do this.

      2. the signature can be verified with the submitted publc key hashes of
      committee members, AND the concatenated hashed value of these
      signatures correspond to the one saved on chain.

      TODO: I don't think this does the latter of the conditions? i.e., it
      appears it does NOT check the concatenated hashed valuee of the committee
      submited correspond to the one saved on chain (recall the committee hash
      is saved via 'TrustlessSidechain.OnChain.UpdateCommitteeHash')

      3. Exactly one token is minted

      TODO: the spec doesn't say this, but this is what the implementation
      does. Fairly certain this is what we want...
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> SignedMerkleRoot -> ScriptContext -> Bool
mkMintingPolicy
  _
  SignedMerkleRoot
    { merkleRoot
    , signatures
    , committeePubKeys
    , threshold
    }
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    -- 2.
    traceIfFalse
      "error 'MPTRootTokenMintingPolicy' verifyMultisig failed"
      (Utils.verifyMultisig (map (getLedgerBytes . Ledger.getPubKey) committeePubKeys) threshold merkleRoot signatures)
      && verifyTokenAmount (traceIfFalse "Amount must be 1" . (== 1)) -- 3.
    where
      verifyTokenAmount verify =
        case Value.flattenValue txInfoMint of
          [(sym, name, amount)] ->
            verify amount
              && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
              && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
          _ -> False
      ownTokenName = Value.TokenName merkleRoot
mkMintingPolicy _ _ _ = False

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(PlutusTx.compile [||wrap . mkMintingPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
  where
    wrap = Script.wrapMintingPolicy

mintingPolicyCurrencySymbol :: SidechainParams -> CurrencySymbol
mintingPolicyCurrencySymbol = Contexts.scriptCurrencySymbol . mintingPolicy
