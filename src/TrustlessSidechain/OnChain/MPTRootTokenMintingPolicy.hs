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
import TrustlessSidechain.OnChain.Types (SignedMerkleRoot (SignedMerkleRoot, committeePubKeys, merkleRoot, signatures, threshold))
import TrustlessSidechain.OnChain.Utils qualified as Utils

{- | 'MerkleTreeEntry' is the data which are the elements in the merkle tree
 for the MPTRootToken.
-}
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    mteIndex :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    mteAmount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    mteRecipient :: BuiltinByteString
  , -- | sidechain epoch for which merkle tree was created
    mteSidechainEpoch :: Integer
  }

PlutusTx.makeLift ''MerkleTreeEntry
PlutusTx.makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

{- | 'serialiseMerkleTreeEntry' serialises a 'MerkleTreeEntry' with cbor.

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
serialiseMerkleTreeEntry :: MerkleTreeEntry -> BuiltinByteString
serialiseMerkleTreeEntry = mteRecipient

{- | 'mkMintingPolicy' verifies the following

      1. UTXO with the last Merkle root is referenced in the transaction.

      TODO: The spec mentions this, but this currently doesn't do this.

      2. the signature can be verified with the submitted publc key hashes of
      committee members, AND the concatenated hashed value of these
      signatures correspond to the one saved on chain.

      3. Exactly one token is minted

      TODO: the spec doesn't say this, but this is what the implementation
      does.
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
    Utils.verifyMultisig (map (getLedgerBytes . Ledger.getPubKey) committeePubKeys) threshold merkleRoot signatures
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
