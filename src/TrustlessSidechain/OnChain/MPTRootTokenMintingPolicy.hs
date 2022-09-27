{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import PlutusTx.Prelude

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (MintingPolicy)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  LedgerBytes (getLedgerBytes),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (..),
  TxInInfo (txInInfoResolved),
  TxInfo (..),
  TxOut (txOutDatum, txOutValue),
 )
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx qualified
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.OffChain.Types (SidechainParams (genesisUtxo))
import TrustlessSidechain.OnChain.Types (MerkleTreeEntry (mteHash), SignedMerkleRoot (..), UpdateCommitteeHashDatum (committeeHash))
import TrustlessSidechain.OnChain.UpdateCommitteeHash (InitCommitteeHashMint (InitCommitteeHashMint, icTxOutRef))
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.OnChain.Utils qualified as Utils (verifyMultisig)

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

-- | 'SignedMerkleRootMint' is used to parameterize 'mkMintingPolicy'.
data SignedMerkleRootMint = SignedMerkleRootMint
  { -- | 'smrmSidechainParams' includes the 'SidechainParams'
    smrmSidechainParams :: SidechainParams
  , -- | 'smrmUpdateCommitteeHashCurrencySymbol' is the 'CurrencySymbol' which
    -- identifies the utxo for which the 'UpdateCommitteeHashDatum'
    -- resides.
    smrmUpdateCommitteeHashCurrencySymbol :: CurrencySymbol
  }

PlutusTx.makeLift ''SignedMerkleRootMint
PlutusTx.makeIsDataIndexed ''SignedMerkleRootMint [('SignedMerkleRootMint, 0)]

{- | 'signedMerkleRootMint' is a smart constructor to create the 'SignedMerkleRootMint'.

 TODO: Not totally too sure why we need the sidechain params here in the
 parameter, but it was like that before, so we'll leave it there. As an
 optimization, we could get rid of the sidechain params.
-}
signedMerkleRootMint :: SidechainParams -> SignedMerkleRootMint
signedMerkleRootMint sc =
  SignedMerkleRootMint
    { smrmSidechainParams = sc
    , smrmUpdateCommitteeHashCurrencySymbol =
        UpdateCommitteeHash.committeeHashCurSymbol
          InitCommitteeHashMint {icTxOutRef = genesisUtxo sc}
    }

{- | 'mkMintingPolicy' verifies the following

      1. UTXO with the last Merkle root is referenced in the transaction.

      TODO: The spec mentions this, but this currently doesn't do this.
      Actually I'm not really sure what this achieves / why we need to do
      this.. and this certainly begs the question of what to do for the first
      cross chain transaction when there is no last merkle root.

      2.  the signature can be verified with the submitted public key hashes of
      committee members, and the list of public keys are unique

      3. the concatenated and hashed value of the public keys correspond to the
      one saved on-chain in 'TrustlessSidechain.OnChain.UpdatingCommitteeHash'

      4. Exactly one token is minted

      TODO: the spec doesn't say this, but this is what the implementation
      does. Fairly certain this is what we want...
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SignedMerkleRootMint -> SignedMerkleRoot -> ScriptContext -> Bool
mkMintingPolicy
  smrm
  SignedMerkleRoot
    { merkleRoot
    , signatures
    , committeePubKeys
    , threshold
    }
  ctx =
    and
      [ -- TODO: the first condition isn't done yet.. See 1. in the function documentation
        traceIfFalse "error 'MPTRootTokenMintingPolicy' last merkle root not referenced" p1
      , traceIfFalse "error 'MPTRootTokenMintingPolicy' verifyMultisig failed" p2
      , traceIfFalse "error 'MPTRootTokenMintingPolicy' committee hash mismatch" p3
      , traceIfFalse "error 'MPTRootTokenMintingPolicy' bad mint" p4
      ]
    where
      info = scriptContextTxInfo ctx
      minted = txInfoMint info
      ownTokenName = Value.TokenName merkleRoot

      committeeDatum :: UpdateCommitteeHashDatum
      committeeDatum =
        let go :: [TxInInfo] -> UpdateCommitteeHashDatum
            go (t : ts)
              | o <- txInInfoResolved t
                , amt <-
                    Value.valueOf
                      (txOutValue o)
                      (smrmUpdateCommitteeHashCurrencySymbol smrm)
                      UpdateCommitteeHash.initCommitteeHashMintTn
                , UpdateCommitteeHash.initCommitteeHashMintAmount == amt
                , -- TODO can we use inline datums? not sure how to datumHash -> datum in v2
                  -- updateCommitteeHashDatum contains just a hash
                  OutputDatum d <- txOutDatum o -- Just dh <- txOutDatumHash o , Just d <- findDatum dh info
                =
                IsData.unsafeFromBuiltinData $ getDatum d
              | otherwise = go ts
            go [] = traceError "error 'MPTRootTokenMintingPolicy' no committee utxo found"
         in go (txInfoInputs info)

      -- Checks:
      -- @p1@, @p2@, @p3@, @p4@ correspond to verifications 1., 2., 3., 4. resp. in the
      -- documentation of this function.
      p1, p2, p3, p4 :: Bool
      p1 = True -- TODO: it doesn't do this yet.
      p2 = Utils.verifyMultisig (map (getLedgerBytes . Ledger.getPubKey) committeePubKeys) threshold merkleRoot signatures
      p3 = UpdateCommitteeHash.aggregateCheck committeePubKeys $ committeeHash committeeDatum
      p4 = case Value.flattenValue minted of
        [(_sym, tn, amt)] -> amt == 1 && tn == ownTokenName
        -- There's no need to verify the following condition
        -- > sym == Contexts.ownCurrencySymbol ctx
        -- since we know that the the minting script is run in the case we are
        -- minting a token, and we pattern match to guarantee that there is
        -- only one token being minted namely this token.
        _ -> False

-- | 'mintingPolicy' is the minting policy for the signed merkle root tokens
mintingPolicy :: SignedMerkleRootMint -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = ScriptUtils.mkUntypedMintingPolicy

-- CTL hack
mkMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMintingPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkMintingPolicy . IsData.unsafeFromBuiltinData

serialisableMintingPolicy :: Script
serialisableMintingPolicy = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
