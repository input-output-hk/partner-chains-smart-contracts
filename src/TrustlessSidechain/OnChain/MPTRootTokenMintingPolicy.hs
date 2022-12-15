{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy where

import PlutusTx.Prelude

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value (TokenName (TokenName))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (MintingPolicy)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (..),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.OffChain.Types (
  SidechainParams (
    genesisUtxo,
    thresholdDenominator,
    thresholdNumerator
  ),
  SidechainPubKey (getSidechainPubKey),
  convertSCParams,
 )
import TrustlessSidechain.OnChain.Types (MerkleRootInsertionMessage (..), MerkleTreeEntry, SignedMerkleRoot (..), UpdateCommitteeHashDatum (committeeHash))
import TrustlessSidechain.OnChain.UpdateCommitteeHash (InitCommitteeHashMint (InitCommitteeHashMint, icTxOutRef))
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.OnChain.Utils qualified as Utils (verifyMultisig)

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

      Note: actually, this condition is just done offchain because we can trust
      the bridge is going to do the right thing when referencing the
      transaction.

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
    , previousMerkleRoot
    }
  ctx =
    traceIfFalse "error 'MPTRootTokenMintingPolicy' previous merkle root not referenced" p1
      && traceIfFalse "error 'MPTRootTokenMintingPolicy' verifyMultisig failed" p2
      && traceIfFalse "error 'MPTRootTokenMintingPolicy' committee hash mismatch" p3
      && traceIfFalse "error 'MPTRootTokenMintingPolicy' bad mint" p4
    where
      info = scriptContextTxInfo ctx
      minted = txInfoMint info
      ownCurrencySymbol = Contexts.ownCurrencySymbol ctx
      ownTokenName = Value.TokenName merkleRoot
      sc = smrmSidechainParams smrm

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
                , -- See Note [Committee Hash Inline Datum] in
                  -- 'TrustlessSidechain.OnChain.UpdateCommitteeHash'
                  OutputDatum d <- txOutDatum o =
                IsData.unsafeFromBuiltinData $ getDatum d
              | otherwise = go ts
            go [] = traceError "error 'MPTRootTokenMintingPolicy' no committee utxo given as reference input"
         in go (txInfoReferenceInputs info)

      threshold :: Integer
      threshold =
        -- See Note [Threshold of Strictly More than Threshold Majority] in
        -- 'TrustlessSidechain.OnChain.UpdateCommitteeHash' (this is mostly
        -- duplicated from there)
        ( length committeePubKeys
            `Builtins.multiplyInteger` thresholdNumerator sc
            `Builtins.divideInteger` thresholdDenominator sc
        )
          + 1

      -- Checks:
      -- @p1@, @p2@, @p3@, @p4@ correspond to verifications 1., 2., 3., 4. resp. in the
      -- documentation of this function.
      p1, p2, p3, p4 :: Bool
      p1 = case previousMerkleRoot of
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
                  > 0 || go rest
              go [] = False
           in go (txInfoReferenceInputs info)
      p2 =
        Utils.verifyMultisig
          (map getSidechainPubKey committeePubKeys)
          threshold
          ( serialiseMrimHash
              MerkleRootInsertionMessage
                { mrimSidechainParams = convertSCParams $ smrmSidechainParams smrm
                , mrimMerkleRoot = merkleRoot
                , mrimPreviousMerkleRoot = previousMerkleRoot
                }
          )
          signatures
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

serialisableMintingPolicy :: Versioned Script
serialisableMintingPolicy = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])) PlutusV2
