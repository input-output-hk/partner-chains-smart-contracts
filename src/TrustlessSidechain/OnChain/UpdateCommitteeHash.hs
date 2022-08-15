{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.UpdateCommitteeHash where

import Cardano.Crypto.Wallet (XPrv)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (PubKey)
import Ledger qualified
import Ledger.Address (Address)
import Ledger.Crypto qualified as Crypto
import Ledger.Value (AssetClass)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  LedgerBytes (getLedgerBytes),
  MintingPolicy,
  TokenName (TokenName),
  Validator,
  Value,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
  TxOutRef,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import Plutus.V2.Ledger.Tx (OutputDatum (..))
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude as PlutusTx
import TrustlessSidechain.MerkleTree qualified as MT
import TrustlessSidechain.OnChain.Types (
  UpdateCommitteeHash (cToken),
  UpdateCommitteeHashDatum (UpdateCommitteeHashDatum, committeeHash),
  UpdateCommitteeHashRedeemer (committeePubKeys, committeeSignatures, newCommitteeHash),
 )
import TrustlessSidechain.OnChain.Utils (verifyMultisig)
import Prelude qualified

-- * Updating the committee hash

{- | 'aggregateKeys' aggregates a list of public keys into a single
 committee hash by essentially computing the merkle root of all public keys
 together.
 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [PubKey] -> BuiltinByteString
aggregateKeys [] = traceError "Empty committee"
aggregateKeys lst = MT.unRootHash $ MT.rootHash $ MT.fromList $ map (getLedgerBytes . Crypto.getPubKey) lst

{- Note [Aggregate Keys Append Scheme]
 In early versions, we used a "simple append scheme" i.e., we implemented this function with
  > aggregateKeys = Builtins.blake2b_256 . mconcat . map (Bytes.getLedgerBytes . Crypto.getPubKey)
  but this didn't work in the Plutip integration tests (budget exceeded errors), so we jumped straight
  to the merkle root solution instead.
 -}

{- | 'aggregateCheck' takes a sequence of public keys and an aggregate public
 key, and returns true or false to determinig whether the public keys were
 used to produce the aggregate public key
-}
{-# INLINEABLE aggregateCheck #-}
aggregateCheck :: [PubKey] -> BuiltinByteString -> Bool
aggregateCheck pubKeys avk = aggregateKeys pubKeys == avk

{- | 'verifyMultiSignature' is a wrapper for how we verify multi signatures.

 TODO: For now, to simplify things we just test if any of the committee has
 signed the message, and we should do a proper multisign later.
-}
{-# INLINEABLE verifyMultiSignature #-}
verifyMultiSignature ::
  [PubKey] -> BuiltinByteString -> BuiltinByteString -> Bool
verifyMultiSignature pubKeys msg sig = any go pubKeys
  where
    go pubKey =
      let pubKey' = getLedgerBytes (Crypto.getPubKey pubKey)
       in PlutusTx.verifyEd25519Signature pubKey' msg sig

{- | 'multiSign'' is a wrapper for how multiple private keys can sign a message.
Warning: there should be a non-empty number of private keys.

We put this function here (even though it isn't used in the on chain code)
because it corresponds to the 'verifyMultiSignature'

TODO: For now, to simplify things we just make the first person sign the message.

TODO: do a proper multisign later.
-}
multiSign :: BuiltinByteString -> [XPrv] -> BuiltinByteString
multiSign msg (prvKey : _) = Crypto.getSignature (Crypto.sign' msg prvKey)
multiSign _ _ = traceError "Empty multisign"

{- | 'mkUpdateCommitteeHashValidator' is the on-chain validator. We test for the following conditions

  1. The native token is in both the input and output.

  2. The new committee hash is signed by the current committee

  3. The committee provided really is the current committee

  4. The new output transaction contains the new committee hash

TODO an optimization. Instead of putting the new committee hash in the
redeemer, we could just:

    1. check if the committee hash is included in the datum (we already do
    this)

    2. check if what is in the datum is signed by the current committee

Note [Committee hash in output datum]:
Normally, the producer of a utxo is only required to include the datum hash,
and not the datum itself (but can optionally do so). In this case, we rely on
the fact that the producer actually does include the datum; and enforce this
with 'outputDatum'.

Note [Input has Token and Output has Token]:
In an older iteration, we used to check if the tx's input has the token, but
this is implicitly covered when checking if the output spends the token. Hence,
we don't need to check if the input tx's spends the token which is a nice
little optimization.
-}
{-# INLINEABLE mkUpdateCommitteeHashValidator #-}
mkUpdateCommitteeHashValidator ::
  UpdateCommitteeHash ->
  UpdateCommitteeHashDatum ->
  UpdateCommitteeHashRedeemer ->
  ScriptContext ->
  Bool
mkUpdateCommitteeHashValidator uch dat red ctx =
  traceIfFalse "Token missing from output" outputHasToken
    && traceIfFalse "Committee signature missing" signedByCurrentCommittee
    && traceIfFalse "Wrong committee" isCurrentCommittee
    && traceIfFalse "Wrong output datum" (outputDatum == UpdateCommitteeHashDatum (newCommitteeHash red))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownOutput :: TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "Expected exactly one committee output"

    outputDatum :: UpdateCommitteeHashDatum
    outputDatum =
      fromMaybe (traceError "Committee output datum missing") $
        case txOutDatum ownOutput of
          NoOutputDatum -> Nothing
          OutputDatum d -> Just d
          OutputDatumHash dh -> Contexts.findDatum dh info -- TODO: better return Nothing and disallow datumhashes entirely?
          >>= PlutusTx.fromBuiltinData . getDatum

    outputHasToken :: Bool
    outputHasToken = hasNft (txOutValue ownOutput)

    hasNft :: Value -> Bool
    hasNft val = Value.assetClassValueOf val (cToken uch) == 1

    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      verifyMultisig
        (getLedgerBytes . Crypto.getPubKey <$> committeePubKeys red)
        1
        (newCommitteeHash red)
        (committeeSignatures red) -- TODO where are the other signatures?
    isCurrentCommittee :: Bool
    isCurrentCommittee = aggregateCheck (committeePubKeys red) $ committeeHash dat

-- | 'updateCommitteeHashValidator' is the validator of the script
updateCommitteeHashValidator :: UpdateCommitteeHash -> Validator
updateCommitteeHashValidator updateCommitteeHash =
  Ledger.mkValidatorScript
    ( $$(PlutusTx.compile [||untypedValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode updateCommitteeHash
    )
  where
    untypedValidator = ScriptUtils.mkUntypedValidator . mkUpdateCommitteeHashValidator

-- | 'updateCommitteeHashAddress' is the address of the script
{-# INLINEABLE updateCommitteeHashAddress #-}
updateCommitteeHashAddress :: UpdateCommitteeHash -> Address
updateCommitteeHashAddress = Ledger.scriptHashAddress . ScriptUtils.validatorHash . updateCommitteeHashValidator

-- * Initializing the committee hash

-- | 'InitCommitteeHashMint' is used as the parameter for the minting policy
newtype InitCommitteeHashMint = InitCommitteeHashMint
  { -- | 'TxOutRef' is the output reference to mint the NFT initially.
    icTxOutRef :: TxOutRef
  }
  deriving newtype (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic, PlutusTx.UnsafeFromData)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''InitCommitteeHashMint

{- | 'initCommitteeHashMintTn'  is the token name of the NFT which identifies
 the utxo which contains the committee hash. We use an empty bytestring for
 this because the name really doesn't matter, so we mighaswell save a few
 bytes by giving it the empty name.
-}
{-# INLINEABLE initCommitteeHashMintTn #-}
initCommitteeHashMintTn :: TokenName
initCommitteeHashMintTn = TokenName Builtins.emptyByteString

{- | 'mkCommitteeHashPolicy' is the minting policy for the NFT which identifies
 the committee hash.
-}
{-# INLINEABLE mkCommitteeHashPolicy #-}
mkCommitteeHashPolicy :: InitCommitteeHashMint -> () -> ScriptContext -> Bool
mkCommitteeHashPolicy ichm _red ctx =
  traceIfFalse "UTxO not consumed" hasUtxo
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oref :: TxOutRef
    oref = icTxOutRef ichm

    hasUtxo :: Bool
    hasUtxo = any ((oref ==) . txInInfoOutRef) $ txInfoReferenceInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(_cs, tn', amt)] -> tn' == initCommitteeHashMintTn && amt == 1
      -- Note: we don't need to check that @cs == Contexts.ownCurrencySymbol ctx@
      -- since the ledger rules ensure that the minting policy will only
      -- be run if some of the asset is actually being minted: see
      -- https://playground.plutus.iohkdev.io/doc/plutus/tutorials/basic-minting-policies.html.
      _ -> False

-- | 'committeeHashPolicy' is the minting policy
{-# INLINEABLE committeeHashPolicy #-}
committeeHashPolicy :: InitCommitteeHashMint -> MintingPolicy
committeeHashPolicy gch =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||ScriptUtils.mkUntypedMintingPolicy . mkCommitteeHashPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode gch

-- | 'committeeHashCurSymbol' is the currency symbol
{-# INLINEABLE committeeHashCurSymbol #-}
committeeHashCurSymbol :: InitCommitteeHashMint -> CurrencySymbol
committeeHashCurSymbol ichm = scriptCurrencySymbol $ committeeHashPolicy ichm

{- | 'committeeHashCurSymbol' is the asset class. See 'initCommitteeHashMintTn'
 for details on the token name
-}
{-# INLINEABLE committeeHashAssetClass #-}
committeeHashAssetClass :: InitCommitteeHashMint -> AssetClass
committeeHashAssetClass ichm = Value.assetClass (committeeHashCurSymbol ichm) initCommitteeHashMintTn

-- CTL hack
mkCommitteHashPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCommitteHashPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkCommitteeHashPolicy . PlutusTx.unsafeFromBuiltinData

serialisableCommitteHashPolicy :: Ledger.Script
serialisableCommitteHashPolicy = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCommitteHashPolicyUntyped||])
