{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.UpdateCommitteeHash where

import Cardano.Crypto.Wallet (XPrv)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Address (Address)
import Ledger.Contexts qualified as Contexts
import Ledger.Crypto qualified as Crypto
import Ledger.Typed.Scripts (MintingPolicy, TypedValidator, Validator, ValidatorTypes)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Bytes qualified as Bytes
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutDatumHash, txOutValue),
  TxOutRef,
 )
import Plutus.V1.Ledger.Crypto (PubKey)
import Plutus.V1.Ledger.Scripts (Datum (getDatum))
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (
  AssetClass,
  CurrencySymbol,
  TokenName (TokenName),
  Value (getValue),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude as PlutusTx
import TrustlessSidechain.MerkleTree qualified as MT
import TrustlessSidechain.OnChain.Types (
  UpdateCommitteeHashRedeemer (committeePubKeys, committeeSignatures, newCommitteeHash),
 )
import TrustlessSidechain.OnChain.Utils (verifyMultisig)
import Prelude qualified

-- * Updating the committee hash

-- | 'UpdateCommitteeHash' is used as the parameter for the contract.
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { -- | 'cToken' is the 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    cToken :: AssetClass
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''UpdateCommitteeHash

{- | 'aggregateKeys' aggregates a list of public keys into a single
 committee hash by essentially computing the merkle root of all public keys
 together.
 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [PubKey] -> BuiltinByteString
aggregateKeys [] = traceError "Empty committee"
aggregateKeys lst = MT.unRootHash $ MT.rootHash $ MT.fromList $ map (Bytes.getLedgerBytes . Crypto.getPubKey) lst

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

{- | Datum for the committee hash. This /committee hash/ is used to verify
 signatures for sidechain to mainchain transfers. This is a hash of
 concatenated public key hashes of the committee members

 TODO: this isn't actually used to verify signatures in the FUEL minting /
 burning policies (perhaps this will be used in a later iteration)
-}
newtype UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash :: BuiltinByteString
  }

instance Eq UpdateCommitteeHashDatum where
  {-# INLINEABLE (==) #-}
  UpdateCommitteeHashDatum cmtHsh == UpdateCommitteeHashDatum cmtHsh' =
    cmtHsh == cmtHsh'

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashDatum [('UpdateCommitteeHashDatum, 0)]

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
  -- TODO: remove the if statement here and just have the else clause. We do
  -- this for now because this is needed in the signed merkle root to emulate
  -- reference inputs... so we allow people to spend this output just to read
  -- the data here essentially
  -- BUT THIS SHOULD BE REMOVED WHEN WE HAVE REFERENCE INPUTS IN PLUTUSV2!
  if newCommitteeHash red == committeeHash dat
    then True
    else
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
        txOutDatumHash ownOutput
          >>= flip Contexts.findDatum info
          >>= PlutusTx.fromBuiltinData . getDatum

    outputHasToken :: Bool
    outputHasToken = hasNft (txOutValue ownOutput)

    hasNft :: Value -> Bool
    hasNft val = Value.assetClassValueOf val (cToken uch) == 1

    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      verifyMultisig
        (Bytes.getLedgerBytes . Crypto.getPubKey <$> committeePubKeys red)
        1 -- TODO: this should be the threshold?
        (newCommitteeHash red)
        (committeeSignatures red)
    isCurrentCommittee :: Bool
    isCurrentCommittee = aggregateCheck (committeePubKeys red) $ committeeHash dat

{- | 'UpdatingCommitteeHash' is the type to associate the 'DatumType' and
 'RedeemerType' to the acutal types used at run time.
-}
data UpdatingCommitteeHash

instance ValidatorTypes UpdatingCommitteeHash where
  type DatumType UpdatingCommitteeHash = UpdateCommitteeHashDatum
  type RedeemerType UpdatingCommitteeHash = UpdateCommitteeHashRedeemer

-- | 'typedUpdateCommitteeHashValidator' is the typed validator of the script
{-# INLINEABLE typedUpdateCommitteeHashValidator #-}
typedUpdateCommitteeHashValidator :: UpdateCommitteeHash -> TypedValidator UpdatingCommitteeHash
typedUpdateCommitteeHashValidator updateCommitteeHash =
  Scripts.mkTypedValidator @UpdatingCommitteeHash
    ( $$(PlutusTx.compile [||mkUpdateCommitteeHashValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode updateCommitteeHash
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @UpdateCommitteeHashDatum @UpdateCommitteeHashRedeemer

-- | 'updateCommitteeHashValidator' is the validator of the script
{-# INLINEABLE updateCommitteeHashValidator #-}
updateCommitteeHashValidator :: UpdateCommitteeHash -> Validator
updateCommitteeHashValidator = Scripts.validatorScript . typedUpdateCommitteeHashValidator

-- | 'updateCommitteeHashAddress' is the address of the script
{-# INLINEABLE updateCommitteeHashAddress #-}
updateCommitteeHashAddress :: UpdateCommitteeHash -> Address
updateCommitteeHashAddress = Scripts.validatorAddress . typedUpdateCommitteeHashValidator

-- * Initializing the committee hash

-- | 'InitCommitteeHashMint' is used as the parameter for the minting policy
newtype InitCommitteeHashMint = InitCommitteeHashMint
  { -- | 'TxOutRef' is the output reference to mint the NFT initially.
    icTxOutRef :: TxOutRef
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
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

{- | 'initCommitteeHashMintAmount' is the amount of the currency to mint which
 is 1.
-}
{-# INLINEABLE initCommitteeHashMintAmount #-}
initCommitteeHashMintAmount :: Integer
initCommitteeHashMintAmount = 1

{- | 'mkCommitteeHashPolicy' is the minting policy for the NFT which identifies
 the committee hash.
-}
{-# INLINEABLE mkCommitteeHashPolicy #-}
mkCommitteeHashPolicy :: InitCommitteeHashMint -> () -> ScriptContext -> Bool
mkCommitteeHashPolicy ichm _red ctx =
  traceIfFalse "error 'mkCommitteeHashPolicy' UTxO not consumed" hasUtxo
    && traceIfFalse "error 'mkCommitteeHashPolicy' wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oref :: TxOutRef
    oref = icTxOutRef ichm

    hasUtxo :: Bool
    hasUtxo = any ((oref ==) . txInInfoOutRef) $ txInfoInputs info

    -- assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount = case fmap AssocMap.toList $ AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue $ txInfoMint info of
      Just [(tn', amt)] -> tn' == initCommitteeHashMintTn && amt == initCommitteeHashMintAmount
      _ -> False

-- | 'committeeHashPolicy' is the minting policy
{-# INLINEABLE committeeHashPolicy #-}
committeeHashPolicy :: InitCommitteeHashMint -> MintingPolicy
committeeHashPolicy gch =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkCommitteeHashPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode gch

-- | 'committeeHashCurSymbol' is the currency symbol
{-# INLINEABLE committeeHashCurSymbol #-}
committeeHashCurSymbol :: InitCommitteeHashMint -> CurrencySymbol
committeeHashCurSymbol ichm = Contexts.scriptCurrencySymbol $ committeeHashPolicy ichm

{- | 'committeeHashAssetClass' is the asset class. See 'initCommitteeHashMintTn'
 for details on the token name
-}
{-# INLINEABLE committeeHashAssetClass #-}
committeeHashAssetClass :: InitCommitteeHashMint -> AssetClass
committeeHashAssetClass ichm = Value.assetClass (committeeHashCurSymbol ichm) initCommitteeHashMintTn
