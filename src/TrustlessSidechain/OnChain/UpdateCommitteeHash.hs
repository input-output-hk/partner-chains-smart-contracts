{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.UpdateCommitteeHash where

-- import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (
  UpdateCommitteeHashRedeemer (committeePubKeys, newCommitteeHash, signature),
 )

import Ledger.Typed.Scripts (TypedValidator, Validator, ValidatorTypes)
import Ledger.Typed.Scripts qualified as Scripts

import Ledger.Address (Address)

import Plutus.V1.Ledger.Bytes qualified as Bytes
import Plutus.V1.Ledger.Crypto (PubKey)
import Plutus.V1.Ledger.Crypto qualified as Crypto
import Plutus.V1.Ledger.Value (
  AssetClass,
  Value,
 )
import Plutus.V1.Ledger.Value qualified as Value

import Plutus.V1.Ledger.Scripts (Datum (getDatum))

import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo,
  TxOut (txOutDatumHash, txOutValue),
 )
import Plutus.V1.Ledger.Contexts qualified as Contexts

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude qualified

import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude

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
 committee hash by concatenating all public keys together and appending them.
 We call the output of this function an /aggregate public key/.

 TODO: this is a very simple scheme...
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [PubKey] -> BuiltinByteString
aggregateKeys = Builtins.blake2b_256 . foldMap (Bytes.getLedgerBytes . Crypto.getPubKey)

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

{- | 'verifyMultiSignature' is a wrapper for how we verify multi signatures.
 For now, to simplify things we just test if any of the committee has signed
 the message.

 TODO: do a proper multisign later.
-}
{-# INLINEABLE verifyMultiSignature #-}
verifyMultiSignature ::
  [PubKey] -> BuiltinByteString -> BuiltinByteString -> Bool
verifyMultiSignature pubKeys msg sig = any f pubKeys
  where
    f pubKey =
      let pubKey' = Bytes.getLedgerBytes (Crypto.getPubKey pubKey)
       in verifySignature pubKey' msg sig

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

Note [Committee hash in output datum]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, the producer of a utxo is only required to include the datum hash,
and not the datum itself (but can optionally do so). In this case, we rely on
the fact that the producer actually does include the datum; and enforce this
with 'outputDatum'.
-}
{-# INLINEABLE mkUpdateCommitteeHashValidator #-}
mkUpdateCommitteeHashValidator ::
  UpdateCommitteeHash ->
  UpdateCommitteeHashDatum ->
  UpdateCommitteeHashRedeemer ->
  ScriptContext ->
  Bool
mkUpdateCommitteeHashValidator cmtHsh dat red ctx =
  traceIfFalse "Token missing from input" inputHasToken
    && traceIfFalse "Token missing from output" outputHasToken
    && traceIfFalse "Committee signature missing" signedByCurrentCommittee
    && traceIfFalse "Wrong committee" isCurrentCommittee
    && traceIfFalse
      "Wrong output datum"
      (outputDatum == UpdateCommitteeHashDatum (newCommitteeHash red))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput =
      txInInfoResolved $
        fromMaybe (traceError "Committee hash input missing") $
          Contexts.findOwnInput ctx

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

    inputHasToken :: Bool
    inputHasToken = hasNft (txOutValue ownInput)

    outputHasToken :: Bool
    outputHasToken = hasNft (txOutValue ownOutput)

    hasNft :: Value -> Bool
    hasNft val = Value.assetClassValueOf val (cToken cmtHsh) == 1

    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      verifyMultiSignature
        (committeePubKeys red)
        (newCommitteeHash red)
        (signature red)

    isCurrentCommittee :: Bool
    isCurrentCommittee = aggregateCheck (committeePubKeys red) $ committeeHash dat

data UpdatingCommitteeHash
instance ValidatorTypes UpdatingCommitteeHash where
  type DatumType UpdatingCommitteeHash = UpdateCommitteeHashDatum
  type RedeemerType UpdatingCommitteeHash = UpdateCommitteeHashRedeemer

-- | 'typedUpdateCommitteeHashValidator' is the typed validator of the script
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
updateCommitteeHashValidator :: UpdateCommitteeHash -> Validator
updateCommitteeHashValidator = Scripts.validatorScript . typedUpdateCommitteeHashValidator

-- | 'updateCommitteeHashAddress' is the address of the script
updateCommitteeHashAddress :: UpdateCommitteeHash -> Address
updateCommitteeHashAddress = Scripts.validatorAddress . typedUpdateCommitteeHashValidator
