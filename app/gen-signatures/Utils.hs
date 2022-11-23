{- | 'Utils' includes utility functions for tasks like:

      -  signing messages

      -  showing signatures

      -  converting public keys to private keys
-}
module Utils where

import Prelude

import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Crypto.Secp256k1 qualified as SECP
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b_256)
import Data.Maybe (fromMaybe)
import Ledger (PubKey (PubKey), Signature (Signature))
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
  BuiltinByteString,
  LedgerBytes (LedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.MerkleTree (MerkleProof, MerkleTree, RootHash (RootHash))
import TrustlessSidechain.OffChain.Types (
  SidechainPubKey (SidechainPubKey),
 )
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistrationMsg,
 )

-- * Signing a message

-- | Sign a message with an Ed25519DSIGN key
signWithSPOKey ::
  SignKeyDSIGN Ed25519DSIGN ->
  BlockProducerRegistrationMsg ->
  Crypto.Signature
signWithSPOKey skey msg =
  let serialised = Builtins.fromBuiltin $ Builtins.serialiseData $ toBuiltinData msg
   in Crypto.Signature
        . Builtins.toBuiltin
        . rawSerialiseSigDSIGN
        $ signDSIGN () serialised skey

{- | Given a @message@, converts the @message@ to the 'BuiltinData'
 representation, seralises to cbor, then takes the @blake2b_256@ hash.
 Finally, this signs the hash with the given SECP256K1 key
-}
signWithSidechainKey ::
  ToData a =>
  SECP.SecKey ->
  a ->
  Crypto.Signature
signWithSidechainKey skey msg =
  let serialised = Builtins.serialiseData $ toBuiltinData msg
      hashedMsg = blake2b_256 $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg
   in Crypto.Signature
        . Builtins.toBuiltin
        . SECP.getCompactSig
        . SECP.exportCompactSig
        $ SECP.signMsg skey ecdsaMsg

-- * Show functions

-- | Serialise transaction output reference into CLI format (TX_ID#TX_IDX)
showTxOutRef :: TxOutRef -> String
showTxOutRef (TxOutRef (TxId txId) txIdx) =
  showBuiltinBS txId ++ "#" ++ show txIdx

-- | Serialise a ByteString into hex string
showBS :: ByteString -> String
showBS =
  Char8.unpack . Base16.encode

-- | Serialise a BuiltinByteString into hex string
showBuiltinBS :: BuiltinByteString -> String
showBuiltinBS = showBS . Builtins.fromBuiltin

-- | Serialise a RootHash into hex string
showRootHash :: RootHash -> String
showRootHash (RootHash bs) = showBuiltinBS bs

-- | Serialise public key
showPubKey :: PubKey -> String
showPubKey (PubKey (LedgerBytes pk)) = showBuiltinBS pk

-- | Serialise sidechain public key
showScPubKey :: SidechainPubKey -> String
showScPubKey (SidechainPubKey pk) = showBuiltinBS pk

{- | Serialise a sidechain public key and signature into
 > PUBKEY:SIG
-}
showScPubKeyAndSig :: SidechainPubKey -> Signature -> String
showScPubKeyAndSig sckey sig = concat [showScPubKey sckey, ":", showSig sig]

-- | Serialise signature
showSig :: Signature -> String
showSig (Signature sig) = showBuiltinBS sig

{- | 'showThreshold' shows integers @n@ and @m@ as
 > n/m
 Importantly, this is compatible with the purescript parser format
-}
showThreshold :: Integer -> Integer -> String
showThreshold n m = show n ++ "/" ++ show m

{- | 'showMerkleTree' seralises a merkle tree to the hex encoded cbor builtin
 data representation
-}
showMerkleTree :: MerkleTree -> String
showMerkleTree = showBuiltinBS . Builtins.serialiseData . toBuiltinData

{- | 'showMerkleProof' seralises a merkle tree proof to the hex encoded cbor builtin
 data representation
-}
showMerkleProof :: MerkleProof -> String
showMerkleProof = showBuiltinBS . Builtins.serialiseData . toBuiltinData

-- * Covnerting converting private keys to public keys

-- | Derive Ed25519DSIGN public key from the private key
toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
toSpoPubKey =
  Crypto.PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN
    . deriveVerKeyDSIGN

-- | Derive SECP256K1 public key from the private key
toSidechainPubKey :: SECP.SecKey -> SidechainPubKey
toSidechainPubKey =
  secpPubKeyToSidechainPubKey
    . SECP.derivePubKey

-- | Converts a 'SECP.PubKey' to a 'SidechainPubKey'
secpPubKeyToSidechainPubKey :: SECP.PubKey -> SidechainPubKey
secpPubKeyToSidechainPubKey = SidechainPubKey . Builtins.toBuiltin . SECP.exportPubKey True
