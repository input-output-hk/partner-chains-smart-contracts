{- | 'Utils' includes utility functions for tasks like:

      -  signing messages

      -  showing signatures

      -  converting public keys to private keys
-}
module Utils (
  signWithSPOKey,
  signWithSidechainKey,
  showTxOutRef,
  showBS,
  showBuiltinBS,
  showRootHash,
  showPubKey,
  showScPubKey,
  showScPubKeyAndSig,
  showSig,
  showThreshold,
  showMerkleTree,
  showMerkleProof,
  toSpoPubKey,
  toSidechainPubKey,
  secpPubKeyToSidechainPubKey,
  generateRandomSidechainPrivateKey,
) where

import Prelude

import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Crypto.Random qualified as Random
import Crypto.Secp256k1 qualified as SECP
import Crypto.Secp256k1.Internal qualified as SECP.Internal
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
import TrustlessSidechain.MerkleTree (MerkleProof, MerkleTree, RootHash)
import TrustlessSidechain.OffChain.Types (
  SidechainPubKey (SidechainPubKey),
 )
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistrationMsg,
 )

-- * Generating a private sidechain key

{- | Generates a random sidechain private key.

 The implementation is a translation of the underlying example in the C
 library: https://github.com/bitcoin-core/secp256k1/blob/master/examples/ecdsa.c
 where we use the module 'Crypto.Random' from cryptonite to handle the cross
 platform random bytestring generation

 Surprisingly, the Haskell library we're using doesn't include a nice wrapper
 for this, so we do it ourselves I guess.

 Some implementation notes:

      - We use the default context 'Crypto.Secp256k1.Internal.ctx' which is
      thread safe and used internally for precomputed values (and adding
      randomization to prevent side channel attacks)

      - As given in the C example, we loop forever until we find a "valid"
      key i.e., a non-zero key smaller than the group's order (see
      [here](https://github.com/bitcoin-core/secp256k1/blob/44c2452fd387f7ca604ab42d73746e7d3a44d8a2/include/secp256k1.h#L608)...
      Ostensibly, the probability of a secret key being "invalid" is
      neglible, so we _expect_ this loop to run at most once.

 TODO: might be a good idea to put this in a newtype wrapper...
-}
generateRandomSidechainPrivateKey :: IO BuiltinByteString
generateRandomSidechainPrivateKey =
  let go = do
        bs <- Random.getRandomBytes 32
        ret <- SECP.Internal.useByteString bs $ \(ptr, _len) ->
          SECP.Internal.ecSecKeyVerify SECP.Internal.ctx ptr
        -- Returns 1 in the case that this is valid, see the FFI
        -- call
        -- [here](https://github.com/bitcoin-core/secp256k1/blob/44c2452fd387f7ca604ab42d73746e7d3a44d8a2/include/secp256k1.h#L608)
        if ret == 1
          then return bs
          else go
   in fmap Builtins.toBuiltin go

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
showRootHash = showBuiltinBS . Builtins.serialiseData . toBuiltinData

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
