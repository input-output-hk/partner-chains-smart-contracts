{-# LANGUAGE TemplateHaskell #-}

-- | "TrustlessSidechain.PoCSchnorr" includes provides a minting policy for a
-- proof of concept test that isolates a test to integrate offchain code with
-- the onchain 'verifySchnorrSecp256k1Signature' function.
--
-- _Notes on @'verifySchnorrSecp256k1Signature' publicKey message signature@_.
--
-- It all eventually boils down to the secp256k1 C library using functions in the following headers:
--
--      - [1]:
--      <https://github.com/bitcoin-core/secp256k1/blob/master/include/secp256k1_extrakeys.h>:
--      is for public key serialization / deserialization.
--
--      - [2]:
--      <https://github.com/bitcoin-core/secp256k1/blob/master/include/secp256k1_schnorrsig.h>
--      is for schnorr signatures
--
-- There's a nice specification written up in the following link -- interested
-- readers may find (some) details there.
--
--      - [3]: <https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki>
--
-- Question 0: Short overview of how the Schnorr signature works -- this skips
-- _many many_ details, so for full details in [3].
--  - Notation:
--      - @G@ is the base point in the elliptic curve (a particular element in
--      the group)
--
--      - @H@ is a hash function
--
--  - Keys are generated as follows
--  ```
--      secretKey <- random integer mod the curve order
--      publicKey <- secretKey * G
--      return (secretKey , publicKey)
--  ```
--  - Signature generation for signing a byte array @m@ with secret key
--  @secretKey@
--  ```
--      k' <- random nonzero integer mod the curve order
--      R <- k' * G
--      s <- k' + H(R || publicKey || m) * secretKey
--      return (R, s) as the signature
--  ```
--  - Verification for signature @(R,s)@ and public key @publicKey@ is as
--  follows.
--  ```
--      Test if
--          R == s * G - H(R || publicKey || m ) * publicKey
--  ```
--  for which correctness (i.e., valid signatures accept) follows since
--  ```
--      s * G - H(R || publicKey || m ) * publicKey
--          = (k' + H(R || publicKey || m) * secretKey)  * G        [defn. of s]
--              - H(R || publicKey || m ) * publicKey
--          = (k' + H(R || publicKey || m) * secretKey)  * G
--              - H(R || publicKey || m ) * secretKey * G           [defn. of publicKey]
--          = k' * G                                                [distribute / do the addition]
--          = R                                                     [defn. of R]
--  ```
--
-- Question 1: What is the format of the @publicKey@?
--  - The Haskell documentation says it should be 64 bytes... but I don't think
--  that's quite right...
--
--  Reading the implementation of 'verifySchnorrSecp256k1Signature', it boils
--  down to call
--  <https://github.com/bitcoin-core/secp256k1/blob/332af315fcbe73f8f9dd64b4e87cfaa83d87d5b4/include/secp256k1_extrakeys.h#L37-L51>
--  which says it should be 32 bytes contradicting the Haskell documentation
--  claiming it should be 64 bytes?
--  Hm, that's strange.
--
--  Is this a typo? I believe it is a typo in the Haskell documentation... Even
--  reading the implementation, it actually verifies that the public key is 32
--  bytes long [3]. Later Haskell versions correct this typo.
--
--  - So, we conclude:
--      - Public keys are 32 bytes long (contrary to the Haskell
--      documentation); and are _only_ the X-coordinate of the elliptic curve
--      point (contrast this to either: storing both X,Y coordinates (64
--      bytes), or the X-coordinate + 1 bit (33 bytes altogether) to determine
--      the Y coordinate)
--
--      Moreover, recalling that every X-coordinate has two possible Y
--      coordinates, the design decision was to implicitly always choose the
--      even Y coordinate (one is always even, and the other is always odd) as
--      the Y-coordinate of the given X-coordinate.
--
--      Hence, these X-only coordinates are equivalent to the compressed format
--      which is _always_ prefixed by the byte @0x02@
--
--      See [3].
--
-- Question 2: What is the format of the @signature@?
--      - Without getting too "gritty" in the details of exactly _how_ the
--      Schnorr signature is generated, the signature must be 64 bytes long for
--      which
--
--          - the first 32 bytes are the X-coordinate of a random elliptic
--          curve point (via exponentiation of the base point)
--
--          - the next 32 bytes are an integer
--
--      See [3].
--
-- Question 3: What is the format of the @message@?
--      - unlike ECDSA, message can be an arbitrary length byte array -- see
--      [3].
module TrustlessSidechain.PoCSchnorr (
  SchnorrRedeemer (..),
  serialisablePolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (ScriptContext)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedMintingPolicy,
 )

data SchnorrRedeemer = SchnorrRedeemer
  { -- | arbitrary byte array
    message :: BuiltinByteString
  , -- | 64 bytes
    signature :: BuiltinByteString
  , -- | 32 bytes
    publicKey :: BuiltinByteString
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Show)

PlutusTx.makeIsDataIndexed ''SchnorrRedeemer [('SchnorrRedeemer, 0)]

-- | a simple minting policy which mints only if the data provided in the
-- redeemer is a valid schnorr signature.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: SchnorrRedeemer -> ScriptContext -> Bool
mkPolicy redeemer _context =
  traceIfFalse "schnorr signature check failed"
    . verifySchnorrSecp256k1Signature (publicKey redeemer) (message redeemer)
    $ signature redeemer

untypedPolicy :: BuiltinData -> BuiltinData -> ()
untypedPolicy = mkUntypedMintingPolicy mkPolicy

serialisablePolicy :: SerialisedScript
serialisablePolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||untypedPolicy||])
