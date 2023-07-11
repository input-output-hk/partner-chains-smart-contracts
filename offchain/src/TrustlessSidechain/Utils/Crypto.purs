module TrustlessSidechain.Utils.Crypto
  ( SidechainMessage(SidechainMessage)
  , sidechainMessage
  , byteArrayToSidechainMessageUnsafe
  , sidechainMessageToTokenName
  , SidechainPrivateKey
  , byteArrayToSidechainPublicKeyUnsafe
  , SidechainPublicKey(SidechainPublicKey)
  , SidechainSignature(SidechainSignature)
  , toPubKeyUnsafe
  , generatePrivKey
  , generateRandomPrivateKey
  , multiSign
  , sign
  , verifyEcdsaSecp256k1Signature
  , sidechainPublicKey
  , normalizeCommitteePubKeysAndSignatures
  , unzipCommitteePubKeysAndSignatures
  , verifyMultiSignature
  , getSidechainPublicKeyByteArray
  , getSidechainPrivateKeyByteArray
  , getSidechainSignatureByteArray
  , byteArrayToSidechainPrivateKeyUnsafe
  , sidechainPrivateKey
  , getSidechainMessageByteArray
  , byteArrayToSidechainSignatureUnsafe
  , sidechainSignature
  , aggregateKeys
  , countEnoughSignatures
  , takeExactlyEnoughSignatures
  ) where

import Contract.Prelude

import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Value (TokenName)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (on)
import Data.Maybe as Maybe
import Data.Ord as Ord
import Partial.Unsafe as Unsafe

-- | Invariant: ∀ x : SidechainPublicKey. length x = 33
-- | Format: Compressed & Serialized as per secp256k1 implementation
-- | make sure to check the leading byte for valid key format
newtype SidechainPublicKey = SidechainPublicKey ByteArray

derive newtype instance ordSidechainPublicKey ∷ Ord SidechainPublicKey

derive newtype instance eqSidechainPublicKey ∷ Eq SidechainPublicKey

derive newtype instance toDataSidechainPublicKey ∷ ToData SidechainPublicKey

derive newtype instance fromDataSidechainPublicKey ∷ FromData SidechainPublicKey

derive instance Newtype SidechainPublicKey _

instance Show SidechainPublicKey where
  show (SidechainPublicKey byteArray) = "(byteArrayToSidechainPublicKeyUnsafe "
    <> show byteArray
    <> ")"

-- | Smart constructor for `SidechainPublicKey` to ensure it is a valid
-- | compressed (33 bytes) secp256k1 public key.
sidechainPublicKey ∷ ByteArray → Maybe SidechainPublicKey
sidechainPublicKey bs
  | ByteArray.byteLength bs == 33
      && pubKeyVerify bs = Just $ SidechainPublicKey bs
  | otherwise = Nothing

-- | `getSidechainPublicKeyByteArray` grabs the underlying `ByteArray` of the
-- | `SidechainPublicKey`
getSidechainPublicKeyByteArray ∷ SidechainPublicKey → ByteArray
getSidechainPublicKeyByteArray (SidechainPublicKey byteArray) = byteArray

-- | `byteArrayToSidechainPublicKeyUnsafe` constructs a sidechain public key without
-- | verifying any of the invariants.
byteArrayToSidechainPublicKeyUnsafe ∷ ByteArray → SidechainPublicKey
byteArrayToSidechainPublicKeyUnsafe = SidechainPublicKey

-- | Invariant: ∀ x : SidechainPrivateKey. length x = 32, and is non zero and
-- | less then the secp256k1 curve order. See 1. for details.
-- | Format: raw bytes
-- |
-- | References.
-- |    1. https://github.com/bitcoin-core/secp256k1/blob/e3f84777eba58ea010e61e02b0d3a65787bc4fd7/include/secp256k1.h#L662-L673
newtype SidechainPrivateKey = SidechainPrivateKey ByteArray

derive newtype instance ordSidechainPrivateKey ∷ Ord SidechainPrivateKey

derive newtype instance eqSidechainPrivateKey ∷ Eq SidechainPrivateKey

derive newtype instance toDataSidechainPrivateKey ∷ ToData SidechainPrivateKey

derive newtype instance fromDataSidechainPrivateKey ∷
  FromData SidechainPrivateKey

instance Show SidechainPrivateKey where
  show (SidechainPrivateKey byteArray) = "(byteArrayToSidechainPrivateKeyUnsafe "
    <> show byteArray
    <> ")"

-- | `sidechainPrivateKey` is a smart constructor for `SidechainPrivateKey` to
-- | check the required invariants.
sidechainPrivateKey ∷ ByteArray → Maybe SidechainPrivateKey
sidechainPrivateKey byteArray
  | ByteArray.byteLength byteArray == 32
      && secKeyVerify byteArray = Just $ SidechainPrivateKey byteArray
  | otherwise = Nothing

-- | `byteArrayToSidechainPrivateKeyUnsafe` constructs a sidechain public key without
-- | verifying any of the invariants.
byteArrayToSidechainPrivateKeyUnsafe ∷ ByteArray → SidechainPrivateKey
byteArrayToSidechainPrivateKeyUnsafe = SidechainPrivateKey

-- | `getSidechainPrivateKeyByteArray` grabs the underlying `ByteArray` of the
-- | `SidechainPrivateKey`
getSidechainPrivateKeyByteArray ∷ SidechainPrivateKey → ByteArray
getSidechainPrivateKeyByteArray (SidechainPrivateKey byteArray) = byteArray

-- | Invariant: ∀ x : SidechainMessage. length x = 32
-- | Format: raw bytes
newtype SidechainMessage = SidechainMessage ByteArray

derive newtype instance ordSidechainMessage ∷ Ord SidechainMessage

derive newtype instance eqSidechainMessage ∷ Eq SidechainMessage

derive newtype instance toDataSidechainMessage ∷ ToData SidechainMessage

derive newtype instance fromDataSidechainMessage ∷ FromData SidechainMessage

instance Show SidechainMessage where
  show (SidechainMessage byteArray) = "(byteArrayToSidechainMessageUnsafe "
    <> show byteArray
    <> ")"

-- | `sidechainMessage` is a smart constructor for `SidechainMessage` which verifies the
-- | invariants
sidechainMessage ∷ ByteArray → Maybe SidechainMessage
sidechainMessage byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ SidechainMessage byteArray
  | otherwise = Nothing

-- | `byteArrayToSidechainMessageUnsafe` constructs a `SidechainMessage`
-- | without verifying any of the invariants
byteArrayToSidechainMessageUnsafe ∷ ByteArray → SidechainMessage
byteArrayToSidechainMessageUnsafe = SidechainMessage

-- | `sidechainMessageToTokenName` converts a sidechain message to a token name
sidechainMessageToTokenName ∷ SidechainMessage → TokenName
sidechainMessageToTokenName (SidechainMessage byteArray) =
  -- should be safe as they have the same length requirements
  -- i.e., token names should be less than or equal to 32 bytes long
  -- See:
  -- https://github.com/Plutonomicon/cardano-transaction-lib/blob/fde2e42b2e57ea978b3517913a1917ebf8836ab6/src/Internal/Types/TokenName.purs#L104-L109
  Unsafe.unsafePartial $ Maybe.fromJust $ Value.mkTokenName byteArray

-- | `getSidechainMessageByteArray` grabs the underlying `ByteArray` of the
-- | `SidechainMessage`
getSidechainMessageByteArray ∷ SidechainMessage → ByteArray
getSidechainMessageByteArray (SidechainMessage byteArray) = byteArray

-- | Invariant: ∀ x : SidechainSignature. length x = 64
newtype SidechainSignature = SidechainSignature ByteArray

derive newtype instance ordSidechainSignature ∷ Ord SidechainSignature

derive newtype instance eqSidechainSignature ∷ Eq SidechainSignature

derive newtype instance toDataSidechainSignature ∷ ToData SidechainSignature

derive newtype instance fromDataSidechainSignature ∷ FromData SidechainSignature

derive instance Newtype SidechainSignature _

instance Show SidechainSignature where
  show (SidechainSignature byteArray) = "(byteArrayToSidechainSignatureUnsafe "
    <> show byteArray
    <> ")"

-- | `sidechainSignature` is a smart constructor for `SidechainSignature` to
-- | verify the invariants.
sidechainSignature ∷ ByteArray → Maybe SidechainSignature
sidechainSignature byteArray
  | ByteArray.byteLength byteArray == 64 = Just $ SidechainSignature byteArray
  | otherwise = Nothing

-- | `getSidechainSignatureArray` grabs the underlying `ByteArray` of the
-- | `SidechainSignature`
getSidechainSignatureByteArray ∷ SidechainSignature → ByteArray
getSidechainSignatureByteArray (SidechainSignature byteArray) = byteArray

-- | `byteArrayToSidechainSignatureUnsafe` constructs a sidechain public key without
-- | verifying any of the invariants.
byteArrayToSidechainSignatureUnsafe ∷ ByteArray → SidechainSignature
byteArrayToSidechainSignatureUnsafe = SidechainSignature

-- TODO: newtype checks the type aliases above

foreign import generateRandomPrivateKey ∷ Effect SidechainPrivateKey

foreign import toPubKeyUnsafe ∷ SidechainPrivateKey → SidechainPublicKey

foreign import pubKeyVerify ∷ ByteArray → Boolean

foreign import secKeyVerify ∷ ByteArray → Boolean

foreign import sign ∷
  SidechainMessage → SidechainPrivateKey → SidechainSignature

foreign import verifyEcdsaSecp256k1Signature ∷
  SidechainPublicKey → SidechainMessage → SidechainSignature → Boolean

generatePrivKey ∷ Contract SidechainPrivateKey
generatePrivKey =
  liftEffect generateRandomPrivateKey

multiSign ∷
  Array SidechainPrivateKey → SidechainMessage → Array SidechainSignature
multiSign xkeys msg = map (sign msg) xkeys

-- | `normalizeCommitteePubKeysAndSignatures` takes a list of public keys and their
-- | associated signatures and sorts by the natural lexicographical ordering of the
-- | `SidechainPublicKey`s
-- |
-- | Previously, the onchain multisign method required that the public keys are
-- | sorted (to verify uniqueness of public keys), but this requirement was
-- | relaxed and hence sorting is technically no longer necessary....
-- |
-- | But, as per the specification, the committee hash will be created from
-- | lexicographically sorted public keys, so sorting the public keys will
-- | ensure that it matches the same onchain committee format.
normalizeCommitteePubKeysAndSignatures ∷
  ∀ a b.
  Ord a ⇒
  Array (a /\ Maybe b) →
  Array (a /\ Maybe b)
normalizeCommitteePubKeysAndSignatures = Array.sortBy (Ord.compare `on` fst)

-- | `unzipCommitteePubKeysAndSignatures` unzips public keys and associated
-- | signatures, and removes all the `Nothing` signatures.
-- |
-- | Preconditions to be compatible with the onchain Haskell multisign method:
-- |    - The input array should be sorted lexicographically by
-- |    `SidechainPublicKey` by `normalizeCommitteePubKeysAndSignatures`
unzipCommitteePubKeysAndSignatures ∷
  ∀ a b.
  Array (a /\ Maybe b) →
  Tuple (Array a) (Array b)
unzipCommitteePubKeysAndSignatures = map Array.catMaybes <<< Array.unzip

-- | `countEnoughSignatures` counts the minimum number of signatures needed for
-- | the onchain code to verify successfully.
countEnoughSignatures ∷
  ∀ a.
  -- numerator
  BigInt →
  -- denominator (ensure this is non-zero)
  BigInt →
  Array a →
  BigInt
countEnoughSignatures numerator denominator arr =
  let
    len = BigInt.fromInt $ Array.length arr
  in
    one + ((numerator * len) / denominator)

-- | `takeExactlyEnoughSignatures` takes exactly enough signatures (if it
-- | or less than if it cannot) for committee certificate verification as a
-- | minor optimization so that we only provide the onchain code with the
-- | minimum amount of
-- | signatures needed.
takeExactlyEnoughSignatures ∷
  ∀ a b.
  -- numerator
  BigInt →
  -- denominator (ensure this is non-zero)
  BigInt →
  Array a /\ Array b →
  Array a /\ Array b
takeExactlyEnoughSignatures numerator denominator (pks /\ sigs) =
  pks /\
    Array.take
      -- It should be big enough to fit in a plain old int as this
      -- corresponds to the array length (size of int is log of array
      -- length)
      -- TODO
      ( Unsafe.unsafePartial $ Maybe.fromJust $ BigInt.toInt
          (countEnoughSignatures numerator denominator pks)
      )
      sigs

-- | `verifyMultiSignature thresholdNumerator thresholdDenominator pubKeys msg signatures`
-- | returns true iff
-- |
-- |      - `pubKeys` is sorted lexicographically and are distinct
-- |
-- |      - `signatures` are the corresponding signatures `pubKeys` of `msg`
-- |      as a subsequence of `pubKeys` (i.e., ordered the same way as `pubKeys`).
-- |
-- |      - strictly more than `thresholdNumerator/thresholdDenominator`
-- |      `pubKeys` have signed `msg`
-- |
-- | Note: this loosely replicates the behavior of the corresponding on chain
-- | function, but should be significantly more efficient (since we use the
-- | assumption that the signatures are essentially a subsequence of the public
-- | keys); and is generalized to allow arbitrary thresholds to be given.
verifyMultiSignature ∷
  ∀ pubKey msg signature.
  Ord pubKey ⇒
  (pubKey → msg → signature → Boolean) →
  BigInt →
  BigInt →
  Array pubKey →
  msg →
  Array signature →
  Boolean
verifyMultiSignature
  verifySig
  thresholdNumerator
  thresholdDenominator
  pubKeys
  msg
  signatures =
  let
    go ∷ BigInt → Array pubKey → Array signature → Boolean
    go signed pubs sigs =
      let
        ok = signed >
          ( BigInt.quot
              (thresholdNumerator * BigInt.fromInt (Array.length pubKeys))
              thresholdDenominator
          )
      in
        case Array.uncons pubs of
          Nothing → ok
          Just { head: pub, tail: pubs' } →
            case Array.uncons sigs of
              Nothing → ok
              Just { head: sig, tail: sigs' } →
                if verifySig pub msg sig then
                  -- the public key and signature match, so
                  -- we move them both forward..
                  go (signed + one) pubs' sigs'

                else
                  -- otherwise, they don't match so since
                  -- `sigs` is essentially a subsequence of
                  -- `pubs`, we move only `pubs` forward
                  -- since a later key should match with
                  -- `sig`.
                  go signed pubs' sigs
  in
    isSorted pubKeys && go zero pubKeys signatures

{- | Verifies that the non empty array is sorted -}
isSorted ∷ ∀ (a ∷ Type). Ord a ⇒ Array a → Boolean
isSorted xss = case Array.tail xss of
  Just xs → and (Array.zipWith (<=) xss xs)
  Nothing → false

-- | `aggregateKeys` aggregates a list of keys s.t. the resulting `ByteArray`
-- | may be stored in the `UpdateCommitteeDatum` in an onchain compatible way.
-- | Note: this sorts the input array
aggregateKeys ∷ Array ByteArray → ByteArray
aggregateKeys =
  Hashing.blake2b256Hash
    <<< mconcat
    <<< Array.sort
