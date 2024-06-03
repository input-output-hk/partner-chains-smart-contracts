module TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1Message(EcdsaSecp256k1Message)
  , ecdsaSecp256k1Message
  , byteArrayToEcdsaSecp256k1MessageUnsafe
  , ecdsaSecp256k1MessageToAssetName
  , EcdsaSecp256k1PrivateKey
  , byteArrayToEcdsaSecp256k1PubKeyUnsafe
  , EcdsaSecp256k1PubKey(EcdsaSecp256k1PubKey)
  , EcdsaSecp256k1Signature(EcdsaSecp256k1Signature)
  , toPubKeyUnsafe
  , generatePrivKey
  , generateRandomPrivateKey
  , multiSign
  , sign
  , verifyEcdsaSecp256k1Signature
  , ecdsaSecp256k1PubKey
  , normalizeCommitteePubKeysAndSignatures
  , unzipCommitteePubKeysAndSignatures
  , verifyMultiSignature
  , getEcdsaSecp256k1PubKeyByteArray
  , getEcdsaSecp256k1PrivateKeyByteArray
  , getEcdsaSecp256k1SignatureByteArray
  , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe
  , ecdsaSecp256k1PrivateKey
  , getEcdsaSecp256k1MessageByteArray
  , byteArrayToEcdsaSecp256k1SignatureUnsafe
  , ecdsaSecp256k1Signature
  , aggregateKeys
  , countEnoughSignatures
  , takeExactlyEnoughSignatures
  , serialiseEcdsaSecp256k1PubKey
  , serialiseEcdsaSecp256k1PrivateKey
  , serialiseEcdsaSecp256k1SignatureToDer
  , serialiseEcdsaSecp256k1Signature
  , blake2b256Hash
  ) where

import Partial.Unsafe (unsafePartial)
import Contract.Prelude
import Contract.PlutusData (class FromData, class ToData, fromData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Value (AssetName)
import Cardano.Types.AssetName (mkAssetName)
import Data.Array as Array
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Data.BigInt as RegularBigInt
import Data.ByteArray (hexToByteArrayUnsafe, byteArrayToHex)
import Data.Function (on)
import Data.Maybe as Maybe
import Data.Ord as Ord
import Partial.Unsafe as Unsafe

-- | Invariant: length of the pubkey must be 33 bytes.
-- | Format: Compressed and serialized as per ECDSA signatures for SECP256k1.
-- | Check the leading byte for valid key format.
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey ByteArray

derive newtype instance Eq EcdsaSecp256k1PubKey

derive newtype instance Ord EcdsaSecp256k1PubKey

derive newtype instance ToData EcdsaSecp256k1PubKey

instance FromData EcdsaSecp256k1PubKey where
  fromData = fromData >=> ecdsaSecp256k1PubKey

derive instance Newtype EcdsaSecp256k1PubKey _

instance Show EcdsaSecp256k1PubKey where
  show (EcdsaSecp256k1PubKey byteArray) =
    "(byteArrayToEcdsaSecp256k1PubKeyUnsafe "
      <> show byteArray
      <> ")"

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1PubKey ∷ EcdsaSecp256k1PubKey → String
serialiseEcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey pubKey) =
  ByteArray.byteArrayToHex pubKey

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1PrivateKey ∷ EcdsaSecp256k1PrivateKey → String
serialiseEcdsaSecp256k1PrivateKey (EcdsaSecp256k1PrivateKey privKey) =
  ByteArray.byteArrayToHex privKey

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1Signature ∷ EcdsaSecp256k1Signature → String
serialiseEcdsaSecp256k1Signature (EcdsaSecp256k1Signature privKey) =
  ByteArray.byteArrayToHex privKey

-- | Smart constructor for `EcdsaSecp256k1PubKey` to ensure it is a valid
-- | compressed (33 bytes) secp256k1 public key.
ecdsaSecp256k1PubKey ∷ ByteArray → Maybe EcdsaSecp256k1PubKey
ecdsaSecp256k1PubKey bs
  | ByteArray.byteLength bs == 33
      && pubKeyVerify bs = Just $ EcdsaSecp256k1PubKey bs
  | otherwise = Nothing

-- | Get the underlying `ByteArray` of the `EcdsaSecp256k1PubKey`.
getEcdsaSecp256k1PubKeyByteArray ∷ EcdsaSecp256k1PubKey → ByteArray
getEcdsaSecp256k1PubKeyByteArray (EcdsaSecp256k1PubKey byteArray) = byteArray

-- | Construct a `EcdsaSecp256k1PubKey` without verifying invariants. Use with
-- | extreme care.
byteArrayToEcdsaSecp256k1PubKeyUnsafe ∷ ByteArray → EcdsaSecp256k1PubKey
byteArrayToEcdsaSecp256k1PubKeyUnsafe = EcdsaSecp256k1PubKey

-- | Invariant 1: length of the privkey must be 32 bytes
-- | Invariant 2: privkey must be nonzero
-- | Invariant 3: privkey must be less than the secp256k1 curve order (see
-- | reference for details)
-- |
-- | Reference: https://github.com/bitcoin-core/secp256k1/blob/e3f84777eba58ea010e61e02b0d3a65787bc4fd7/include/secp256k1.h#L662-L673
newtype EcdsaSecp256k1PrivateKey = EcdsaSecp256k1PrivateKey ByteArray

derive newtype instance Ord EcdsaSecp256k1PrivateKey

derive newtype instance Eq EcdsaSecp256k1PrivateKey

derive newtype instance ToData EcdsaSecp256k1PrivateKey

instance FromData EcdsaSecp256k1PrivateKey where
  fromData = fromData >=> ecdsaSecp256k1PrivateKey

instance Show EcdsaSecp256k1PrivateKey where
  show (EcdsaSecp256k1PrivateKey byteArray) =
    "(byteArrayToEcdsaSecp256k1PrivateKeyUnsafe "
      <> show byteArray
      <> ")"

-- | Smart constructor for `EcdsaSecp256k1PrivateKey` which checks its
-- | invariants.
ecdsaSecp256k1PrivateKey ∷ ByteArray → Maybe EcdsaSecp256k1PrivateKey
ecdsaSecp256k1PrivateKey byteArray
  | ByteArray.byteLength byteArray == 32
      && secKeyVerify byteArray = Just $ EcdsaSecp256k1PrivateKey byteArray
  | otherwise = Nothing

-- | Construct an `EcdsaSecp256k1PrivateKey` without checking its invariants.
-- | Use with extreme care.
byteArrayToEcdsaSecp256k1PrivateKeyUnsafe ∷ ByteArray → EcdsaSecp256k1PrivateKey
byteArrayToEcdsaSecp256k1PrivateKeyUnsafe = EcdsaSecp256k1PrivateKey

-- | Get the underlying `ByteArray` of an `EcdsaSecp256k1PrivateKey`.
getEcdsaSecp256k1PrivateKeyByteArray ∷ EcdsaSecp256k1PrivateKey → ByteArray
getEcdsaSecp256k1PrivateKeyByteArray (EcdsaSecp256k1PrivateKey byteArray) =
  byteArray

-- | Invariant: length of the message must be 32 bytes.
newtype EcdsaSecp256k1Message = EcdsaSecp256k1Message ByteArray

derive newtype instance Ord EcdsaSecp256k1Message

derive newtype instance Eq EcdsaSecp256k1Message

derive newtype instance ToData EcdsaSecp256k1Message

instance FromData EcdsaSecp256k1Message where
  fromData = fromData >=> ecdsaSecp256k1Message

instance Show EcdsaSecp256k1Message where
  show (EcdsaSecp256k1Message byteArray) =
    "(byteArrayToEcdsaSecp256k1MessageUnsafe "
      <> show byteArray
      <> ")"

-- | Smart constructor for `EcdsaSecp256k1Message` which verifies its invariant.
ecdsaSecp256k1Message ∷ ByteArray → Maybe EcdsaSecp256k1Message
ecdsaSecp256k1Message byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ EcdsaSecp256k1Message byteArray
  | otherwise = Nothing

-- | Construct an `EcdsaSecp256k1Message` without verifying its invariant. Use
-- | with extreme care.
byteArrayToEcdsaSecp256k1MessageUnsafe ∷ ByteArray → EcdsaSecp256k1Message
byteArrayToEcdsaSecp256k1MessageUnsafe = EcdsaSecp256k1Message

-- | `ecdsaSecp256k1MessageToAssetName` converts a sidechain message to a token name
ecdsaSecp256k1MessageToAssetName ∷ EcdsaSecp256k1Message → AssetName
ecdsaSecp256k1MessageToAssetName (EcdsaSecp256k1Message byteArray) =
  -- should be safe as they have the same length requirements
  -- i.e., token names should be less than or equal to 32 bytes long
  -- See:
  -- https://github.com/Plutonomicon/cardano-transaction-lib/blob/fde2e42b2e57ea978b3517913a1917ebf8836ab6/src/Internal/Types/AssetName.purs#L104-L109
  Unsafe.unsafePartial $ Maybe.fromJust $ mkAssetName byteArray

-- | Get the underlying `ByteArray` from an `EcdsaSecp256k1Message`.
getEcdsaSecp256k1MessageByteArray ∷ EcdsaSecp256k1Message → ByteArray
getEcdsaSecp256k1MessageByteArray (EcdsaSecp256k1Message byteArray) = byteArray

-- | Invariant: length of the signature must be 64 bytes.
newtype EcdsaSecp256k1Signature = EcdsaSecp256k1Signature ByteArray

derive newtype instance Ord EcdsaSecp256k1Signature

derive newtype instance Eq EcdsaSecp256k1Signature

derive newtype instance ToData EcdsaSecp256k1Signature

instance FromData EcdsaSecp256k1Signature where
  fromData = fromData >=> ecdsaSecp256k1Signature

instance Show EcdsaSecp256k1Signature where
  show (EcdsaSecp256k1Signature byteArray) =
    "(byteArrayToEcdsaSecp256k1SignatureUnsafe "
      <> show byteArray
      <> ")"

-- | `ecdsaSecp256k1Signature` is a smart constructor for `EcdsaSecp256k1Signature` to
-- | verify the invariants.
ecdsaSecp256k1Signature ∷ ByteArray → Maybe EcdsaSecp256k1Signature
ecdsaSecp256k1Signature byteArray
  | ByteArray.byteLength byteArray == 64 = Just $ EcdsaSecp256k1Signature
      byteArray
  | otherwise = Nothing

-- | Get the underlying `ByteArray` of a `EcdsaSecp256k1Signature`
getEcdsaSecp256k1SignatureByteArray ∷ EcdsaSecp256k1Signature → ByteArray
getEcdsaSecp256k1SignatureByteArray (EcdsaSecp256k1Signature byteArray) =
  byteArray

-- | Construct a `EcdsaSecp256k1Signature` without verifying invariants. Use
-- | with extreme care.
byteArrayToEcdsaSecp256k1SignatureUnsafe ∷ ByteArray → EcdsaSecp256k1Signature
byteArrayToEcdsaSecp256k1SignatureUnsafe = EcdsaSecp256k1Signature

-- TODO: newtype checks the type aliases above

foreign import generateRandomPrivateKey ∷ Effect EcdsaSecp256k1PrivateKey

foreign import toPubKeyUnsafe ∷ EcdsaSecp256k1PrivateKey → EcdsaSecp256k1PubKey

foreign import pubKeyVerify ∷ ByteArray → Boolean

foreign import secKeyVerify ∷ ByteArray → Boolean

foreign import sign ∷
  EcdsaSecp256k1Message → EcdsaSecp256k1PrivateKey → EcdsaSecp256k1Signature

foreign import signatureExport ∷ EcdsaSecp256k1Signature → ByteArray

foreign import verifyEcdsaSecp256k1Signature ∷
  EcdsaSecp256k1PubKey →
  EcdsaSecp256k1Message →
  EcdsaSecp256k1Signature →
  Boolean

-- | Serialises a signature to DER format as hex encoded bytes
serialiseEcdsaSecp256k1SignatureToDer ∷ EcdsaSecp256k1Signature → String
serialiseEcdsaSecp256k1SignatureToDer = ByteArray.byteArrayToHex <<<
  signatureExport

generatePrivKey ∷ Effect EcdsaSecp256k1PrivateKey
generatePrivKey =
  generateRandomPrivateKey

multiSign ∷
  Array EcdsaSecp256k1PrivateKey →
  EcdsaSecp256k1Message →
  Array EcdsaSecp256k1Signature
multiSign xkeys msg = map (sign msg) xkeys

-- | `normalizeCommitteePubKeysAndSignatures` takes a list of public keys and their
-- | associated signatures and sorts by the natural lexicographical ordering of the
-- | `EcdsaSecp256k1PubKey`s
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
-- |    `EcdsaSecp256k1PubKey` by `normalizeCommitteePubKeysAndSignatures`
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
          ( unsafePartial
          $ fromJust
          $ BigInt.fromString
          $ RegularBigInt.toString
          ( RegularBigInt.quot
              ((unsafePartial $ fromJust $ RegularBigInt.fromString (BigInt.toString thresholdNumerator)) * RegularBigInt.fromInt (Array.length pubKeys))
              ((unsafePartial $ fromJust $ RegularBigInt.fromString (BigInt.toString thresholdDenominator)))
          )
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





foreign import blake2b256 :: String -> String
blake2b256Hash :: Partial => ByteArray -> ByteArray
blake2b256Hash d = hexToByteArrayUnsafe $ blake2b256 $ byteArrayToHex d



-- | `aggregateKeys` aggregates a list of keys s.t. the resulting `ByteArray`
-- | may be stored in the `UpdateCommitteeDatum` in an onchain compatible way.
-- | Note: this sorts the input array
aggregateKeys ∷ Partial => Array ByteArray → ByteArray
aggregateKeys keys = blake2b256Hash $ mconcat $ Array.sort keys



