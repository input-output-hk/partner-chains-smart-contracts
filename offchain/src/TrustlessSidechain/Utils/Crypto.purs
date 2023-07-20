module TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1Message
  , ecdsaSecp256k1Message
  , byteArrayToEcdsaSecp256k1MessageUnsafe
  , SidechainPrivateKey
  , byteArrayToEcdsaSecp256k1PubKeyUnsafe
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
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
  , getSidechainPrivateKeyByteArray
  , getEcdsaSecp256k1SignatureByteArray
  , byteArrayToSidechainPrivateKeyUnsafe
  , sidechainPrivateKey
  , getEcdsaSecp256k1MessageByteArray
  , byteArrayToEcdsaSecp256k1SignatureUnsafe
  , ecdsaSecp256k1Signature
  , aggregateKeys
  , countEnoughSignatures
  , takeExactlyEnoughSignatures
  ) where

import Contract.Prelude

import Contract.Hashing as Hashing
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData, fromData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (on)
import Data.Maybe as Maybe
import Data.Ord as Ord
import Partial.Unsafe as Unsafe
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))

-- | Invariant: length of the pubkey must be 33 bytes.
-- | Format: Compressed and serialized as per ECDSA signatures for SECP256k1.
-- | Check the leading byte for valid key format.
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey ByteArray

derive newtype instance Eq EcdsaSecp256k1PubKey

derive newtype instance Ord EcdsaSecp256k1PubKey

derive newtype instance ToData EcdsaSecp256k1PubKey

instance FromData EcdsaSecp256k1PubKey where
  fromData = fromData >=> ecdsaSecp256k1PubKey

instance Show EcdsaSecp256k1PubKey where
  show (EcdsaSecp256k1PubKey byteArray) =
    "(byteArrayToEcdsaSecp256k1PubKeyUnsafe "
      <> show byteArray
      <> ")"

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

-- | Smart constructor for `EcdsaSecp256k1Signature`, which verifies its
-- | invariants.
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

foreign import generateRandomPrivateKey ∷ Effect SidechainPrivateKey

foreign import toPubKeyUnsafe ∷ SidechainPrivateKey → EcdsaSecp256k1PubKey

foreign import pubKeyVerify ∷ ByteArray → Boolean

foreign import secKeyVerify ∷ ByteArray → Boolean

foreign import sign ∷
  EcdsaSecp256k1Message → SidechainPrivateKey → EcdsaSecp256k1Signature

foreign import verifyEcdsaSecp256k1Signature ∷
  EcdsaSecp256k1PubKey →
  EcdsaSecp256k1Message →
  EcdsaSecp256k1Signature →
  Boolean

generatePrivKey ∷ Contract SidechainPrivateKey
generatePrivKey =
  liftEffect generateRandomPrivateKey

multiSign ∷
  Array SidechainPrivateKey →
  EcdsaSecp256k1Message →
  Array EcdsaSecp256k1Signature
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
  Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature) →
  Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)
normalizeCommitteePubKeysAndSignatures = Array.sortBy (Ord.compare `on` fst)

-- | `unzipCommitteePubKeysAndSignatures` unzips public keys and associated
-- | signatures, and removes all the `Nothing` signatures.
-- |
-- | Preconditions to be compatible with the onchain Haskell multisign method:
-- |    - The input array should be sorted lexicographically by
-- |    `SidechainPublicKey` by `normalizeCommitteePubKeysAndSignatures`
unzipCommitteePubKeysAndSignatures ∷
  Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature) →
  Tuple (Array EcdsaSecp256k1PubKey) (Array EcdsaSecp256k1Signature)
unzipCommitteePubKeysAndSignatures = map Array.catMaybes <<< Array.unzip

-- | `countEnoughSignatures` counts the minimum number of signatures needed for
-- | the onchain code to verify successfully.
countEnoughSignatures ∷
  SidechainParams →
  Array EcdsaSecp256k1PubKey →
  BigInt
countEnoughSignatures (SidechainParams params) arr =
  let
    len = BigInt.fromInt $ Array.length arr
  in
    one + ((params.thresholdNumerator * len) / params.thresholdDenominator)

-- | `takeExactlyEnoughSignatures` takes exactly enough signatures (if it
-- | or less than if it cannot) for committee certificate verification as a
-- | minor optimization so that we only provide the onchain code with the
-- | minimum amount of
-- | signatures needed.
takeExactlyEnoughSignatures ∷
  SidechainParams →
  Array EcdsaSecp256k1PubKey /\ Array EcdsaSecp256k1Signature →
  Array EcdsaSecp256k1PubKey /\ Array EcdsaSecp256k1Signature
takeExactlyEnoughSignatures sc (pks /\ sigs) =
  pks /\
    Array.take
      -- It should be big enough to fit in a plain old int as this
      -- corresponds to the array length (size of int is log of array
      -- length)
      -- TODO
      ( Unsafe.unsafePartial $ Maybe.fromJust $ BigInt.toInt
          (countEnoughSignatures sc pks)
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
  BigInt →
  BigInt →
  Array EcdsaSecp256k1PubKey →
  EcdsaSecp256k1Message →
  Array EcdsaSecp256k1Signature →
  Boolean
verifyMultiSignature
  thresholdNumerator
  thresholdDenominator
  pubKeys
  msg
  signatures =
  let
    go ∷
      BigInt →
      Array EcdsaSecp256k1PubKey →
      Array EcdsaSecp256k1Signature →
      Boolean
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
                if verifyEcdsaSecp256k1Signature pub msg sig then
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
-- | may be stored in the `UpdateCommitteeHashDatum` in an onchain compatible way.
-- | For this to be truly compatible with the onchain function, you need to ensure
-- | that the input list is sorted
aggregateKeys ∷ Array EcdsaSecp256k1PubKey → ByteArray
aggregateKeys = Hashing.blake2b256Hash <<< foldMap
  getEcdsaSecp256k1PubKeyByteArray
