module TrustlessSidechain.Options.Parsers
  ( parsePubKeyAndSignature
  , transactionInput
  , combinedMerkleProofParser
  , committeeSignature
  , sidechainSignature
  , sidechainPublicKey
  , sidechainAddress
  , combinedMerkleProofParserWithPkh
  , uint
  , bigInt
  , byteArray
  , rootHash
  , thresholdFraction
  , numerator
  , denominator
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.CborBytes (CborBytes, cborBytesFromByteArray)
import Contract.PlutusData (fromData)
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData (convertPlutusData)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.EuclideanRing as EuclideanRing
import Data.String (Pattern(Pattern), split)
import Data.UInt (UInt)
import Data.UInt as UInt
import Options.Applicative (ReadM, eitherReader, maybeReader, readerError)
import TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof
  , addressFromCborBytes
  , getBech32BytesByteArray
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

-- | Parse a transaction input from a CLI format (e.g. `aabbcc#0`)
transactionInput ∷ ReadM TransactionInput
transactionInput = maybeReader \txIn →
  case split (Pattern "#") txIn of
    [ txId, txIdx ] → ado
      index ← UInt.fromString txIdx
      transactionId ← TransactionHash <$> hexToByteArray txId
      in
        TransactionInput
          { transactionId
          , index
          }
    _ → Nothing

toCombinedMerkleProof ∷ CborBytes → Maybe CombinedMerkleProof
toCombinedMerkleProof = unwrap >>> fromBytes >=> convertPlutusData >=> fromData

combinedMerkleProofParser ∷ ReadM CombinedMerkleProof
combinedMerkleProofParser = cbor >>= toCombinedMerkleProof >>>
  maybe (readerError "Error while parsing supplied CBOR as CombinedMerkleProof.")
    pure

-- | This parser will convert the raw bytestring to a valid Cardano address
combinedMerkleProofParserWithPkh ∷
  ReadM (CombinedMerkleProof /\ Address)
combinedMerkleProofParserWithPkh = do
  cmp ← combinedMerkleProofParser
  -- Getting the parsed recipient from the combined proof and deserialising to
  -- an address
  let
    recipient = getBech32BytesByteArray $
      (unwrap (unwrap cmp).transaction).recipient
  addr ← maybe
    (readerError "Couldn't convert recipient bech32 to Plutus address")
    pure
    (addressFromCborBytes (cborBytesFromByteArray recipient))

  pure (cmp /\ addr)

-- | Parse ByteArray from hexadecimal representation
byteArray ∷ ReadM ByteArray
byteArray = maybeReader hexToByteArray

-- | Parses a SidechainPublicKey from hexadecimal representation.
-- | See `SidechainPublicKey` for the invariants.
sidechainPublicKey ∷ ReadM SidechainPublicKey
sidechainPublicKey = maybeReader
  $ Utils.Crypto.sidechainPublicKey
  <=< hexToByteArray

-- | Parses a SidechainSignature from hexadecimal representation.
-- | See `SidechainSignature` for the invariants.
sidechainSignature ∷ ReadM SidechainSignature
sidechainSignature = maybeReader
  $ Utils.Crypto.sidechainSignature
  <=< hexToByteArray

-- | Parse only CBOR encoded hexadecimal
-- Note: This assumes there will be some validation with the CborBytes, otherwise
-- we should simplify the code and fall back to ByteArray.
cbor ∷ ReadM CborBytes
cbor = cborBytesFromByteArray <$> byteArray

-- | Parse BigInt
bigInt ∷ ReadM BigInt
bigInt = maybeReader BigInt.fromString

-- | Parse a numerator in the threshold.
numerator ∷ ReadM BigInt
numerator = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i >= zero → pure i
        | otherwise → Left "numerator must be non-negative"
      Nothing → Left "failed to parse int numerator"
  )

-- | Parse a denominator in the threshold.
denominator ∷ ReadM BigInt
denominator = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i > zero → pure i
        | otherwise → Left "denominator must be positive"
      Nothing → Left "failed to parse int denominator"
  )

-- | Parse UInt
uint ∷ ReadM UInt
uint = maybeReader UInt.fromString

-- | Parses the raw bytes of a `RootHash`
rootHash ∷ ReadM RootHash
rootHash = maybeReader (MerkleTree.rootHashFromByteArray <=< hexToByteArray)

-- | `sidechainAddress` parses
-- | ```
-- | sidechainAddress
-- |        -> 0x hexStr
-- |        -> hexStr
-- | ```
-- where `hexStr` is a sequence of hex digits.
sidechainAddress ∷ ReadM ByteArray
sidechainAddress = maybeReader $ \str →
  case split (Pattern "0x") str of
    [ "", hex ] → hexToByteArray hex
    [ hex ] → hexToByteArray hex
    _ → Nothing

-- | `thresholdFraction` is the CLI parser for `parseThresholdFraction`.
thresholdFraction ∷
  ReadM { thresholdNumerator ∷ BigInt, thresholdDenominator ∷ BigInt }
thresholdFraction = eitherReader parseThresholdFraction

-- | `parseThresholdFraction` parses the threshold represented as `Num/Denom`
-- | and verifies that
-- |        - Num <= Denom
-- |        - Num >= 0, Denom > 0
-- |        - Num and Denom are coprime
parseThresholdFraction ∷
  String →
  Either String { thresholdNumerator ∷ BigInt, thresholdDenominator ∷ BigInt }
parseThresholdFraction str =
  case split (Pattern "/") str of
    [ n, d ] | n /= "" && d /= "" → do
      let
        fromString' = maybe (Left "failed to parse Int from String") pure <<<
          BigInt.fromString
      thresholdNumerator ← fromString' n
      thresholdDenominator ← fromString' d
      if
        -- not totally too sure if purescript short circuits, so write out
        -- the if statements explicitly..
        ( if thresholdNumerator >= zero && thresholdDenominator > zero then
            thresholdDenominator >= thresholdNumerator
              && EuclideanRing.gcd thresholdDenominator thresholdNumerator
              == one
          else false
        ) then pure { thresholdNumerator, thresholdDenominator }
      else Left
        "'n/m' must be coprime, in the interval [0,1], and both non-negative."
    _ → Left "failed to parse ratio 'n/m'"

-- | `committeeSignature` is a the CLI parser for `parsePubKeyAndSignature`.
committeeSignature ∷ ReadM (SidechainPublicKey /\ Maybe SidechainSignature)
committeeSignature = maybeReader parsePubKeyAndSignature

-- | `parsePubKeyAndSignature` parses the following format `hexStr[:[hexStr]]`
-- Note: should we make this more strict and disallow `aa:`? in a sense:
-- `aa` denotes a pubkey without a signature
-- `aa:bb` denotes a pubkey and a signature
-- anything else is likely an error, and should be treated as malformed input
parsePubKeyAndSignature ∷
  String → Maybe (SidechainPublicKey /\ Maybe SidechainSignature)
parsePubKeyAndSignature str =
  case split (Pattern ":") str of
    [ l, r ] | l /= "" → do
      l' ← Utils.Crypto.sidechainPublicKey <=< hexToByteArray $ l
      if r == "" then pure $ l' /\ Nothing
      else do
        r' ← Utils.Crypto.sidechainSignature <=< hexToByteArray $ r
        pure $ l' /\ Just r'
    [ l ] → ado
      l' ← Utils.Crypto.sidechainPublicKey <=< hexToByteArray $ l
      in l' /\ Nothing
    _ → Nothing
