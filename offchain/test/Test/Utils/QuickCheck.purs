module Test.Utils.QuickCheck
  ( NonNegative(..)
  , Positive(..)
  , DA
  , ArbitraryTransactionInput(..)
  , ArbitraryAddress(..)
  , ArbitraryTransactionHash(..)
  , ArbitraryUInt(..)
  , ArbitraryBigInt(..)
  , ArbitraryPubKey(..)
  , ArbitrarySignature(..)
  , ArbitraryPubKeyHash(..)
  , ArbitraryPaymentPubKeyHash(..)
  , ArbitraryCurrencySymbol(..)
  , ArbitraryValidatorHash(..)
  , ArbitraryCredential(..)
  , ArbitraryAssetClass(..)
  , ArbitraryTokenName(..)
  , suchThatMap
  , class Arbitrary1
  , liftArbitrary
  ) where

import Contract.Prelude hiding (oneOf)

import Aeson (decodeAeson, encodeAeson)
import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  )
import Contract.PlutusData
  ( class FromData
  , class ToData
  , toData
  )
import Contract.Scripts
  ( PlutusScript
  , ValidatorHash(ValidatorHash)
  , applyArgs
  )
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  , mkTokenName
  , scriptHashAsCurrencySymbol
  )
import Control.Monad.Rec.Class
  ( Step(Loop, Done)
  , tailRecM
  )
import Ctl.Internal.Hashing (plutusScriptHash)
import Ctl.Internal.Plutus.Types.Address (Address, pubKeyHashAddress)
import Ctl.Internal.Plutus.Types.Credential
  ( Credential
      ( PubKeyCredential
      , ScriptCredential
      )
  )
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  , scriptHashToBytes
  )
import Ctl.Internal.Types.ByteArray
  ( byteArrayFromIntArrayUnsafe
  , byteArrayToIntArray
  )
import Data.Array.NonEmpty as NEA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.List.Types as NE
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.Ord (abs)
import Data.Tuple (Tuple(Tuple))
import Data.UInt (UInt)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary
  ( class Arbitrary
  , class Coarbitrary
  , arbitrary
  , coarbitrary
  )
import Test.QuickCheck.Gen
  ( Gen
  , arrayOf
  , chooseInt
  , frequency
  , oneOf
  , resize
  , sized
  , suchThat
  , vectorOf
  )
import TrustlessSidechain.Types (AssetClass, PubKey, Signature)

-- | Generator wrapper for 'TokenName'
newtype ArbitraryTokenName = ArbitraryTokenName TokenName

derive newtype instance Eq ArbitraryTokenName

derive newtype instance Ord ArbitraryTokenName

derive instance Generic ArbitraryTokenName _

instance Show ArbitraryTokenName where
  show = genericShow

instance Arbitrary ArbitraryTokenName where
  arbitrary = ArbitraryTokenName <$> do
    len ← chooseInt 0 32
    suchThatMap (vectorOf len genByteInt)
      (byteArrayFromIntArrayUnsafe >>> mkTokenName)

instance Coarbitrary ArbitraryTokenName where
  coarbitrary (ArbitraryTokenName tn) =
    (getTokenName >>> byteArrayToIntArray >>> coarbitrary) tn

-- | Generator wrapper for 'AssetClass'
newtype ArbitraryAssetClass = ArbitraryAssetClass AssetClass

derive newtype instance Eq ArbitraryAssetClass

derive newtype instance Ord ArbitraryAssetClass

derive instance Generic ArbitraryAssetClass _

instance Show ArbitraryAssetClass where
  show = genericShow

instance Arbitrary ArbitraryAssetClass where
  arbitrary = ArbitraryAssetClass <$> do
    ArbitraryCurrencySymbol cs ← arbitrary
    ArbitraryTokenName tn ← arbitrary
    pure $ cs /\ tn

instance Coarbitrary ArbitraryAssetClass where
  coarbitrary (ArbitraryAssetClass (cs /\ tn)) =
    coarbitrary (ArbitraryCurrencySymbol cs) >>>
      coarbitrary (ArbitraryTokenName tn)

-- | Generator wrapper for 'Credential'
newtype ArbitraryCredential = ArbitraryCredential Credential

derive newtype instance Eq ArbitraryCredential

derive newtype instance Ord ArbitraryCredential

derive instance Generic ArbitraryCredential _

instance Show ArbitraryCredential where
  show = genericShow

instance Arbitrary ArbitraryCredential where
  arbitrary = oneOf $ map (map ArbitraryCredential) $
    NEA.cons' (PubKeyCredential <$> go)
      [ ScriptCredential <$> go2 ]
    where
    go ∷ Gen PubKeyHash
    go = do
      ArbitraryPubKeyHash pkh ← arbitrary
      pure pkh

    go2 ∷ Gen ValidatorHash
    go2 = do
      ArbitraryValidatorHash vh ← arbitrary
      pure vh

-- | Generator wrapper for 'Address'
newtype ArbitraryAddress = ArbitraryAddress Address

derive newtype instance Eq ArbitraryAddress

derive newtype instance Ord ArbitraryAddress

derive instance Generic ArbitraryAddress _

instance Show ArbitraryAddress where
  show = genericShow

instance Arbitrary ArbitraryAddress where
  arbitrary = ArbitraryAddress <$> do
    ArbitraryPaymentPubKeyHash pkh ← arbitrary
    scred ← arbitrary
    pure $ case scred of
      Nothing → pubKeyHashAddress pkh Nothing
      Just (ArbitraryCredential addressStakingCredential) →
        pubKeyHashAddress pkh (Just addressStakingCredential)

-- | A 'fill in' for polymorphic types whose exact details we don't care about.
-- | Can be used for testing higher-kinded types for stuff requiring Data
-- | encodings.
newtype DA = DA BigInt

derive newtype instance Eq DA

derive newtype instance Ord DA

derive newtype instance FromData DA

derive newtype instance ToData DA

instance Show DA where
  show _ = "DA"

instance Arbitrary DA where
  arbitrary = do
    ArbitraryBigInt bi ← arbitrary
    pure $ DA bi

instance Coarbitrary DA where
  coarbitrary (DA bi) = coarbitrary (ArbitraryBigInt bi)

-- | Generator wrapper for CurrencySymbol
newtype ArbitraryCurrencySymbol = ArbitraryCurrencySymbol CurrencySymbol

derive newtype instance Eq ArbitraryCurrencySymbol

derive newtype instance Ord ArbitraryCurrencySymbol

derive instance Generic ArbitraryCurrencySymbol _

instance Show ArbitraryCurrencySymbol where
  show = genericShow

instance Arbitrary ArbitraryCurrencySymbol where
  arbitrary = ArbitraryCurrencySymbol <$> do
    x ← BigInt.fromInt <$> arbitrary
    pure $ scriptHashAsCurrencySymbol $ plutusScriptHash (noncedAlwaysFail x)

instance Coarbitrary ArbitraryCurrencySymbol where
  coarbitrary (ArbitraryCurrencySymbol cs) =
    (getCurrencySymbol >>> byteArrayToIntArray >>> coarbitrary) cs

-- | Generator wrapper for ValidatorHash
newtype ArbitraryValidatorHash = ArbitraryValidatorHash ValidatorHash

derive newtype instance Eq ArbitraryValidatorHash

derive newtype instance Ord ArbitraryValidatorHash

derive instance Generic ArbitraryValidatorHash _

instance Show ArbitraryValidatorHash where
  show = genericShow

instance Arbitrary ArbitraryValidatorHash where
  arbitrary = (ValidatorHash >>> ArbitraryValidatorHash) <$> do
    x ← BigInt.fromInt <$> arbitrary
    pure $ plutusScriptHash (noncedAlwaysFail x)

instance Coarbitrary ArbitraryValidatorHash where
  coarbitrary (ArbitraryValidatorHash (ValidatorHash vh)) =
    (scriptHashToBytes >>> unwrap >>> byteArrayToIntArray >>> coarbitrary) vh

-- | A 'lifter' for 'Arbitrary' to values of kind 'Type -> Type'.
class Arbitrary1 (f ∷ Type → Type) where
  liftArbitrary ∷ ∀ (a ∷ Type). Gen a → Gen (f a)

instance Arbitrary1 Maybe where
  liftArbitrary gen =
    frequency $ NE.NonEmptyList $ NonEmpty (Tuple 1.0 (pure Nothing)) $
      pure (Tuple 3.0 (Just <$> gen))

instance Arbitrary1 Array where
  liftArbitrary = arrayOf

-- | Generator wrapper for PubKey
newtype ArbitraryPubKey = ArbitraryPubKey PubKey

derive newtype instance Eq ArbitraryPubKey

derive newtype instance Ord ArbitraryPubKey

derive instance Generic ArbitraryPubKey _

instance Show ArbitraryPubKey where
  show = genericShow

instance Arbitrary ArbitraryPubKey where
  arbitrary =
    (byteArrayFromIntArrayUnsafe >>> ArbitraryPubKey) <$>
      vectorOf 32 genByteInt

instance Coarbitrary ArbitraryPubKey where
  coarbitrary (ArbitraryPubKey ba) =
    coarbitrary (byteArrayToIntArray ba)

-- | Generator wrapper for Signature
newtype ArbitrarySignature = ArbitrarySignature Signature

derive newtype instance Eq ArbitrarySignature

derive newtype instance Ord ArbitrarySignature

derive instance Generic ArbitrarySignature _

instance Arbitrary ArbitrarySignature where
  arbitrary =
    (byteArrayFromIntArrayUnsafe >>> ArbitrarySignature) <$>
      vectorOf 64 genByteInt

instance Coarbitrary ArbitrarySignature where
  coarbitrary (ArbitrarySignature sig) =
    coarbitrary (byteArrayToIntArray sig)

-- | Generator wrapper for PubKeyHash
newtype ArbitraryPubKeyHash = ArbitraryPubKeyHash PubKeyHash

derive newtype instance Eq ArbitraryPubKeyHash

derive newtype instance Ord ArbitraryPubKeyHash

derive instance Generic ArbitraryPubKeyHash _

instance Arbitrary ArbitraryPubKeyHash where
  arbitrary = (PubKeyHash >>> ArbitraryPubKeyHash) <$>
    suchThatMap (byteArrayFromIntArrayUnsafe <$> vectorOf 28 genByteInt)
      ed25519KeyHashFromBytes

instance Coarbitrary ArbitraryPubKeyHash where
  coarbitrary (ArbitraryPubKeyHash (PubKeyHash x)) =
    coarbitrary (byteArrayToIntArray $ unwrap $ ed25519KeyHashToBytes x)

-- | Generator wrapper for PaymentPubKeyHash
newtype ArbitraryPaymentPubKeyHash = ArbitraryPaymentPubKeyHash
  PaymentPubKeyHash

derive newtype instance Eq ArbitraryPaymentPubKeyHash

derive newtype instance Ord ArbitraryPaymentPubKeyHash

derive instance Generic ArbitraryPaymentPubKeyHash _

instance Arbitrary ArbitraryPaymentPubKeyHash where
  arbitrary = do
    ArbitraryPubKeyHash pkh ← arbitrary
    pure (ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh))

instance Coarbitrary ArbitraryPaymentPubKeyHash where
  coarbitrary (ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh)) =
    coarbitrary (ArbitraryPubKeyHash pkh)

-- | Ensure that the generator only produces values for which the Kleisli arrow
-- | 'hits'. If the Kleisli arrow never 'hits', this will loop forever.
suchThatMap ∷
  ∀ (a ∷ Type) (b ∷ Type).
  Gen a →
  (a → Maybe b) →
  Gen b
suchThatMap gen arrow = sized $ tailRecM go
  where
  go ∷ Int → Gen (Step Int b)
  go size = do
    x ← resize size gen
    pure $ case arrow x of
      Nothing → Loop (2 * size)
      Just y → Done y

-- | Generator wrapper for BigInt
newtype ArbitraryBigInt = ArbitraryBigInt BigInt

derive newtype instance Eq ArbitraryBigInt

derive newtype instance Ord ArbitraryBigInt

derive newtype instance Semiring ArbitraryBigInt

derive newtype instance Ring ArbitraryBigInt

derive instance Generic ArbitraryBigInt _

instance Show ArbitraryBigInt where
  show = genericShow

instance Arbitrary ArbitraryBigInt where
  arbitrary = (BigInt.fromInt >>> ArbitraryBigInt) <$> arbitrary

instance Coarbitrary ArbitraryBigInt where
  coarbitrary (ArbitraryBigInt x) =
    coarbitrary (BigInt.toString x)

-- | Generator wrapper for UInt
newtype ArbitraryUInt = ArbitraryUInt UInt

derive newtype instance Eq ArbitraryUInt

derive newtype instance Ord ArbitraryUInt

derive instance Generic ArbitraryUInt _

instance Show ArbitraryUInt where
  show = genericShow

instance Arbitrary ArbitraryUInt where
  arbitrary = (UInt.fromInt >>> ArbitraryUInt) <$> arbitrary

instance Coarbitrary ArbitraryUInt where
  coarbitrary (ArbitraryUInt x) = coarbitrary $ UInt.toInt x

-- | Generator wrapper for TransactionHash
newtype ArbitraryTransactionHash = ArbitraryTransactionHash TransactionHash

derive newtype instance Eq ArbitraryTransactionHash

derive newtype instance Ord ArbitraryTransactionHash

derive instance Generic ArbitraryTransactionHash _

instance Show ArbitraryTransactionHash where
  show = genericShow

instance Arbitrary ArbitraryTransactionHash where
  arbitrary =
    ( byteArrayFromIntArrayUnsafe >>> TransactionHash >>>
        ArbitraryTransactionHash
    ) <$> vectorOf 32 genByteInt

instance Coarbitrary ArbitraryTransactionHash where
  coarbitrary (ArbitraryTransactionHash (TransactionHash x)) =
    coarbitrary (byteArrayToIntArray x)

-- | Generator wrapper for TransactionInput
newtype ArbitraryTransactionInput = ArbitraryTransactionInput TransactionInput

derive newtype instance Eq ArbitraryTransactionInput

derive newtype instance Ord ArbitraryTransactionInput

derive instance Generic ArbitraryTransactionInput _

instance Show ArbitraryTransactionInput where
  show = genericShow

instance Arbitrary ArbitraryTransactionInput where
  arbitrary = ArbitraryTransactionInput <$> do
    ArbitraryTransactionHash transactionId ← arbitrary
    ArbitraryUInt index ← arbitrary
    pure $ TransactionInput
      { transactionId
      , index
      }

instance Coarbitrary ArbitraryTransactionInput where
  coarbitrary (ArbitraryTransactionInput (TransactionInput x)) =
    coarbitrary (ArbitraryUInt x.index) >>>
      coarbitrary (ArbitraryTransactionHash x.transactionId)

-- | Generator wrapper for number-like types that should never be negative (that
-- | is, must be zero or larger).
newtype NonNegative (a ∷ Type) = NonNegative a

derive instance Functor NonNegative

derive newtype instance (Eq a) ⇒ Eq (NonNegative a)

derive newtype instance (Ord a) ⇒ Ord (NonNegative a)

derive instance Generic (NonNegative a) _

instance (Show a) ⇒ Show (NonNegative a) where
  show = genericShow

instance (Arbitrary a, Ord a, Ring a) ⇒ Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> do
    x ← arbitrary
    pure $ abs x

instance (Coarbitrary a) ⇒ Coarbitrary (NonNegative a) where
  coarbitrary (NonNegative x) = coarbitrary x

-- | Generator wrapper for number-like types that should always be positive
-- | (that is, must be larger than zero).
newtype Positive (a ∷ Type) = Positive a

derive instance Functor Positive

derive newtype instance (Eq a) ⇒ Eq (Positive a)

derive newtype instance (Ord a) ⇒ Ord (Positive a)

derive instance Generic (Positive a) _

instance (Show a) ⇒ Show (Positive a) where
  show = genericShow

instance (Arbitrary a, Ord a, Semiring a) ⇒ Arbitrary (Positive a) where
  arbitrary = Positive <$> do
    x ← suchThat arbitrary (_ > zero)
    pure x

instance (Coarbitrary a) ⇒ Coarbitrary (Positive a) where
  coarbitrary (Positive x) = coarbitrary x

-- helpers

genByteInt ∷ Gen Int
genByteInt = chooseInt 0 255

noncedAlwaysFail ∷
  ∀ (a ∷ Type).
  ToData a ⇒
  a →
  PlutusScript
noncedAlwaysFail x = unsafePartial $ fromJust do
  script ← hush
    (decodeAeson (encodeAeson [ "4701000022224981", "PlutusV2" ]))
  applyArgs script [ toData x ] # hush
