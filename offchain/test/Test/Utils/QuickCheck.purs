module Test.Utils.QuickCheck
  ( NonNegative(..)
  , Positive(..)
  , DA
  , ArbitraryTransactionInput(..)
  , ArbitraryAddress(..)
  , ArbitraryTransactionHash(..)
  , ArbitraryUInt(..)
  , ArbitraryBigNum(..)
  , ArbitraryBigInt(..)
  , ArbitraryPubKey(..)
  , ArbitrarySignature(..)
  , ArbitraryPubKeyHash(..)
  , ArbitraryPaymentPubKeyHash(..)
  , ArbitraryScriptHash(..)
  , ArbitraryCredential(..)
  , ArbitraryAsset(..)
  , ArbitraryAssetName(..)
  , suchThatMap
  , class Arbitrary1
  , liftArbitrary
  ) where

import Contract.Prelude hiding (oneOf)

import Aeson (decodeAeson, encodeAeson)
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address (Address, pubKeyHashAddress)
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  )
import Cardano.Plutus.Types.PaymentPubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash))
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash)
import Cardano.Types.Asset (Asset(Asset, AdaAsset))
import Cardano.Types.AssetName (AssetName, mkAssetName, unAssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (fromInt, toString) as BigNum
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.PlutusData (class FromData, class ToData, toData)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Data.Array.NonEmpty (fromNonEmpty) as NonEmptyArray
import Data.Array.NonEmpty as NEA
import Data.ByteArray (byteArrayFromIntArrayUnsafe, byteArrayToIntArray)
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.Ord (abs)
import Data.Tuple (Tuple(Tuple))
import Data.UInt (UInt)
import Data.UInt as UInt
import JS.BigInt as BigInt
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
import TrustlessSidechain.Types (PubKey, Signature)

-- | Generator wrapper for 'AssetName'
newtype ArbitraryAssetName = ArbitraryAssetName AssetName

derive newtype instance Eq ArbitraryAssetName

derive newtype instance Ord ArbitraryAssetName

derive instance Generic ArbitraryAssetName _

instance Show ArbitraryAssetName where
  show = genericShow

instance Arbitrary ArbitraryAssetName where
  arbitrary = ArbitraryAssetName <$> do
    len <- chooseInt 0 32
    suchThatMap (vectorOf len genByteInt)
      (byteArrayFromIntArrayUnsafe >>> mkAssetName)

instance Coarbitrary ArbitraryAssetName where
  coarbitrary (ArbitraryAssetName an) =
    (unAssetName >>> byteArrayToIntArray >>> coarbitrary) an

-- | Generator wrapper for 'AssetClass'
newtype ArbitraryAsset = ArbitraryAsset Asset

derive newtype instance Eq ArbitraryAsset

derive newtype instance Ord ArbitraryAsset

derive instance Generic ArbitraryAsset _

instance Show ArbitraryAsset where
  show = genericShow

instance Arbitrary ArbitraryAsset where
  arbitrary = ArbitraryAsset <$> do
    ArbitraryScriptHash sh <- arbitrary
    ArbitraryAssetName an <- arbitrary
    pure $ Asset sh an

instance Coarbitrary ArbitraryAsset where
  coarbitrary (ArbitraryAsset (Asset sh an)) =
    coarbitrary (ArbitraryScriptHash sh) >>>
      coarbitrary (ArbitraryAssetName an)
  coarbitrary (ArbitraryAsset AdaAsset) = \x -> x

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
    go :: Gen PubKeyHash
    go = do
      ArbitraryPubKeyHash pkh <- arbitrary
      pure pkh

    go2 :: Gen ValidatorHash
    go2 = do
      ArbitraryScriptHash sh <- arbitrary
      pure $ wrap sh

-- | Generator wrapper for 'Address'
newtype ArbitraryAddress = ArbitraryAddress Address

derive newtype instance Eq ArbitraryAddress

derive newtype instance Ord ArbitraryAddress

derive instance Generic ArbitraryAddress _

instance Show ArbitraryAddress where
  show = genericShow

instance Arbitrary ArbitraryAddress where
  arbitrary = ArbitraryAddress <$> do
    ArbitraryPaymentPubKeyHash pkh <- arbitrary
    scred <- arbitrary
    pure $ case scred of
      Nothing -> pubKeyHashAddress pkh Nothing
      Just (ArbitraryCredential addressStakingCredential) ->
        pubKeyHashAddress pkh (Just addressStakingCredential)

-- | A 'fill in' for polymorphic types whose exact details we don't care about.
-- | Can be used for testing higher-kinded types for stuff requiring Data
-- | encodings.
newtype DA = DA BigNum

derive newtype instance Eq DA

derive newtype instance Ord DA

derive newtype instance FromData DA

derive newtype instance ToData DA

instance Show DA where
  show _ = "DA"

instance Arbitrary DA where
  arbitrary = do
    ArbitraryBigNum bi <- arbitrary
    pure $ DA bi

instance Coarbitrary DA where
  coarbitrary (DA bi) = coarbitrary (ArbitraryBigNum bi)

-- | Generator wrapper for ScriptHash
newtype ArbitraryScriptHash = ArbitraryScriptHash ScriptHash

derive newtype instance Eq ArbitraryScriptHash

derive newtype instance Ord ArbitraryScriptHash

derive instance Generic ArbitraryScriptHash _

instance Show ArbitraryScriptHash where
  show = genericShow

instance Arbitrary ArbitraryScriptHash where
  arbitrary = do
    x <- BigNum.fromInt <$> arbitrary
    pure $ ArbitraryScriptHash $ PlutusScript.hash (noncedAlwaysFail x)

instance Coarbitrary ArbitraryScriptHash where
  coarbitrary (ArbitraryScriptHash sh) =
    (encodeCbor >>> unwrap >>> byteArrayToIntArray >>> coarbitrary) sh

-- | A 'lifter' for 'Arbitrary' to values of kind 'Type -> Type'.
class Arbitrary1 (f :: Type -> Type) where
  liftArbitrary :: forall (a :: Type). Gen a -> Gen (f a)

instance Arbitrary1 Maybe where
  liftArbitrary gen =
    frequency $ NonEmptyArray.fromNonEmpty $ NonEmpty (Tuple 1.0 (pure Nothing))
      [ (Tuple 3.0 (Just <$> gen)) ]

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
      (wrap >>> decodeCbor)

instance Coarbitrary ArbitraryPubKeyHash where
  coarbitrary (ArbitraryPubKeyHash (PubKeyHash x)) =
    coarbitrary (byteArrayToIntArray $ unwrap $ encodeCbor x)

-- | Generator wrapper for PaymentPubKeyHash
newtype ArbitraryPaymentPubKeyHash = ArbitraryPaymentPubKeyHash
  PaymentPubKeyHash

derive newtype instance Eq ArbitraryPaymentPubKeyHash

derive newtype instance Ord ArbitraryPaymentPubKeyHash

derive instance Generic ArbitraryPaymentPubKeyHash _

instance Arbitrary ArbitraryPaymentPubKeyHash where
  arbitrary = do
    ArbitraryPubKeyHash pkh <- arbitrary
    pure (ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh))

instance Coarbitrary ArbitraryPaymentPubKeyHash where
  coarbitrary (ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh)) =
    coarbitrary (ArbitraryPubKeyHash pkh)

-- | Ensure that the generator only produces values for which the Kleisli arrow
-- | 'hits'. If the Kleisli arrow never 'hits', this will loop forever.
suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap gen arrow = sized $ tailRecM go
  where
  go :: Int -> Gen (Step Int b)
  go size = do
    x <- resize size gen
    pure $ case arrow x of
      Nothing -> Loop (2 * size)
      Just y -> Done y

-- | Generator wrapper for BigNum
newtype ArbitraryBigNum = ArbitraryBigNum BigNum

derive newtype instance Eq ArbitraryBigNum

derive newtype instance Ord ArbitraryBigNum

-- derive newtype instance Ring ArbitraryBigNum

derive instance Generic ArbitraryBigNum _

instance Show ArbitraryBigNum where
  show = genericShow

instance Arbitrary ArbitraryBigNum where
  arbitrary = (BigNum.fromInt >>> ArbitraryBigNum) <$> arbitrary

instance Coarbitrary ArbitraryBigNum where
  coarbitrary (ArbitraryBigNum x) =
    coarbitrary (BigNum.toString x)

-- | Generator wrapper for BigNum
newtype ArbitraryBigInt = ArbitraryBigInt BigInt.BigInt

derive newtype instance Eq ArbitraryBigInt

derive newtype instance Ord ArbitraryBigInt

-- derive newtype instance Ring ArbitraryBigInt

derive instance Generic ArbitraryBigInt _

instance Show ArbitraryBigInt where
  show = genericShow

instance Arbitrary ArbitraryBigInt where
  arbitrary = (BigInt.fromInt >>> ArbitraryBigInt) <$> arbitrary

instance Coarbitrary ArbitraryBigInt where
  coarbitrary (ArbitraryBigInt x) =
    coarbitrary (BigInt.toString x)

instance Semiring ArbitraryBigInt where
  add (ArbitraryBigInt x) (ArbitraryBigInt y) = ArbitraryBigInt (x + y)
  zero = ArbitraryBigInt (BigInt.fromInt 0)
  mul (ArbitraryBigInt x) (ArbitraryBigInt y) = ArbitraryBigInt (x * y)
  one = ArbitraryBigInt (BigInt.fromInt 1)

instance Ring ArbitraryBigInt where
  sub (ArbitraryBigInt x) (ArbitraryBigInt y) = ArbitraryBigInt (x - y)

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
  arbitrary = ArbitraryTransactionHash <$> arbitrary

instance Coarbitrary ArbitraryTransactionHash where
  coarbitrary (ArbitraryTransactionHash x) =
    coarbitrary x

-- | Generator wrapper for TransactionInput
newtype ArbitraryTransactionInput = ArbitraryTransactionInput TransactionInput

derive newtype instance Eq ArbitraryTransactionInput

derive newtype instance Ord ArbitraryTransactionInput

derive instance Generic ArbitraryTransactionInput _

instance Show ArbitraryTransactionInput where
  show = genericShow

instance Arbitrary ArbitraryTransactionInput where
  arbitrary = ArbitraryTransactionInput <$> do
    ArbitraryTransactionHash transactionId <- arbitrary
    ArbitraryUInt index <- arbitrary
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
newtype NonNegative (a :: Type) = NonNegative a

derive instance Functor NonNegative

derive newtype instance (Eq a) => Eq (NonNegative a)

derive newtype instance (Ord a) => Ord (NonNegative a)

derive instance Generic (NonNegative a) _

instance (Show a) => Show (NonNegative a) where
  show = genericShow

instance (Arbitrary a, Ord a, Ring a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> do
    x <- arbitrary
    pure $ abs x

instance (Coarbitrary a) => Coarbitrary (NonNegative a) where
  coarbitrary (NonNegative x) = coarbitrary x

-- | Generator wrapper for number-like types that should always be positive
-- | (that is, must be larger than zero).
newtype Positive (a :: Type) = Positive a

derive instance Functor Positive

derive newtype instance (Eq a) => Eq (Positive a)

derive newtype instance (Ord a) => Ord (Positive a)

derive instance Generic (Positive a) _

instance (Show a) => Show (Positive a) where
  show = genericShow

instance (Arbitrary a, Ord a, Semiring a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> do
    x <- suchThat arbitrary (_ > zero)
    pure x

instance (Coarbitrary a) => Coarbitrary (Positive a) where
  coarbitrary (Positive x) = coarbitrary x

-- helpers

genByteInt :: Gen Int
genByteInt = chooseInt 0 255

noncedAlwaysFail ::
  forall (a :: Type).
  ToData a =>
  a ->
  PlutusScript
noncedAlwaysFail x = unsafePartial $ fromJust do
  script <- hush
    (decodeAeson (encodeAeson [ "4701000022224981", "PlutusV2" ]))
  applyArgs script [ toData x ] # hush
