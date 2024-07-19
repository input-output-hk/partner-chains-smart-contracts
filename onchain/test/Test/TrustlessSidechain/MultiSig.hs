{-# LANGUAGE UndecidableInstances #-}

module Test.TrustlessSidechain.MultiSig (test) where

import Control.Applicative ((<|>))
import Control.Monad (fail, guard)
import Crypto.Secp256k1 qualified as SECP

import Data.List qualified as List
import Data.String qualified as HString
import Data.Word (Word8)
import GHC.Exts (fromListN)
import GHC.Real (fromRational)
import PlutusLedgerApi.V2 (LedgerBytes (LedgerBytes))
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  classify,
  forAllShrinkShow,
  label,
  liftShrink,
  property,
 )
import Test.QuickCheck.Extra qualified as QCExtra
import Test.QuickCheck.Gen as Gen
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import TrustlessSidechain.CommitteePlainATMSPolicy qualified as CommitteePlainATMSPolicy
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

test :: TestTree
test =
  testGroup
    "MultiSig"
    [ unitTests
    , adjustOption go propertyTests
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = const 10000

verifyMultisig :: [LedgerBytes] -> Integer -> LedgerBytes -> [LedgerBytes] -> Bool
verifyMultisig = CommitteePlainATMSPolicy.verifyPlainMultisig verifyEcdsaSecp256k1Signature

unitTests :: TestTree
unitTests =
  let
    -- blake2b_256 "msg"
    msg = "96a7ed8861db0abc006f473f9e64687875f3d9df8e723adae9f53a02b2aec378"
    -- skey1 = genKeyDSIGN . mkSeedFromBytes $ blake2b_256 "321"
    -- skey2 = ... "123"
    -- rawSerialiseVerKeyDSIGN $ deriveVerKeyDSIGN skeyN
    key1 = "031fcd8941891fa49e0e1710a83c552f5fa528a9e279007c4d700d5105ef9ec3fa"
    key2 = "037df15cfd5ca49c1ed1b04d23f27979840e47bec1fed3c2251adf4d2a0262ed48"
    -- rawSerialiseSigDSIGN $ signDSIGN () msg skeyN
    sig1 = "947b717935180c9a4e69903249ab1a0b94e549018ca3d124098bf2488bcc931478ee252fb23ec15bbeb8e5d837d6eb99e164d87420848acff6369ad1852f7446"
    sig2 = "bea04eef29ee7e6a020b18180de2715c387473132167e9646d0a6594cd7a27074ddadae2cdd13899395fa12e08393176d3640359fedc66501fd82589c9f953b8"
   in
    testGroup
      "Unit tests"
      [ testCase "0 threshold"
          $ verifyMultisig [] 0 msg []
          @?= True
      , testCase "not eq"
          $ verifyMultisig [key1] 1 msg [sig2]
          @?= False
      , testCase "1-1"
          $ verifyMultisig [key1] 1 msg [sig1]
          @?= True
      , testCase "2-1"
          $ verifyMultisig [key1, key2] 1 msg [sig2]
          @?= True
      , testCase "1-2"
          $ verifyMultisig [key2] 1 msg [sig1, sig2]
          @?= False
      , -- this test is malformed, and hence should be 'False'.. while
        -- it is a valid signature, it doesn't satisfy the
        -- preconditions of the function
        testCase "attemptDuplicatePubkey"
          $ verifyMultisig [key1, key1] 2 msg [sig1]
          @?= False
      , testCase "attemptDuplicateSigs"
          $ verifyMultisig [key1] 2 msg [sig1, sig1]
          @?= False
      ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "verifies with sufficient signatures" sufficientVerification
    , testProperty "fails to verify with unsufficient signatures" insufficientVerification
    ]

-- Properties

sufficientVerification :: Property
sufficientVerification =
  forAllShrinkShow arbitrary shrink showSufficientVerification
    $ \(SufficientVerification pubKeys enough message signatures) ->
      let sigLen = TSPrelude.length signatures
          densityRatio :: TSPrelude.Double =
            TSPrelude.fromIntegral enough TSPrelude./ TSPrelude.fromIntegral sigLen
       in classify (densityRatio TSPrelude.< 0.5) "sparse (less than half signatures needed)"
            . classify (densityRatio TSPrelude.>= 0.5) "dense (at least half signatures neeeded)"
            . property
            $ verifyMultisig pubKeys enough message signatures

insufficientVerification :: Property
insufficientVerification =
  forAllShrinkShow arbitrary shrink showInsufficientVerification
    $ \(InsufficientVerification pubKeys enough message signatures) ->
      let sigLen = TSPrelude.fromIntegral . TSPrelude.length $ signatures
       in label ("shortfall of " <> TSPrelude.show (enough - sigLen))
            . property
            . TSPrelude.not
            $ verifyMultisig pubKeys enough message signatures

-- Helpers

data SufficientVerification
  = SufficientVerification
      [LedgerBytes]
      Integer
      LedgerBytes
      [LedgerBytes]

instance Arbitrary SufficientVerification where
  arbitrary = do
    privKeys <- Gen.listOf1 $ QCExtra.suchThatMap (Gen.vectorOf 32 arbitrary) mkPrivKey
    -- need a function that lifts IO Ctx to
    let
      {-# NOINLINE ctx #-}
      ctx = unsafePerformIO SECP.createContext
    let pubKeys = TSPrelude.fmap (SECP.derivePubKey ctx) privKeys
    (message, messageBS) <- QCExtra.suchThatMap (Gen.vectorOf 32 arbitrary) $ \bytes -> do
      let bs = fromListN 32 bytes
      msg <- SECP.msg bs
      TSPrelude.pure (msg, bs)
    let f = signWithKey ctx
    signatures <-
      TSPrelude.fmap (TSPrelude.fmap (`f` message))
        . QCExtra.suchThatMap (QCExtra.sublistOf privKeys)
        $ \pkSubs -> do
          guard (TSPrelude.not . List.null $ pkSubs)
          TSPrelude.pure pkSubs
    enough <- TSPrelude.fmap TSPrelude.fromIntegral . Gen.chooseInt $ (1, TSPrelude.length signatures)
    let pubKeyBytes = TSPrelude.fmap (LedgerBytes . toBuiltin . SECP.exportPubKey ctx True) pubKeys
    let messageBBS = LedgerBytes $ toBuiltin messageBS
    TSPrelude.pure . SufficientVerification pubKeyBytes enough messageBBS $ signatures
  shrink (SufficientVerification pks enough msg sigs) = do
    enough' <- shrink enough
    guard (enough' TSPrelude.> 0)
    sigs' <- liftShrink (const []) sigs
    let len = TSPrelude.fromIntegral . TSPrelude.length $ sigs'
    -- Ensure that we always have at least as many signatures as we need
    guard (len TSPrelude.>= enough')
    TSPrelude.pure $ SufficientVerification pks enough' msg sigs'

showSufficientVerification :: SufficientVerification -> HString.String
showSufficientVerification (SufficientVerification pks enough msg sigs) =
  "Pubkeys: "
    <> TSPrelude.show pks
    <> "\n"
    <> "Pubkeys length: "
    <> TSPrelude.show (length pks)
    <> "\n"
    <> "Signatures: "
    <> TSPrelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> TSPrelude.show (length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> TSPrelude.show enough
    <> "\n"
    <> "Message: "
    <> TSPrelude.show msg

data InsufficientVerification
  = InsufficientVerification
      [LedgerBytes]
      Integer
      LedgerBytes
      [LedgerBytes]

-- We piggyback off SufficientVerification, then cull some signatures
instance Arbitrary InsufficientVerification where
  arbitrary = do
    SufficientVerification pks enough msg sigs <- arbitrary
    shortfall <- Gen.chooseInteger (1, enough)
    let sigs' = List.take (TSPrelude.fromIntegral $ enough - shortfall) sigs
    TSPrelude.pure $ InsufficientVerification pks enough msg sigs'
  shrink (InsufficientVerification pks enough msg sigs) =
    fewerSignatures <|> fewerNeeded
    where
      fewerSignatures :: [InsufficientVerification]
      fewerSignatures = do
        sigs' <- liftShrink (const []) sigs
        TSPrelude.pure $ InsufficientVerification pks enough msg sigs'
      fewerNeeded :: [InsufficientVerification]
      fewerNeeded = do
        enough' <- shrink enough
        guard (enough' TSPrelude.> 0)
        sigs' <- liftShrink (const []) sigs
        let len = TSPrelude.fromIntegral . TSPrelude.length $ sigs'
        guard (len TSPrelude.> enough)
        TSPrelude.pure $ InsufficientVerification pks enough' msg sigs'

showInsufficientVerification :: InsufficientVerification -> HString.String
showInsufficientVerification (InsufficientVerification pks enough msg sigs) =
  "Pubkeys: "
    <> TSPrelude.show pks
    <> "\n"
    <> "Pubkeys length: "
    <> TSPrelude.show (TSPrelude.length pks)
    <> "\n"
    <> "Signatures: "
    <> TSPrelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> TSPrelude.show (TSPrelude.length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> TSPrelude.show enough
    <> "\n"
    <> "Shortfall: "
    <> TSPrelude.show (enough - (TSPrelude.fromIntegral . TSPrelude.length $ sigs))
    <> "\n"
    <> "Message: "
    <> TSPrelude.show msg

mkPrivKey :: [Word8] -> Maybe SECP.SecKey
mkPrivKey = SECP.secKey . fromListN 32

signWithKey :: SECP.Ctx -> SECP.SecKey -> SECP.Msg -> LedgerBytes
signWithKey ctx sk =
  LedgerBytes
    . toBuiltin
    . (\(SECP.CompactSig sig) -> sig)
    . SECP.exportCompactSig ctx
    . SECP.signMsg ctx sk
