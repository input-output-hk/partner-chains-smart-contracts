{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import PlutusTx.Code (CompiledCode, sizePlc)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest, testFailed, testPassed)
import TrustlessSidechain.Utils (verifyMultisig)
import Type.Reflection (Typeable)
import Prelude qualified

main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsUnder "verifyMultisig" ("new", newVerify) ("old", oldVerify)
    ]

-- Helpers

fitsUnder ::
  forall (a :: Type).
  (Typeable a) =>
  Prelude.String ->
  (Prelude.String, CompiledCode a) ->
  (Prelude.String, CompiledCode a) ->
  TestTree
fitsUnder name test target = singleTest name $ SizeComparisonTest test target

data SizeComparisonTest (a :: Type)
  = SizeComparisonTest
      (Prelude.String, CompiledCode a)
      (Prelude.String, CompiledCode a)

instance (Typeable a) => IsTest (SizeComparisonTest a) where
  run _ (SizeComparisonTest (mName, mCode) (tName, tCode)) _ = do
    let tEstimate = sizePlc tCode
    let mEstimate = sizePlc mCode
    let diff = tEstimate - mEstimate
    Prelude.pure $ case Prelude.signum diff of
      (-1) -> testFailed . renderFailed (tName, tEstimate) (mName, mEstimate) $ diff
      0 -> testPassed . renderEstimates (tName, tEstimate) $ (mName, mEstimate)
      _ -> testPassed . renderExcess (tName, tEstimate) (mName, mEstimate) $ diff
  testOptions = Tagged []

renderFailed ::
  (Prelude.String, Integer) ->
  (Prelude.String, Integer) ->
  Integer ->
  Prelude.String
renderFailed tData mData diff =
  renderEstimates tData mData
    <> "Exceeded by: "
    <> Prelude.show diff

renderEstimates ::
  (Prelude.String, Integer) ->
  (Prelude.String, Integer) ->
  Prelude.String
renderEstimates (tName, tEstimate) (mName, mEstimate) =
  "Target: " <> tName <> "; size " <> Prelude.show tEstimate <> "\n"
    <> "Measured: "
    <> mName
    <> "; size "
    <> Prelude.show mEstimate
    <> "\n"

renderExcess ::
  (Prelude.String, Integer) ->
  (Prelude.String, Integer) ->
  Integer ->
  Prelude.String
renderExcess tData mData diff =
  renderEstimates tData mData
    <> "Remaining headroom: "
    <> Prelude.show diff

{-# INLINEABLE originalVerifyMultisig #-}
originalVerifyMultisig ::
  [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
originalVerifyMultisig pubKeys threshold message signatures =
  let go :: Integer -> [BuiltinByteString] -> [BuiltinByteString] -> Bool
      go !signed !pubs !sigs =
        let ok = signed >= threshold
         in ok
              || ( case pubs of
                    [] -> ok
                    pub : pubs' ->
                      case sigs of
                        [] -> ok
                        sig : sigs' ->
                          if verifyEcdsaSecp256k1Signature pub message sig
                            then -- the public key and signature match, so
                            -- we move them both forward..
                              go (signed + 1) pubs' sigs'
                            else -- otherwise, they don't match so since
                            -- `sigs` is essentially a subsequence of
                            -- `pubs`, we move only `pubs` forward
                            -- since a later key should match with
                            -- `sig`.
                              go signed pubs' sigs
                 )
   in go 0 pubKeys signatures

oldVerify ::
  CompiledCode ([BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool)
oldVerify = $$(compile [||originalVerifyMultisig||])

newVerify ::
  CompiledCode ([BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool)
newVerify = $$(compile [||verifyMultisig||])
