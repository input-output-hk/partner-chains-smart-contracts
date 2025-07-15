{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.TrustlessSidechain.Transactions where

import Data.String (fromString)
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.Transactions.Versioning (versioningTests)

-- | Top-level transaction tests, delegating to contract-specific modules
--
-- @since 0.1
tests :: TestTree
tests =
  testGroup
    "Transaction tests"
    [ versioningTests
    ]
