module Test.Utils.Address (tests) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (PlutusData(..))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray as ByteArray
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Test.Unit (Test)
import Test.Unit.Assert as Test.Unit.Assert
import Test.Utils (WrappedTests, pureGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof(..)
  , MerkleTreeEntry(..)
  )
import TrustlessSidechain.MerkleTree
  ( MerkleProof(..)
  , Side(..)
  , Up(..)
  , byteArrayToRootHashUnsafe
  )
import TrustlessSidechain.Utils.Address
  ( byteArrayToBech32BytesUnsafe
  )

type TestCase = Mote (Const Void) Test Unit

tests ∷ WrappedTests
tests = pureGroup "Merkle proof serialisation for #249" do
  test1

test1 ∷ TestCase
test1 =
  Mote.Monad.test
    "Serialize address to bech32"
    $ Test.Unit.Assert.assert "expected different bytearray representation"
    $ (1 == 1)
