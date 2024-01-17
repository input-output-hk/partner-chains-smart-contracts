module Test.TrustlessSidechain.Golden.DistributedSet (tests) where

import Data.String
import GHC.Num (fromInteger)
import Plutus.V1.Ledger.Api (TxOutRef (TxOutRef))
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.DistributedSet (
  Ds (..),
  DsConfDatum (..),
  DsConfMint (..),
  DsDatum (..),
  DsKeyMint (..),
  Ib (..),
  Node (..),
 )

tests :: TestTree
tests =
  testGroup
    "Golden tests for DistributedSet module"
    [ dataEncoderGoldenTest "Ds" sampleDs
    , dataEncoderGoldenTest "DsDatum" sampleDsDatum
    , dataEncoderGoldenTest "Node" sampleNode
    , dataEncoderGoldenTest "DsConfDatum" sampleDsConfDatum
    , dataEncoderGoldenTest "DsConfMint" sampleDsConfMint
    , dataEncoderGoldenTest "DsKeyMint" sampleDsKeyMint
    , dataEncoderGoldenTest "IbUnit" sampleIbUnit
    ]

sampleDs :: Ds
sampleDs =
  Ds
    { identitySymbol = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    }

sampleDsDatum :: DsDatum
sampleDsDatum =
  DsDatum
    { next = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    }

sampleNode :: Node
sampleNode =
  Node
    { key = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , next = "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
    }

sampleDsConfDatum :: DsConfDatum
sampleDsConfDatum =
  DsConfDatum
    { keyPolicy = "ba057436091a591a90329bd86e0e1617ac05cff039fb594b577a4084"
    , fuelPolicy = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    }

sampleDsConfMint :: DsConfMint
sampleDsConfMint =
  DsConfMint
    { txOutRef = sampleTxOutRef
    }

sampleDsKeyMint :: DsKeyMint
sampleDsKeyMint =
  DsKeyMint
    { validatorHash = "ba057436091a591a90329bd86e0e1617ac05cff039fb594b577a4084"
    , confCurrencySymbol = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    }

sampleTxOutRef :: TxOutRef
sampleTxOutRef = TxOutRef "e41c9b57841e582c207bb68d5e9736fb48c7af5f1ec29ade00692fa5e0e47efa" 4

sampleIbUnit :: Ib ()
sampleIbUnit =
  Ib ((), ())
