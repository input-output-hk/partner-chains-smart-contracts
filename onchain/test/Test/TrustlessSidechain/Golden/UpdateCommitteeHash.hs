module Test.TrustlessSidechain.Golden.UpdateCommitteeHash where

import Data.String
import GHC.Num (fromInteger)
import Plutus.V1.Ledger.Api (TxOutRef (TxOutRef))
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.UpdateCommitteeHash (
  InitCommitteeHashMint (..),
 )

tests :: TestTree
tests =
  testGroup
    "Golden tests for UpdateCommitteeHash module"
    [ dataEncoderGoldenTest "InitCommitteeHashMint" sampleInitCommitteeHashMint
    ]

sampleInitCommitteeHashMint :: InitCommitteeHashMint
sampleInitCommitteeHashMint =
  InitCommitteeHashMint
    { icTxOutRef = sampleTxOutRef
    }

sampleTxOutRef :: TxOutRef
sampleTxOutRef = TxOutRef "e41c9b57841e582c207bb68d5e9736fb48c7af5f1ec29ade00692fa5e0e47efa" 4
