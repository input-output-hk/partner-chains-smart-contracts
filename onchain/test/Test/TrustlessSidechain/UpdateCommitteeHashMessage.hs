{-# LANGUAGE TypeApplications #-}

module Test.TrustlessSidechain.UpdateCommitteeHashMessage (test) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Ledger (TxId (TxId), TxOutRef (TxOutRef))
import PlutusTx.Builtins (blake2b_256)
import PlutusTx.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TrustlessSidechain.Types (
  GenesisHash (GenesisHash),
  SidechainParams (SidechainParams),
  SidechainPubKey (SidechainPubKey),
  UpdateCommitteeHashMessage (UpdateCommitteeHashMessage),
 )
import TrustlessSidechain.UpdateCommitteeHash (serialiseUchm)

test :: TestTree
test = unitTests

unitTests :: TestTree
unitTests =
  let conv = (toBuiltin @ByteString) . Base16.decodeLenient
      genesisHash = GenesisHash (conv "e8118a6a0f2ea8447b2418b0301fa53fa97f95a042fc92edbd7eda9f809d9040")
      genesisUtxo = TxOutRef (TxId (conv "7247647315d327d4e56fd3fe62d45be1dc3a76a647c910e0048cca8b97c8df3e")) 0
      pubKeys =
        [ SidechainPubKey (conv "029ef0f8e7f2144461246f4d14772a34945069e0b746fc93d641f5cceb23dba760")
        , SidechainPubKey (conv "0374df74bd17e647fbea5ad07699ee36eae9247b2fb633f31223da66626e083272")
        , SidechainPubKey (conv "038b098f1a3ccb005419df63dc1ce954a3f9071e2f41aefece0f86ee991285b498")
        , SidechainPubKey (conv "03f16df0d21e2a447d820999c6b65794820cd1920cafccc3a7b83956f6148441ed")
        ]
      scParams = SidechainParams 78 genesisHash genesisUtxo 2 3
      epoch = 123
      previousMRH = conv "abababababababababababababababababababababababababababababababab"
      withoutPreviousMRH = UpdateCommitteeHashMessage scParams pubKeys Nothing epoch
      withPreviousMRH = UpdateCommitteeHashMessage scParams pubKeys (Just previousMRH) epoch

      actualCbor1 = serialiseUchm withoutPreviousMRH
      actualCbor2 = serialiseUchm withPreviousMRH
      expectedCbor1 = conv "d8799fd8799f184e5820e8118a6a0f2ea8447b2418b0301fa53fa97f95a042fc92edbd7eda9f809d9040d8799fd8799f58207247647315d327d4e56fd3fe62d45be1dc3a76a647c910e0048cca8b97c8df3eff00ff0203ff9f5821029ef0f8e7f2144461246f4d14772a34945069e0b746fc93d641f5cceb23dba76058210374df74bd17e647fbea5ad07699ee36eae9247b2fb633f31223da66626e0832725821038b098f1a3ccb005419df63dc1ce954a3f9071e2f41aefece0f86ee991285b498582103f16df0d21e2a447d820999c6b65794820cd1920cafccc3a7b83956f6148441edffd87a80187bff"
      expectedCbor2 = conv "d8799fd8799f184e5820e8118a6a0f2ea8447b2418b0301fa53fa97f95a042fc92edbd7eda9f809d9040d8799fd8799f58207247647315d327d4e56fd3fe62d45be1dc3a76a647c910e0048cca8b97c8df3eff00ff0203ff9f5821029ef0f8e7f2144461246f4d14772a34945069e0b746fc93d641f5cceb23dba76058210374df74bd17e647fbea5ad07699ee36eae9247b2fb633f31223da66626e0832725821038b098f1a3ccb005419df63dc1ce954a3f9071e2f41aefece0f86ee991285b498582103f16df0d21e2a447d820999c6b65794820cd1920cafccc3a7b83956f6148441edffd8799f5820ababababababababababababababababababababababababababababababababff187bff"
      actualHash1 = blake2b_256 expectedCbor1
      actualHash2 = blake2b_256 expectedCbor2
      expectedHash1 = conv "29851edc4a500fcb6b74597107c202e2a2d167b497c461a4717b1bb00cb3f70b"
      expectedHash2 = conv "cee6b4e5cbb9830a82d5b72cc3e8f75dd5ad7d23027b7e16245582048c7cb2b0"
   in testGroup
        "UpdateCommitteeHashMessage"
        [ testCase "cbor of message without previous merkle root hash" $ actualCbor1 @?= expectedCbor1
        , testCase "cbor of message with previous merkle root hash" $ actualCbor2 @?= expectedCbor2
        , testCase "blake32 hash of message without previous merkle root hash" $ actualHash1 @?= expectedHash1
        , testCase "blake32 hash of message with previous merkle root hash" $ actualHash2 @?= expectedHash2
        ]
