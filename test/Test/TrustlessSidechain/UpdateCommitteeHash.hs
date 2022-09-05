module Test.TrustlessSidechain.UpdateCommitteeHash (test) where

import Crypto.Secp256k1 (SecKey)
import Crypto.Secp256k1 qualified as Secp256k1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Maybe qualified as Maybe
import Ledger.Crypto (Signature (Signature, getSignature))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Class qualified as Class
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import TrustlessSidechain.OffChain.Types (GenesisHash (GenesisHash), SidechainParams (SidechainParams, chainId, genesisHash), SidechainPubKey (SidechainPubKey))
import TrustlessSidechain.OnChain.UpdateCommitteeHash (UpdateCommitteeMessage (UpdateCommitteeMessage, newCommitteePubKeys, sidechainEpoch, sidechainParams))

-- | 'test' includes tests for the UpdateCommitteeHash endpoint.
test :: TestTree
test =
  Tasty.testGroup
    "UpdateCommitteeHash"
    [ HUnit.testCase "Kasper's cool test signature generation test case" $
        {-
        The test case is as follows...

        > From Kasper:
        >  I prepared a single test message together with a signature for the
        >     (updateCommitteeHash)[https://github.com/mlabs-haskell/trustless-sidechain#6-update--committee-hash]
        > message:
        > nextCommitteePublicKeys =[
        >     d6df5ba73496870c246217feb6dae3cd6840fb9b1f86cd526ae69b0fedf433c7e4bfea2d39c07da054f5074cfe9ce245a84abcbed8b66c9142beedec4f2483cf ,
        >     5234df2d3ec47156a7af789e25c1e6352e0bbf0bbc6a5aace1c9f3fef91d29edee44fe98a62a95114f24fe81e117911ef439292dc60a3df455c619cd0531f12d
        > ]
        > SidechainParams = {
        >     chainId = 78
        >     genesisHash = 1a9f9b8967b06ecf3b073f028ee2c2ffabe20cfe27c6eb154ccffbcbd8c45df4
        > }
        >     sidechainEpoch = 123
        >     private ecdsa key to sign the message
        >         c0fff366fb01ff80008a005701ff5d804527190021e90051c200ff802f9e467f
        >     expected signature
        >         6ce97c0cf3996eb032a0abc9a2beeb8922eaa6e15278ab31bbeec92af902e52514a21d7c6462e6eb86e096684c371c8b741b90dfe5fb95c2da5cbeb69e32186b1c

        Some notes about this:

            - Everything is actually hexencoded, so internally, we want to
            decode the hex representation into the binary representation.

            - `1c` at the end of the expected signature should be removed. this
            extra byte is called the "recovery byte" that only exist on Kasper's end.

        -}
        let prv = unsafeToSidechainPrivKey "c0fff366fb01ff80008a005701ff5d804527190021e90051c200ff802f9e467f"
            uchm =
              UpdateCommitteeMessage
                { sidechainParams =
                    SidechainParams
                      { chainId = 78
                      , genesisHash = GenesisHash $ BuiltinByteString $ unsafeDecodeHex "1a9f9b8967b06ecf3b073f028ee2c2ffabe20cfe27c6eb154ccffbcbd8c45df4"
                      }
                , sidechainEpoch = 123
                , newCommitteePubKeys =
                    map SidechainPubKey $
                      sort -- recall the spec says this should be sorted lexicographically, so we sort these...
                      -- Apparently we use compressed form :^) of the
                      -- public keys....
                        [ BuiltinByteString $ unsafeDecodeHex "035234df2d3ec47156a7af789e25c1e6352e0bbf0bbc6a5aace1c9f3fef91d29ed"
                        , BuiltinByteString $ unsafeDecodeHex "03d6df5ba73496870c246217feb6dae3cd6840fb9b1f86cd526ae69b0fedf433c7"
                        ]
                        {-
                        -- Originally, the test case had the public
                        -- keys written in decompressed form, but
                        -- that's not what Kasper was doing on his
                        -- side! His side automagically converted it
                        -- into compressed form (how cool!)
                        [ "d6df5ba73496870c246217feb6dae3cd6840fb9b1f86cd526ae69b0fedf433c7e4bfea2d39c07da054f5074cfe9ce245a84abcbed8b66c9142beedec4f2483cf"
                        , "5234df2d3ec47156a7af789e25c1e6352e0bbf0bbc6a5aace1c9f3fef91d29edee44fe98a62a95114f24fe81e117911ef439292dc60a3df455c619cd0531f12d"
                        ]
                        -}
                }
            uchmBuiltinData = Class.toBuiltinData uchm
            uchmBuiltinDataCbor = Builtins.serialiseData uchmBuiltinData
            uchmBuiltinDataCborHashed = Builtins.blake2b_256 uchmBuiltinDataCbor

            sig =
              Signature
                { getSignature = Builtins.toBuiltin
                    . Secp256k1.getCompactSig
                    . Secp256k1.exportCompactSig
                    $ Secp256k1.signMsg prv $ case uchmBuiltinDataCborHashed of
                      BuiltinByteString bs -> Maybe.fromJust $ Secp256k1.msg bs
                }

            expectedSig =
              Signature
                { getSignature =
                    BuiltinByteString $
                      unsafeDecodeHex "6ce97c0cf3996eb032a0abc9a2beeb8922eaa6e15278ab31bbeec92af902e52514a21d7c6462e6eb86e096684c371c8b741b90dfe5fb95c2da5cbeb69e32186b"
                }
         in expectedSig HUnit.@=? sig
    ]

{- | 'unsafeDecodeHex' decodes a hex string (and throws an exception if unable
 to)
-}
unsafeDecodeHex :: ByteString -> ByteString
unsafeDecodeHex x = case Base16.decode x of
  Right x' -> x'
  Left err -> traceError $ decodeUtf8 $ Class.stringToBuiltinByteString err

{- | 'unsafeToSidechainPrivKey' converts a hexencoded sidechain private key
 into a 'SecKey'.

 This is essentially taken from [this
 link](https://github.com/mlabs-haskell/trustless-sidechain/blob/plutusv2/app/export-scripts/Main.hs#L238)
 with minor modifications to error handling:
-}
unsafeToSidechainPrivKey :: ByteString -> SecKey
unsafeToSidechainPrivKey hex =
  let bin = unsafeDecodeHex hex
   in case Secp256k1.secKey bin of
        Nothing -> traceError "error in 'unsafeToSidechainPrivKey'"
        Just sec -> sec
