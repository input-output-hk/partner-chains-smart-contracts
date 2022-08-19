module Main (main) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId), ownPaymentPubKeyHash)
import Contract.Monad (ConfigParams, runContract)
import Contract.Monad as Contract.Monad
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Trace))
import Data.UInt as UInt
import Effect.Exception (error, throwException)
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import RunFuelMintingPolicy (FuelParams(Mint, Burn), runFuelMP)
import SidechainParams (SidechainParams(SidechainParams))
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.Transaction (TransactionHash(..), TransactionInput(..))
import Wallet.Spec (PrivatePaymentKeySource(..), WalletSpec(..))

main :: Effect Unit
main =
  Contract.Monad.launchAff_ do

    let
      (config :: ConfigParams ()) =
        { ogmiosConfig: defaultOgmiosWsConfig
        , datumCacheConfig: defaultDatumCacheWsConfig
        , ctlServerConfig: defaultServerConfig
        , networkId: TestnetId
        , logLevel: Trace
        , extraConfig: {}
        , walletSpec: Just
            ( UseKeys
                ( PrivatePaymentKeyFile
                    "/Users/gergo/Dev/cardano/testnets/addresses/server.skey"

                )
                Nothing
            )
        , customLogger: Nothing
        }

    runContract config
      ( do
          maybePkh <- ownPaymentPubKeyHash
          case maybePkh of
            Nothing -> liftEffect $ throwException (error "Couldn't find own PKH")
            Just pkh ->
              let
                params = Mint { amount: 1, recipient: pkh }
                scParams = SidechainParams
                  { chainId: BigInt.fromInt 123
                  , genesisHash: "112233"
                  , genesisMint: Just
                      ( toTxIn
                          "2657d280a1f57b8b8995e3784c56e6087ea640ca2c2a3638c2af1c61ab515c46"
                          0
                      )
                  , genesisUtxo:
                      toTxIn
                        "aabbcc"
                        0
                  }
              in
                runFuelMP params scParams
      )

toTxIn :: String -> Int -> TransactionInput
toTxIn txId txIdx = TransactionInput
  { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
  , index: UInt.fromInt txIdx
  }
