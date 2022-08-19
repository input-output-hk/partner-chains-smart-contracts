module Main (main) where

import Contract.Prelude

import CommitteCandidateValidator as CommitteCandidateValidator
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
                scParams = SidechainParams

                  { chainId: BigInt.fromInt 123
                  , genesisHash: "112233"
                  , genesisMint: Just
                      ( toTxIn
                          "4ff9fb6788e9d643d7bb75e5b1003fd0e0d316ffed826a6a5a6f935b14f3126f"
                          0
                      )
                  , genesisUtxo:
                      toTxIn
                        "aabbcc"
                        0
                  }

                -- mintP = Mint { amount: 10, recipient: pkh }
                -- burnP = Burn { amount: 1, recipient: "aabbcc" }
                -- regP = CommitteCandidateValidator.RegisterParams
                deregP = CommitteCandidateValidator.DeregisterParams
                  { sidechainParams: scParams
                  , spoPubKey: CommitteCandidateValidator.SidechainPubKey
                      "aabbcc"
                  }
              in
                do
                  -- runFuelMP mintP scParams
                  -- runFuelMP burnP scParams
                  -- CommitteCandidateValidator.register regP
                  CommitteCandidateValidator.deregister deregP
      )

toTxIn :: String -> Int -> TransactionInput
toTxIn txId txIdx = TransactionInput
  { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
  , index: UInt.fromInt txIdx
  }
