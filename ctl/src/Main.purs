module Main (main) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId), ownPaymentPubKeyHash)
import Contract.Monad (ConfigParams, runContract)
import Contract.Monad as Contract.Monad
import Data.Log.Level (LogLevel(Trace))
import Effect.Exception (error, throwException)
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import RunFuelMintingPolicy (FuelParams(Mint), runFuelMP)
import SidechainParams (SidechainParams(SidechainParams))
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
                  { chainId: "11"
                  , genesisHash: "11"
                  , genesisMint: Nothing
                  }
              in
                runFuelMP params scParams
      )
