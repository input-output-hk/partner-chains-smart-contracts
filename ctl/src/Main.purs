module Main (main) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType(..))
import Contract.Address (NetworkId(TestnetId), ownPaymentPubKeyHash)
import Contract.Monad
  ( Aff
  , ConfigParams(..)
  , DefaultContractConfig
  , Host
  , mkContractConfig
  , runContract
  )
import Contract.Monad as Contract.Monad
import Contract.Wallet (Wallet, privateKeyFromBytes)
import Data.Log.Level (LogLevel(Trace))
import Data.UInt (fromInt)
import Effect.Exception (error, throwException)
import Node.Path (FilePath)
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import RunFuelMintingPolicy (FuelParams(Mint), runFuelMP)
import SidechainParams (SidechainParams(SidechainParams))
import Wallet (mkKeyWallet)
import Wallet.Key (PrivatePaymentKey(..))
import Wallet.KeyFile (keyFromFile)

main :: Effect Unit
main =
  Contract.Monad.launchAff_ do
    pkey <- privatePaymentKeyFromFile
      "/Users/gergo/Dev/cardano/testnets/addresses/server.skey"

    let
      wallet = mkKeyWallet pkey Nothing
      (config :: ConfigParams ()) = ConfigParams
        { ogmiosConfig: defaultOgmiosWsConfig
        , datumCacheConfig: defaultDatumCacheWsConfig
        , ctlServerConfig: defaultServerConfig
        , networkId: TestnetId
        , logLevel: Trace
        , extraConfig: {}
        , wallet: Just wallet
        }

    cconf <- mkContractConfig config
    runContract cconf
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

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  bytes <- keyFromFile filePath PaymentSigningKeyShelleyed25519
  liftM (error "Unable to decode private payment key")
    $ PrivatePaymentKey
    <$> privateKeyFromBytes (wrap bytes)
