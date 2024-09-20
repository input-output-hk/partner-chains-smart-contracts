module TrustlessSidechain.Effects.Wallet
  ( WALLET
  , WalletF
  , getNetworkId
  , getWalletAddresses
  , getWalletUtxos
  , handleWalletLive
  , handleWalletWith
  , ownPaymentPubKeyHashes
  ) where

import Contract.Prelude

import Cardano.Types
  ( Address
  , NetworkId
  )
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Contract.Address as Address
import Contract.Utxos (UtxoMap)
import Contract.Wallet as Wallet
import Effect.Aff (Error)
import Run (Run, interpret, on, send)
import Run as Run
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT, withTry)
import TrustlessSidechain.Effects.Errors.Context
  ( ErrorContext(ErrorContext)
  , ErrorContextType(Wallet)
  )
import TrustlessSidechain.Effects.Errors.Parser
  ( parseDefaultError
  , parseFromError
  )
import TrustlessSidechain.Error (OffchainError)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

data WalletF a
  = GetNetworkId (NetworkId -> a)
  | GetWalletAddresses (Array Address -> a)
  | OwnPaymentPubKeyHashes (Array PaymentPubKeyHash -> a)
  | GetWalletUtxos (Maybe UtxoMap -> a)

derive instance functorWalletF :: Functor WalletF

type WALLET r = (wallet :: WalletF | r)

_wallet :: Proxy "wallet"
_wallet = Proxy

handleWalletWith ::
  forall r. (WalletF ~> Run r) -> Run (WALLET + r) ~> Run r
handleWalletWith f = interpret (on _wallet f send)

getNetworkId :: forall r. Run (WALLET + r) NetworkId
getNetworkId = Run.lift _wallet (GetNetworkId identity)

getWalletAddresses :: forall r. Run (WALLET + r) (Array Address)
getWalletAddresses = Run.lift _wallet (GetWalletAddresses identity)

ownPaymentPubKeyHashes :: forall r. Run (WALLET + r) (Array PaymentPubKeyHash)
ownPaymentPubKeyHashes = Run.lift _wallet (OwnPaymentPubKeyHashes identity)

getWalletUtxos :: forall r. Run (WALLET + r) (Maybe UtxoMap)
getWalletUtxos = Run.lift _wallet (GetWalletUtxos identity)

handleWalletLive ::
  forall r. WalletF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleWalletLive = case _ of
  GetNetworkId f -> f <$> withTry
    (fromError "getNetworkId: ")
    Address.getNetworkId
  GetWalletAddresses f -> f <$> withTry
    (fromError "getWalletAddress: ")
    Wallet.getWalletAddresses
  OwnPaymentPubKeyHashes f -> f <$> withTry
    (fromError "ownPaymentPubKeyHashes: ")
    Wallet.ownPaymentPubKeyHashes
  GetWalletUtxos f -> f <$> withTry
    (fromError "getWalletUtxos: ")
    Wallet.getWalletUtxos
  where
  fromError :: String -> Error -> OffchainError
  fromError ctx = parseFromError parseDefaultError
    (Just (ErrorContext Wallet ctx))
