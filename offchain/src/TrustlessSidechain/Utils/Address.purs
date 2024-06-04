-- | `Utils.Address` provides some utility functions for handling addresses.
module TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  , toScriptHash
  , toAddress
  , getCurrencyInfo
  , getScriptHashHex
  , getPlutusScriptHex
  , fromPaymentPubKeyHash
  , addressFromBech32Bytes
  ) where

import Contract.Prelude hiding (note)

import Cardano.AsCbor (decodeCbor)
import Cardano.Serialization.Lib
  ( toBytes
  )
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.PaymentCredential (PaymentCredential(PaymentCredential))
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.PlutusData (PlutusData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Data.Array as Array
import Run (Run)
import Run.Except (EXCEPT, note)
import Run.Except as Run
import TrustlessSidechain.Effects.Wallet
  ( WALLET
  , getNetworkId
  , getWalletAddresses
  )
import TrustlessSidechain.Effects.Wallet as Effect
import TrustlessSidechain.Error
  ( OffchainError
      ( NotFoundOwnPubKeyHash
      , InvalidAddress
      )
  )
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.Types (ScriptId)
import Type.Row (type (+))

-- | Return a single own payment pub key hash without generating warnings.
getOwnPaymentPubKeyHash ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r) PaymentPubKeyHash
getOwnPaymentPubKeyHash = do
  pubKeyHashes ← Effect.ownPaymentPubKeyHashes
  Run.note NotFoundOwnPubKeyHash $ Array.head pubKeyHashes

-- | Return a single own wallet address without generating warnings.
getOwnWalletAddress ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r) Address
getOwnWalletAddress = do
  addresses ← getWalletAddresses
  Run.note NotFoundOwnPubKeyHash $ Array.head addresses

-- | Convert Address to ValidatorHash, raising an error if an address does not
-- | represent a script.
toScriptHash ∷ ∀ r. Address → Run (EXCEPT OffchainError + r) ScriptHash
toScriptHash addr = do
  PaymentCredential c ←
    note
      (InvalidAddress "Cannot convert Address to ScriptHash" addr)
      $ Address.getPaymentCredential addr

  case c of
    ScriptHashCredential sh → pure sh
    _ → Run.throw $ InvalidAddress "Address does not represent a script" addr

-- | Convert ValidatorHash to Address in the current network, raising an error
-- | if the hash is not valid.
toAddress ∷ ∀ r. ScriptHash → Run (EXCEPT OffchainError + WALLET + r) Address
toAddress sh = do
  netId ← getNetworkId
  pure $
    Address.mkPaymentAddress netId (wrap $ ScriptHashCredential sh) Nothing

-- | `getCurrencyInfo` returns minting policy and currency symbol of a given
-- | script.  Requires providing parameters of that script.
getCurrencyInfo ∷
  ∀ r.
  ScriptId →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) CurrencyInfo
getCurrencyInfo scriptId params = do
  plutusScript ← mkMintingPolicyWithParams scriptId params

  pure $
    { mintingPolicy: plutusScript
    , currencySymbol: PlutusScript.hash plutusScript
    }

getPlutusScriptHex ∷ Partial ⇒ PlutusScript → String
getPlutusScriptHex = ByteArray.byteArrayToHex <<< toBytes <<< PlutusScript.toCsl

getScriptHashHex ∷ ScriptHash → String
getScriptHashHex = ByteArray.byteArrayToHex <<< toBytes <<< unwrap

fromPaymentPubKeyHash ∷ NetworkId → PaymentPubKeyHash → Address
fromPaymentPubKeyHash networkId pkh = Address.mkPaymentAddress networkId
  (wrap $ PubKeyHashCredential $ unwrap pkh)
  Nothing

addressFromBech32Bytes ∷ ByteArray → Maybe Address
addressFromBech32Bytes = decodeCbor <<< wrap

