module TrustlessSidechain.DelegationRegistration where

import Prelude

import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

getDelegationRegistration ∷
  ∀ r.
  ByteArray →
  ByteArray →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { transactionId ∷ ByteArray }
getDelegationRegistration _stakePubKeyHash _partnerChainWallet = do
  pure { transactionId: hexToByteArrayUnsafe "" }
