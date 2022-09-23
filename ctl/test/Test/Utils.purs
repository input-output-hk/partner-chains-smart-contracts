module Test.Utils (toTxIn) where

import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Data.UInt as UInt

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
    , index: UInt.fromInt txIdx
    }
