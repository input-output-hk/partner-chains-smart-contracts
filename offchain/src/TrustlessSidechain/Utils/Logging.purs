module TrustlessSidechain.Utils.Logging
  ( Environment
  , environment
  , fileLogger
  , OffchainError(..)
  , InternalError(..)
  ) where

import Contract.Prelude

import Contract.Config (Message)
import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction as Transaction
import Data.Log.Formatter.JSON (jsonFormatter)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (appendTextFile)
import Node.Process (stdoutIsTTY)

-- | Error raised from the off-chain code of the application
data OffchainError
  -- | Error type for internal errors, with information to developers and maintainers
  = InternalError InternalError
  -- | Error occured because of an invalid input, with informaton to the end-user
  | InvalidInputError String

derive instance Generic OffchainError _

instance Show OffchainError where
  show = genericShow

-- | Error type for internal errors, with error information to developers and maintainers
data InternalError
  -- | A UTxO that should exist (not given as user input) could not be found
  = NotFoundUtxo String
  -- | Own payment public key hashes cannot be found
  | NotFoundOwnPubKeyHash
  -- | Own address cannot be found
  | NotFoundOwnAddress
  -- | Invalid policy or validator script, conversion to currency symbol / validator hash failed
  | InvalidScript String
  -- | Invalid datum or redeemer, decoding errors
  | InvalidData String
  -- | Conversion of any data type (excluding datums and redeemers, use InvalidData for those)
  | ConversionError String
  -- | Error while building a transaction from lookups and constraints
  | BuildTxError ScriptLookups.MkUnbalancedTxError
  -- | Error while attempting to balance a transaction
  | BalanceTxError Transaction.BalanceTxError
  | Other String

derive instance Generic InternalError _

instance Show InternalError where
  show = genericShow

-- | The logging environment, may be used to parametrize functions and override
-- | their logging behaviour at runtime.
type Environment = { isTTY ∷ Boolean, logLevel ∷ LogLevel }

-- | The default logging environment
environment ∷ Environment
environment = { isTTY: stdoutIsTTY, logLevel: Info }

-- | Store all log levels in a file
fileLogger ∷ Message → Aff Unit
fileLogger m = do
  let filename = "./contractlog.json"
  appendTextFile UTF8 filename (jsonFormatter m <> "\n")
