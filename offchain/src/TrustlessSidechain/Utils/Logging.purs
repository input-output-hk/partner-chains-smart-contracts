module TrustlessSidechain.Utils.Logging
  ( Environment
  , environment
  , fileLogger
  , mkReport
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
  -- | Error type for internal errors, with error information to developers and maintainers
  = InternalError InternalError
  -- | Error occured because of an invalid input, with informaton to the end-user
  | InvalidInputError String

derive instance Generic OffchainError _

instance Show OffchainError where
  show = genericShow

-- | Error type for internal errors, with error information to developers and maintainers
data InternalError
  = NotFoundAddress String
  | NotFoundUtxo String
  | NotFoundOwnPubKeyHash
  | NotFoundOwnAddress
  | InvalidScript String
  | ConversionError String
  | InvalidData String
  | BuildTxError ScriptLookups.MkUnbalancedTxError
  | BalanceTxError Transaction.BalanceTxError
  | Other String

derive instance Generic InternalError _

instance Show InternalError where
  show = genericShow

-- | builds a unified look for error messages by giving them more structure.
-- | this function is used to instantiate a message formatter as so:
-- | ```purescript
-- | mkErr ∷ String -> String
-- | mkErr = mkReport "MyModule" "myFunction"
-- |
-- | mkErr "this is an error message."
-- | ```
-- TODO: Remove this whole thing once we can trace the errors better in PureScript (CTL v6?)
--       https://github.com/Plutonomicon/cardano-transaction-lib/pull/1507
mkReport ∷ String → String → OffchainError → String
mkReport mod fun appErr = mod <> "." <> fun <> ": " <> show appErr

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
