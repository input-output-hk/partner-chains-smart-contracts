module TrustlessSidechain.Utils.Logging
  ( Environment
  , environment
  , fileLogger
  ) where

import Contract.Prelude

import Contract.Config (Message)
import Data.Log.Formatter.JSON (jsonFormatter)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (appendTextFile)
import Node.Process (stdoutIsTTY)

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
