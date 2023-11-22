module TrustlessSidechain.Utils.Logging
  ( Environment
  , environment
  , fileLogger
  , LOG
  , LogEff
  , logStr
  , logMsg
  , runLogToConsole
  , runLogToFile
  ) where

import Contract.Prelude

import Contract.Config (Message)
import Data.Log.Formatter.JSON (jsonFormatter)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (appendTextFile)
import Node.Process (stdoutIsTTY)
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

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

-- | Representation of logging effect
data LogEff a
  = -- | Representation of logging a structured `Message`.
    LogMsg Message a
  | -- | Representation of logging a `String`.
    LogStr String a

derive instance Functor LogEff

-- | Type alias for convenient use with `+` type operator.
type LOG r = (log ∷ LogEff | r)

-- | A helper type `Proxy` for internal use.
_log ∷ Proxy "log"
_log = Proxy

-- | Log a structured `Message`.
logMsg ∷ ∀ r. Message → Run (LOG + r) Unit
logMsg msg = Run.lift _log (LogMsg msg unit)

-- | Log a `String`.
logStr ∷ ∀ r. String → Run (LOG + r) Unit
logStr str = Run.lift _log (LogStr str unit)

-- | A helper function used by the actual handler.  Logs to a file.
handleLogToFile ∷ ∀ r. String → LogEff ~> Run (AFF + r)
handleLogToFile fileName = case _ of
  LogMsg msg next → do
    Run.liftAff $ appendTextFile UTF8 fileName (jsonFormatter msg <> "\n")
    pure next

  LogStr str next → do
    Run.liftAff $ appendTextFile UTF8 fileName (str <> "\n")
    pure next

-- | A helper function used by the actual handler.  Logs to console.
handleLogToConsole ∷ ∀ r. LogEff ~> Run (AFF + r)
handleLogToConsole = case _ of
  LogMsg msg next → do
    Run.liftAff $ Console.log (jsonFormatter msg <> "\n")
    pure next

  LogStr str next → do
    Run.liftAff $ Console.log str
    pure next

-- | Handle `LOG` effect by logging to console.
runLogToConsole ∷ ∀ r. Run (LOG + AFF + r) ~> Run (AFF + r)
runLogToConsole = Run.interpret (Run.on _log handleLogToConsole Run.send)

-- | Handle `LOG` effect by logging to a file.
runLogToFile ∷ ∀ r. String → Run (LOG + AFF + r) ~> Run (AFF + r)
runLogToFile fileName =
  Run.interpret (Run.on _log (handleLogToFile fileName) Run.send)
