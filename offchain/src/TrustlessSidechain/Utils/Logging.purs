module TrustlessSidechain.Utils.Logging
  ( Environment
  , environment
  , fileLogger
  , LOG
  , LogEff
  , logStr
  , logMsg
  , runLog
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

data LogEff a
  = LogMsg Message a
  | LogStr String a

derive instance functorLogF ∷ Functor LogEff

type LOG r = (log ∷ LogEff | r)

_log = Proxy ∷ Proxy "log"

logMsg ∷ ∀ r. Message → Run (LOG + r) Unit
logMsg msg = Run.lift _log (LogMsg msg unit)

logStr ∷ ∀ r. String → Run (LOG + r) Unit
logStr str = Run.lift _log (LogStr str unit)

handleLog ∷ ∀ r. LogEff ~> Run (AFF + r)
handleLog = case _ of
  LogMsg msg next → do
    Run.liftAff $ appendTextFile UTF8 "./contractlog.json"
      (jsonFormatter msg <> "\n")
    pure next

  LogStr str next → do
    Run.liftAff $ Console.log str
    pure next

runLog ∷ ∀ r. Run (AFF + LOG + r) ~> Run (AFF + r)
runLog = Run.interpret (Run.on _log handleLog Run.send)
