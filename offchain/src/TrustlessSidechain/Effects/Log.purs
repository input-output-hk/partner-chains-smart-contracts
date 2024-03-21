module TrustlessSidechain.Effects.Log
  ( LOG
  , LogF
  , handleLogLive
  , handleLogWith
  , logInfo'
  , logWarn'
  ) where

import Contract.Prelude

import Contract.Log (logInfo', logWarn') as Contract
import Effect.Aff (Error)
import Run (Run, interpret, on, send)
import Run as Run
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT, withTry)
import TrustlessSidechain.Effects.Errors.Context
  ( ErrorContext(ErrorContext)
  , ErrorContextType(Log)
  )
import TrustlessSidechain.Effects.Errors.Parser
  ( parseDefaultError
  , parseFromError
  )
import TrustlessSidechain.Error (OffchainError)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

data LogF a
  = LogInfo' String a
  | LogWarn' String a

derive instance functorLogF ∷ Functor LogF

type LOG r = (log ∷ LogF | r)

_log ∷ Proxy "log"
_log = Proxy

handleLogWith ∷
  ∀ r. (LogF ~> Run r) → Run (LOG + r) ~> Run r
handleLogWith f = interpret (on _log f send)

logInfo' ∷
  ∀ r. String → Run (LOG + r) Unit
logInfo' msg = Run.lift _log
  (LogInfo' msg unit)

logWarn' ∷
  ∀ r. String → Run (LOG + r) Unit
logWarn' msg = Run.lift _log
  (LogWarn' msg unit)

handleLogLive ∷
  ∀ r. LogF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleLogLive =
  case _ of
    LogInfo' msg x → const x <$> withTry
      (fromError "logInfo': ")
      (Contract.logInfo' msg)
    LogWarn' msg x → const x <$> withTry
      (fromError "logWarn': ")
      (Contract.logWarn' msg)
  where
  fromError ∷ String → Error → OffchainError
  fromError ctx =
    parseFromError parseDefaultError (Just (ErrorContext Log ctx))
