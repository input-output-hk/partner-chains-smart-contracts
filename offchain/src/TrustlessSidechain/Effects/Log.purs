module TrustlessSidechain.Effects.Log
  ( LOG
  , LogF
  , handleLogLive
  , handleLogWith
  , logInfo'
  , logWarn'
  , logDebug'
  ) where

import Contract.Prelude

import Contract.Log (logDebug', logInfo', logWarn') as Contract
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
  | LogDebug' String a

derive instance functorLogF :: Functor LogF

type LOG r = (log :: LogF | r)

_log :: Proxy "log"
_log = Proxy

handleLogWith ::
  forall r. (LogF ~> Run r) -> Run (LOG + r) ~> Run r
handleLogWith f = interpret (on _log f send)

logInfo' ::
  forall r. String -> Run (LOG + r) Unit
logInfo' msg = Run.lift _log
  (LogInfo' msg unit)

logWarn' ::
  forall r. String -> Run (LOG + r) Unit
logWarn' msg = Run.lift _log
  (LogWarn' msg unit)

logDebug' ::
  forall r. String -> Run (LOG + r) Unit
logDebug' msg = Run.lift _log
  (LogDebug' msg unit)

handleLogLive ::
  forall r. LogF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleLogLive =
  case _ of
    LogInfo' msg x -> const x <$> withTry
      (fromError "logInfo': ")
      (Contract.logInfo' msg)
    LogWarn' msg x -> const x <$> withTry
      (fromError "logWarn': ")
      (Contract.logWarn' msg)
    LogDebug' msg x -> const x <$> withTry
      (fromError "logDebug': ")
      (Contract.logDebug' msg)
  where
  fromError :: String -> Error -> OffchainError
  fromError ctx =
    parseFromError parseDefaultError (Just (ErrorContext Log ctx))
