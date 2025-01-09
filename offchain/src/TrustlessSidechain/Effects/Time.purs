module TrustlessSidechain.Effects.Time
  ( TIME
  , TimeF
  , handleTimeLive
  , handleTimeWith
  , getCurrentEra
  , getSystemStart
  ) where

import Contract.Prelude

import Contract.Log (logDebug', logInfo', logWarn') as Contract
import Contract.Time as Time
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

data TimeF a
  = GetCurrentEra' (Time.EraSummary -> a)
  | GetSystemStart' (Time.SystemStart -> a)

derive instance functorTimeF :: Functor TimeF

type TIME r = (time :: TimeF | r)

_time :: Proxy "time"
_time = Proxy

handleTimeWith ::
  forall r. (TimeF ~> Run r) -> Run (TIME + r) ~> Run r
handleTimeWith f = interpret (on _time f send)

getCurrentEra ::
  forall r. Run (TIME + r) Time.EraSummary
getCurrentEra = Run.lift _time
  (GetCurrentEra' identity)

getSystemStart ::
  forall r. Run (TIME + r) Time.SystemStart
getSystemStart = Run.lift _time
  (GetSystemStart' identity)

handleTimeLive ::
  forall r. TimeF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleTimeLive =
  case _ of
    GetCurrentEra' f -> f <$> withTry
      (fromError "getCurrentEra: ")
      (Time.getCurrentEra)
    GetSystemStart' f -> f <$> withTry
      (fromError "getSystemStart: ")
      (Time.getSystemStart)
  where
  fromError :: String -> Error -> OffchainError
  fromError ctx =
    parseFromError parseDefaultError (Just (ErrorContext Log ctx))
