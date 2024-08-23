module TrustlessSidechain.Effects.Log
  ( LOG
  , LogF
  , TimerState
  , TimerStateMap
  , handleLogLive
  , handleLogWith
  , logDebug'
  , logInfo'
  , logTimer
  , logWarn'
  , newTimer
  ) where

import Contract.Prelude

import Contract.Log (logDebug', logInfo', logWarn') as Contract
import Data.DateTime.Instant (Instant, diff)
import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Error)
import Effect.Now (now)
import Effect.Ref as Ref
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
  | LogTimer String String a
  | NewTimer String a

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

logDebug' ∷
  ∀ r. String → Run (LOG + r) Unit
logDebug' msg = Run.lift _log
  (LogDebug' msg unit)

logTimer ∷
  ∀ r. String → String → Run (LOG + r) Unit
logTimer tag msg = Run.lift _log
  (LogTimer tag msg unit)

newTimer ∷
  ∀ r. String → Run (LOG + r) Unit
newTimer msg = Run.lift _log
  (NewTimer msg unit)

handleLogLive ∷
  ∀ r.
  Maybe (Ref.Ref TimerStateMap) →
  LogF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleLogLive timer =
  case _ of
    LogInfo' msg x → const x <$> withTry
      (fromError "logInfo': ")
      (Contract.logInfo' msg)
    LogWarn' msg x → const x <$> withTry
      (fromError "logWarn': ")
      (Contract.logWarn' msg)
    LogDebug' msg x → const x <$> withTry
      (fromError "logDebug': ")
      (Contract.logDebug' msg)
    LogTimer tag msg x → const x <$> withTry
      (fromError "logTimer: ")
      ( case timer of
          Just t → liftEffect $ logElapsedTime t tag msg
          Nothing → pure unit
      )
    NewTimer msg x → const x <$> withTry
      (fromError "newTimer: ")
      ( case timer of
          Just t → do
            newT ← liftEffect $ newTimer' msg
            liftEffect $ Ref.modify_ (Map.insert msg newT) t
            pure unit
          Nothing → pure unit

      )
  where
  fromError ∷ String → Error → OffchainError
  fromError ctx =
    parseFromError parseDefaultError (Just (ErrorContext Log ctx))

type TimerStateMap = Map String TimerState

-- Timer type
type TimerState =
  { startTime ∷ Instant
  , lastLoggedTime ∷ Maybe Instant
  , name ∷ String
  , stopped ∷ Boolean
  , stopTime ∷ Maybe Instant
  }

-- Create a new timer
newTimer' ∷ String → Effect (TimerState)
newTimer' name = do
  startTime ← now
  pure $
    { startTime
    , lastLoggedTime: Nothing
    , name
    , stopped: false
    , stopTime: Nothing
    }

-- Get the elapsed time between two Instants
getElapsedMillis ∷ Instant → Instant → Milliseconds
getElapsedMillis end start = diff end start

-- Get the elapsed time since the timer started
getElapsedTime ∷ Ref.Ref TimerState → Effect Milliseconds
getElapsedTime timerRef = do
  { startTime, stopped, stopTime } ← Ref.read timerRef
  case stopped, stopTime of
    true, Just endTime → pure $ getElapsedMillis endTime startTime
    _, _ → do
      currentTime ← now
      pure $ getElapsedMillis currentTime startTime

-- Log the time difference since last log (or start if first log)
logElapsedTime ∷ Ref.Ref TimerStateMap → String → String → Effect Unit
logElapsedTime timerRef tag msg = do
  timerStateMap ← Ref.read timerRef
  case Map.lookup tag timerStateMap of
    Nothing → do
      newT ← liftEffect $ newTimer' tag
      liftEffect $ Ref.modify_ (Map.insert tag newT) timerRef
      log ("No timer set: " <> tag <> ", so created one.")
    Just timerState → do
      if timerState.stopped then log $ timerState.name <>
        " has been stopped. No more logging."
      else do
        currentTime ← now
        let
          { name, startTime, lastLoggedTime } = timerState
          totalElapsed = getElapsedMillis currentTime startTime
          (timeSinceLastLog /\ timeDescription) = case lastLoggedTime of
            Nothing → (totalElapsed /\ "total")
            Just lastTime →
              let
                interval = getElapsedMillis currentTime lastTime
              in
                (interval /\ "interval")

        log $ "Timer " <> tag <> " says: " <> msg <> " : " <> name <> " "
          <> timeDescription
          <> " time: "
          <> show (unwrap timeSinceLastLog)
          <> " milliseconds"

        -- Update the lastLoggedTime
        Ref.modify_
          (Map.update (Just <<< _ { lastLoggedTime = Just currentTime }) tag)
          timerRef

-- Stop the timer and get the final elapsed time
stopTimer ∷ Ref.Ref TimerState → Effect Milliseconds
stopTimer timerRef = do
  currentTime ← now
  timerState ← Ref.read timerRef
  if timerState.stopped then pure $ case timerState.stopTime of
    Just stopTime → getElapsedMillis stopTime timerState.startTime
    Nothing → Milliseconds 0.0 -- This should never happen if our logic is correct
  else do
    let finalElapsed = getElapsedMillis currentTime timerState.startTime
    Ref.modify_ (_ { stopped = true, stopTime = Just currentTime }) timerRef
    pure finalElapsed