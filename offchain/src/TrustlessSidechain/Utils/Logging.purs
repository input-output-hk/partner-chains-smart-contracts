module TrustlessSidechain.Utils.Logging
  ( Environment
  , Location
  , class Display
  , display
  , environment
  , fileLogger
  , mkReport
  ) where

import Contract.Prelude

import Contract.Config (Message)
import Data.Log.Formatter.JSON (jsonFormatter)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (appendTextFile)
import Node.Process (stdoutIsTTY)

-- | This class overrides `Show` in order to remove quoting from strings
class Display (a ∷ Type) where
  display ∷ a → String

instance Display String where
  display = identity

else instance Show default ⇒ Display default where
  display = show

-- | Used to parametrize `mkReport`
type Location = { mod ∷ String, fun ∷ String }

-- | builds a unified look for error messages by giving them more structure.
-- | this function is used to instantiate a message formatter as so:
-- | ```purescript
-- | myformatter ∷ ∀ e. Display e ⇒ e → String
-- | myformatter = mkReport { mod : "MyModule", fun : "myFunction" }
-- | myformatter "this is an error message."
-- | ```
mkReport ∷ Location → (∀ (e ∷ Type). Display e ⇒ e → String)
mkReport { mod, fun } msg = mod <> "." <> fun <> ": " <> display msg

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
