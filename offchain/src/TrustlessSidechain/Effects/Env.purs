module TrustlessSidechain.Effects.Env
  ( Env(..)
  , emptyEnv
  , module X
  ) where

import Data.Maybe (Maybe(Nothing))
import Run.Reader (READER, ask, runReader) as X
import TrustlessSidechain.Governance (Governance)

emptyEnv ∷ Env
emptyEnv = { governance: Nothing }

type Env =
  { governance ∷ Maybe Governance
  }
