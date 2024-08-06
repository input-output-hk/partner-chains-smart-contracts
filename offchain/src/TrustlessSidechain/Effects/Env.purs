module TrustlessSidechain.Effects.Env
  ( Env(..)
  , emptyEnv
  , module X
  ) where

import TrustlessSidechain.Governance (Governance)
import Run.Reader (READER, ask, runReader) as X
import Data.Maybe (Maybe(Nothing))


emptyEnv :: Env
emptyEnv = { governance: Nothing }

type Env =
    { governance :: Maybe Governance
    }
