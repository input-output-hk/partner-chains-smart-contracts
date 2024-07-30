module TrustlessSidechain.Effects.App
  ( APP
  , BASE
  ) where

import Run (AFF, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import TrustlessSidechain.Effects.Contract (CONTRACT)
import TrustlessSidechain.Effects.Env (Env)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

-- | The top level effects
type APP r = READER Env + EXCEPT OffchainError + WALLET + TRANSACTION + LOG + r

-- | The base effects
type BASE r = CONTRACT + EFFECT + AFF + r
