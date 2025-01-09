module TrustlessSidechain.Effects.App
  ( APP
  , BASE
  ) where

import Run (AFF, EFFECT)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Time (TIME)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

-- | The top level effects
type APP r = EXCEPT OffchainError + WALLET + TRANSACTION + LOG + TIME + r

-- | The base effects
type BASE r = CONTRACT + EFFECT + AFF + r
