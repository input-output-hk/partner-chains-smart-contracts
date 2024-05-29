module TrustlessSidechain.Foo where

import Contract.Prelude

import Contract.PlutusData
  ( unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.Value (TokenName)
import Contract.Value as Value
import Data.Maybe as Maybe
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Utils.LookupsAndConstraints (mintOneToken)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(FooPolicy)
  )
import Type.Row (type (+))

fooTokenName ∷ TokenName
fooTokenName =
  unsafePartial $ Maybe.fromJust $ Value.mkTokenName
    =<< byteArrayFromAscii "Foo"

-- | Deserialize minting policy script, applying it to all required parameters.
fooCurrencyInfo ∷
  ∀ r.
  Run (EXCEPT OffchainError + r) CurrencyInfo
fooCurrencyInfo = do
  getCurrencyInfo FooPolicy []

mintFooTokenLookupsAndConstraints ∷
  ∀ r.
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintFooTokenLookupsAndConstraints = do
  ci ← fooCurrencyInfo

  pure $ mintOneToken fooTokenName unitRedeemer ci
