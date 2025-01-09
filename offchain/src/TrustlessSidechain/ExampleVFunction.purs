module TrustlessSidechain.ExampleVFunction where

import Contract.Prelude hiding (note)

import Cardano.AsCbor (encodeCbor)
import Cardano.ToData (toData)
import Cardano.Types.BigInt (BigInt)
import Cardano.Types.PlutusScript (PlutusScript, getBytes)
import Data.ByteArray (ByteArray)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( ExampleVFunctionPolicy
      )
  )
import Type.Row (type (+))

decodeExampleVFunctionPolicy ::
  forall r.
  BigInt ->
  Run (EXCEPT OffchainError + WALLET + r) ByteArray
decodeExampleVFunctionPolicy unixTimestamp = do

  plutusScript <- mkMintingPolicyWithParams ExampleVFunctionPolicy $
    [ toData unixTimestamp
    ]

  pure $ unwrap $ encodeCbor plutusScript

exampleVFunctionPolicy ::
  forall r.
  BigInt ->
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
exampleVFunctionPolicy unixTimestamp = do

  mkMintingPolicyWithParams ExampleVFunctionPolicy $
    [ toData unixTimestamp
    ]
