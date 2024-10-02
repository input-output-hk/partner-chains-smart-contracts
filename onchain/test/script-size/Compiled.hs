{-# LANGUAGE TemplateHaskell #-}

module Compiled (
  toDataGenerated,
  toDataHandwritten,
  fromDataGenerated,
  fromDataHandwritten,
  unsafeFromDataGenerated,
  unsafeFromDataHandwritten,
  pairToDataGenerated,
  pairToDataHandwritten,
  pairFromDataGenerated,
  pairFromDataHandwritten,
  pairUnsafeFromDataGenerated,
  pairUnsafeFromDataHandwritten,
  listToDataGenerated,
  listToDataHandwritten,
  listFromDataHandwritten,
  listFromDataGenerated,
  toDataWrapper,
  toDataDirect,
  fromDataWrapper,
  fromDataDirect,
  unsafeFromDataWrapper,
  unsafeFromDataDirect,
  toData3CPS,
  toData3Direct,
  fromData3CPS,
  fromData3Direct,
  unsafeFromData3CPS,
  unsafeFromData3Direct,
) where

import Data.Generated qualified as Generated
import Data.Handwritten qualified as Handwritten
import PlutusTx.Code (CompiledCode)
import PlutusTx.TH (compile)
import TrustlessSidechain.PlutusPrelude

toData3CPS :: CompiledCode (Generated.Baz -> BuiltinData)
toData3CPS = $$(compile [||toBuiltinData||])

toData3Direct :: CompiledCode (Handwritten.Baz -> BuiltinData)
toData3Direct = $$(compile [||toBuiltinData||])

fromData3CPS :: CompiledCode (BuiltinData -> Maybe Generated.Baz)
fromData3CPS = $$(compile [||fromBuiltinData||])

fromData3Direct :: CompiledCode (BuiltinData -> Maybe Handwritten.Baz)
fromData3Direct = $$(compile [||fromBuiltinData||])

unsafeFromData3CPS :: CompiledCode (BuiltinData -> Generated.Baz)
unsafeFromData3CPS = $$(compile [||unsafeFromBuiltinData||])

unsafeFromData3Direct :: CompiledCode (BuiltinData -> Handwritten.Baz)
unsafeFromData3Direct = $$(compile [||unsafeFromBuiltinData||])

toDataDirect :: CompiledCode (Handwritten.Bar -> BuiltinData)
toDataDirect = $$(compile [||toBuiltinData||])

toDataWrapper :: CompiledCode (Generated.Bar -> BuiltinData)
toDataWrapper = $$(compile [||toBuiltinData||])

fromDataDirect :: CompiledCode (BuiltinData -> Maybe Handwritten.Bar)
fromDataDirect = $$(compile [||fromBuiltinData||])

fromDataWrapper :: CompiledCode (BuiltinData -> Maybe Generated.Bar)
fromDataWrapper = $$(compile [||fromBuiltinData||])

unsafeFromDataDirect :: CompiledCode (BuiltinData -> Handwritten.Bar)
unsafeFromDataDirect = $$(compile [||unsafeFromBuiltinData||])

unsafeFromDataWrapper :: CompiledCode (BuiltinData -> Generated.Bar)
unsafeFromDataWrapper = $$(compile [||unsafeFromBuiltinData||])

listFromDataGenerated :: CompiledCode (BuiltinData -> Maybe [Integer])
listFromDataGenerated = $$(compile [||fromBuiltinData||])

listFromDataHandwritten :: CompiledCode (BuiltinData -> Maybe [Integer])
listFromDataHandwritten = $$(compile [||Handwritten.listFromData||])

listToDataGenerated :: CompiledCode ([Integer] -> BuiltinData)
listToDataGenerated = $$(compile [||toBuiltinData||])

listToDataHandwritten :: CompiledCode ([Integer] -> BuiltinData)
listToDataHandwritten = $$(compile [||Handwritten.listToData||])

pairUnsafeFromDataGenerated :: CompiledCode (BuiltinData -> (Integer, Integer))
pairUnsafeFromDataGenerated = $$(compile [||unsafeFromBuiltinData||])

pairUnsafeFromDataHandwritten :: CompiledCode (BuiltinData -> (Integer, Integer))
pairUnsafeFromDataHandwritten = $$(compile [||Handwritten.pairUnsafeFromData||])

pairFromDataGenerated :: CompiledCode (BuiltinData -> Maybe (Integer, Integer))
pairFromDataGenerated = $$(compile [||fromBuiltinData||])

pairFromDataHandwritten :: CompiledCode (BuiltinData -> Maybe (Integer, Integer))
pairFromDataHandwritten = $$(compile [||Handwritten.pairFromData||])

pairToDataGenerated :: CompiledCode ((Integer, Integer) -> BuiltinData)
pairToDataGenerated = $$(compile [||toBuiltinData||])

pairToDataHandwritten :: CompiledCode ((Integer, Integer) -> BuiltinData)
pairToDataHandwritten = $$(compile [||Handwritten.pairToData||])

fromDataGenerated :: CompiledCode (BuiltinData -> Maybe Generated.Foo)
fromDataGenerated = $$(compile [||fromBuiltinData||])

fromDataHandwritten :: CompiledCode (BuiltinData -> Maybe Handwritten.Foo)
fromDataHandwritten = $$(compile [||fromBuiltinData||])

toDataGenerated :: CompiledCode (Generated.Foo -> BuiltinData)
toDataGenerated = $$(compile [||toBuiltinData||])

toDataHandwritten :: CompiledCode (Handwritten.Foo -> BuiltinData)
toDataHandwritten = $$(compile [||toBuiltinData||])

unsafeFromDataGenerated :: CompiledCode (BuiltinData -> Generated.Foo)
unsafeFromDataGenerated = $$(compile [||unsafeFromBuiltinData||])

unsafeFromDataHandwritten :: CompiledCode (BuiltinData -> Handwritten.Foo)
unsafeFromDataHandwritten = $$(compile [||unsafeFromBuiltinData||])
