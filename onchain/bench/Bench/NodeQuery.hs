{-# LANGUAGE RankNTypes #-}

{- | "Bench.NodeQuery" provides utility functions for querying the cardano
 node / working with cardano stuff in general.

 TODO: When our CTL dependency gets Kupo, it's probably better to ask Kupo
 for this information.
-}
module Bench.NodeQuery (
  -- * Node querying functionality
  queryNodeUtxoAddress,

  -- * Errors
  NodeQueryError (..),
) where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Aeson (Value (Object))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import Plutus.V2.Ledger.Api (
  TxOutRef,
 )
import System.Process qualified as Process
import TrustlessSidechain.OffChain qualified as OffChain
import Prelude

{- | @'queryNodeUtxoAddress' cardanoCli testNetMagic address@ returns the keys
 - from the
 JSON output from
 @
      cardano-cli query utxo
          --testnet-magic 2 \
          --address "addr_test1vq9m0ma46xzspaq2jwdefuurt2zm2ct9yj495t22578p6xc7kgt8y" \
          --out-file /dev/stdout
 @

 of course, @cardano-cli@, @--testnet-magic@ and @--address@ are replaced by
 the parameters.

 Example output is as follows.
 @
 {
  "c97c374fa579742fb7934b9a9c306734fdc0d48432d4d6b46498c8288b88100c#0": {
      "address": "addr_test1vq9m0ma46xzspaq2jwdefuurt2zm2ct9yj495t22578p6xc7kgt8y",
      "datum": null,
      "datumhash": null,
      "inlineDatum": null,
      "referenceScript": null,
      "value": {
          "lovelace": 1742606
      }
  },
  "e79f1d54a038c4ba5c1a8fc1de4870ef04f0a10408647396634da02e1413b7fa#1": {
      "address": "addr_test1vq9m0ma46xzspaq2jwdefuurt2zm2ct9yj495t22578p6xc7kgt8y",
      "datum": null,
      "datumhash": null,
      "inlineDatum": null,
      "referenceScript": null,
      "value": {
          },
          "fbf161d16b85bad5d73e11b31a4ca24ecab5e0159e9be8530b5efdda": {
              "4655454c": 420
          },
          "lovelace": 9719854530
      }
  }
 @
 Purposely, this API is left rather empty since we don't really need anything
 fancy.
-}
queryNodeUtxoAddress :: String -> Int -> String -> IO [TxOutRef]
queryNodeUtxoAddress cardanoCli testnetMagic bech32Address = do
  let cmd =
        List.unwords
          [ cardanoCli
          , "query"
          , "utxo"
          , List.unwords ["--testnet-magic", show testnetMagic]
          , List.unwords ["--address", bech32Address]
          , List.unwords ["--out-file", "/dev/stdout"]
          ]
  -- Awkward, this uses string..
  stdout <- Process.readCreateProcess (Process.shell cmd) ""
  decodeResult <- case Aeson.eitherDecodeStrict $ ByteString.Char8.pack stdout of
    Right res -> return (res :: Value)
    Left err ->
      Exception.throwIO $
        NodeQueryError $
          "`queryNodeUtxoAddress` internal error malformed JSON: "
            ++ err
            ++ ". Failed parsing: "
            ++ stdout
  case decodeResult of
    Object keyMap ->
      case Traversable.traverse (OffChain.txOutRefFromText . Aeson.Key.toText) $
        Aeson.KeyMap.keys keyMap of
        Left err ->
          Exception.throwIO $
            NodeQueryError $
              "`queryNodeUtxoAddress` internal error bad parse: " ++ err
        Right res -> return res
    _ ->
      Exception.throwIO $
        NodeQueryError
          "`queryNodeUtxoAddress` internal error invalid JSON parsing"

--  | 'NodeQueryError' is an error message relating to a node query. This is
--  used to throw internal exceptions.
newtype NodeQueryError = NodeQueryError String
  deriving (Show)

instance Exception NodeQueryError
