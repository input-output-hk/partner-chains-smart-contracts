module TrustlessSidechain.EndpointResp
  ( EndpointResp(..)
  , encodeEndpointResp
  , stringifyEndpointResp
  ) where

import Contract.Prelude

import Aeson
  ( encodeAeson
  , toStringifiedNumbersJson
  )
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  )
import Data.Argonaut (Json)
import Data.Argonaut.Core as J
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.List (List)
import Data.Map (Map)
import Foreign.Object as Object
import TrustlessSidechain.GetSidechainAddresses (SidechainAddresses)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Codecs
  ( encodeInitTokenStatusData
  , scParamsCodec
  )
import TrustlessSidechain.Versioning.ScriptId (ScriptId)
import TrustlessSidechain.Versioning.Types as Types

-- | Response data to be presented after contract endpoint execution
data EndpointResp
  = CommitteeCandidateRegResp { transactionId :: ByteArray }
  | CommitteeCandidateDeregResp { transactionId :: ByteArray }
  | GetAddrsResp { sidechainAddresses :: SidechainAddresses }
  | InitTokensMintResp
      { transactionId :: Maybe ByteArray
      , sidechainParams :: SidechainParams
      , sidechainAddresses :: SidechainAddresses
      }
  | InitReserveManagementResp
      { scriptsInitTxIds :: Array ByteArray
      }
  | InsertVersionResp { versioningTransactionIds :: Array ByteArray }
  | UpdateVersionResp { versioningTransactionIds :: Array ByteArray }
  | InvalidateVersionResp { versioningTransactionIds :: Array ByteArray }
  | InsertDParameterResp
      { transactionId :: ByteArray }
  | UpdateDParameterResp
      { transactionId :: ByteArray }
  | UpdatePermissionedCandidatesResp
      { transactionId :: ByteArray }
  | InitTokenStatusResp
      { initTokenStatusData :: Map AssetName BigNum }
  | ListVersionedScriptsResp
      { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
      , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
      }
  | ReserveResp { transactionHash :: ByteArray }

-- Note [BigInt values and JSON]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- `BigInt` values are not supported in JSON and coercing them to
-- `Number` can lead to loss of information. `Argonaut.Json` does not
-- support `BigInt` encoding or decoding. Therefore, `BigInt` values
-- are converted to strings before serializing to JSON. See the
-- `BigInt` documentation at
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/

-- | Codec of the endpoint response data. Only includes an encoder, we don't need a decoder.
-- | See Note [BigInt values and JSON]
endpointRespCodec :: CA.JsonCodec EndpointResp
endpointRespCodec = CA.prismaticCodec "EndpointResp" dec enc CA.json
  where
  dec :: Json -> Maybe EndpointResp
  dec _ = Nothing

  enc :: EndpointResp -> Json
  enc = case _ of
    CommitteeCandidateRegResp { transactionId } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeCandidateReg"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    CommitteeCandidateDeregResp { transactionId } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeCandidateDereg"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    GetAddrsResp { sidechainAddresses } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "GetAddrs"
        , "addresses" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.addresses
                )
            )
        , "validatorHashes" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.validatorHashes
                )
            )
        , "mintingPolicies" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.mintingPolicies
                )
            )
        ]
    InitTokensMintResp
      { transactionId
      , sidechainParams
      , sidechainAddresses
      } ->
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "InitTokensMint"
          -- NOTE: Nothing encoded to null
          , "transactionId" /\ CA.encode
              (CAC.maybe CA.string)
              (map byteArrayToHex transactionId)
          , "sidechainParams" /\ CA.encode scParamsCodec sidechainParams
          , "addresses" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.addresses
                  )
              )
          , "validatorHashes" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.validatorHashes
                  )
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) -> show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.mintingPolicies
                  )
              )
          ]

    InitReserveManagementResp
      { scriptsInitTxIds
      } ->
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "InitReserveManagement"
          , "scriptsInitTxIds" /\ J.fromArray
              (map (J.fromString <<< byteArrayToHex) scriptsInitTxIds)
          ]

    InsertVersionResp { versioningTransactionIds } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InitVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
        ]
    UpdateVersionResp { versioningTransactionIds } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdateVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
        ]
    InvalidateVersionResp { versioningTransactionIds } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InvalidateVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
        ]
    InsertDParameterResp
      { transactionId } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InsertDParameter"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    UpdateDParameterResp
      { transactionId } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdateDParameter"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    UpdatePermissionedCandidatesResp
      { transactionId } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdatePermissionedCandidates"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    InitTokenStatusResp
      { initTokenStatusData } ->
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InitTokenStatus"
        , "initTokenStatusData" /\ encodeInitTokenStatusData initTokenStatusData
        ]

    ListVersionedScriptsResp
      { versionedPolicies, versionedValidators } -> do
      -- We encode in JSON the versioned script ids along with their hashes
      let
        (versionedScriptIdsWithHashes :: List (Tuple ScriptId ScriptHash)) =
          (map (map PlutusScript.hash) versionedPolicies)
            <> (map (map PlutusScript.hash) versionedValidators)
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "ListVersionedScripts"
        , "versionedScripts" /\ toStringifiedNumbersJson
            (encodeAeson $ map show $ versionedScriptIdsWithHashes)
        ]

    ReserveResp { transactionHash } ->
      J.fromObject $ Object.fromFoldable
        [ "transactionHash" /\ J.fromString (byteArrayToHex transactionHash) ]

-- | Encode the endpoint response to a json object
encodeEndpointResp :: EndpointResp -> J.Json
encodeEndpointResp = CA.encode endpointRespCodec

-- | Encode the endpoint response to a json encoded string
stringifyEndpointResp :: EndpointResp -> String
stringifyEndpointResp = encodeEndpointResp >>> J.stringify
