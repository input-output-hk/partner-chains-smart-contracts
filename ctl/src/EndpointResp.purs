module EndpointResp
  ( EndpointResp(..)
  , encodeEndpointResp
  , stringifyEndpointResp
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Data.Argonaut.Core as J
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut as CA
import Foreign.Object as Object

-- | Response data to be presented after contract endpoint execution
data EndpointResp
  = MintActResp { transactionId ∷ ByteArray }
  | BurnActResp { transactionId ∷ ByteArray }
  | CommitteeCandidateRegResp { transactionId ∷ ByteArray }
  | CommitteeCandidateDeregResp { transactionId ∷ ByteArray }
  | GetAddrsResp { addresses ∷ Array (Tuple String String) }

-- | Codec of the endpoint response data. Only includes an encoder, we don't need to decoder
endpointRespCodec ∷ CA.JsonCodec EndpointResp
endpointRespCodec = CA.prismaticCodec "EndpointResp" dec enc CA.json
  where
  dec _ = Nothing
  enc = case _ of
    MintActResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "MintAct"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    BurnActResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "BurnAct"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    CommitteeCandidateRegResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeCandidateReg"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    CommitteeCandidateDeregResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeCandidateDereg"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    GetAddrsResp { addresses } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "GetAddrs"
        , "addresses" /\ J.fromObject
            (Object.fromFoldable (map (rmap J.fromString) addresses))
        ]

-- | Encode the endpoint response to a json object
encodeEndpointResp ∷ EndpointResp → J.Json
encodeEndpointResp = CA.encode endpointRespCodec

-- | Encode the endpoint response to a json encoded string
stringifyEndpointResp ∷ EndpointResp → String
stringifyEndpointResp = encodeEndpointResp >>> J.stringify
