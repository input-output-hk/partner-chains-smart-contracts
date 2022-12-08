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
import GetSidechainAddresses (SidechainAddresses)
import SidechainParams (SidechainParams, scParamsCodec)

-- | Response data to be presented after contract endpoint execution
data EndpointResp
  = MintActResp { transactionId ∷ ByteArray }
  | ClaimActResp { transactionId ∷ ByteArray }
  | BurnActResp { transactionId ∷ ByteArray }
  | CommitteeCandidateRegResp { transactionId ∷ ByteArray }
  | CommitteeCandidateDeregResp { transactionId ∷ ByteArray }
  | GetAddrsResp { sidechainAddresses ∷ SidechainAddresses }
  | CommitteeHashResp { transactionId ∷ ByteArray }
  | SaveRootResp { transactionId ∷ ByteArray }
  | CommitteeHandoverResp
      { saveRootTransactionId ∷ ByteArray
      , committeeHashTransactionId ∷ ByteArray
      }
  | InitResp
      { transactionId ∷ ByteArray
      , sidechainParams ∷ SidechainParams
      , sidechainAddresses ∷ SidechainAddresses
      }

-- | Codec of the endpoint response data. Only includes an encoder, we don't need a decoder
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
    ClaimActResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "ClaimAct"
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
    GetAddrsResp { sidechainAddresses } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "GetAddrs"
        , "addresses" /\ J.fromObject
            ( Object.fromFoldable
                (map (rmap J.fromString) sidechainAddresses.addresses)
            )
        , "mintingPolicies" /\ J.fromObject
            ( Object.fromFoldable
                (map (rmap J.fromString) sidechainAddresses.mintingPolicies)
            )
        ]
    CommitteeHashResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeHash"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    SaveRootResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "SaveRoot"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    CommitteeHandoverResp { saveRootTransactionId, committeeHashTransactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CommitteeHandover"
        , "saveRootTransactionId" /\ J.fromString
            (byteArrayToHex saveRootTransactionId)
        , "committeeHashTransactionId" /\ J.fromString
            (byteArrayToHex committeeHashTransactionId)
        ]
    InitResp { transactionId, sidechainParams, sidechainAddresses } →
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "Init"
          , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
          , "sidechainParams" /\ CA.encode scParamsCodec sidechainParams
          , "addresses" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.addresses)
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.mintingPolicies)
              )
          ]

-- | Encode the endpoint response to a json object
encodeEndpointResp ∷ EndpointResp → J.Json
encodeEndpointResp = CA.encode endpointRespCodec

-- | Encode the endpoint response to a json encoded string
stringifyEndpointResp ∷ EndpointResp → String
stringifyEndpointResp = encodeEndpointResp >>> J.stringify
