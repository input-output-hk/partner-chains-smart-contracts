module TrustlessSidechain.EndpointResp
  ( EndpointResp(..)
  , encodeEndpointResp
  , stringifyEndpointResp
  ) where

import Contract.Prelude

import Contract.CborBytes (cborBytesToByteArray)
import Contract.PlutusData (class ToData, PlutusData)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.Value (CurrencySymbol)
import Data.Argonaut (Json)
import Data.Argonaut.Core as J
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut as CA
import Foreign.Object as Object
import TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof
  )
import TrustlessSidechain.GetSidechainAddresses (SidechainAddresses)
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.MerkleTree
  ( MerkleTree
  , RootHash
  , unRootHash
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Codecs (scParamsCodec)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1
  ( SchnorrSecp256k1PrivateKey
  , SchnorrSecp256k1PublicKey
  , SchnorrSecp256k1Signature
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1

-- | Response data to be presented after contract endpoint execution
data EndpointResp
  = MintActResp { transactionId ∷ ByteArray }
  | ClaimActResp { transactionId ∷ ByteArray }
  | BurnActResp { transactionId ∷ ByteArray }
  | CommitteeCandidateRegResp { transactionId ∷ ByteArray }
  | CandidatePermissionTokenResp
      { transactionId ∷ ByteArray
      , candidatePermissionCurrencySymbol ∷ CurrencySymbol
      }
  | CommitteeCandidateDeregResp { transactionId ∷ ByteArray }
  | GetAddrsResp { sidechainAddresses ∷ SidechainAddresses }
  | CommitteeHashResp { transactionId ∷ ByteArray }
  | SaveRootResp { transactionId ∷ ByteArray }
  | CommitteeHandoverResp
      { saveRootTransactionId ∷ ByteArray
      , committeeHashTransactionId ∷ ByteArray
      }
  | InitTokensResp
      { transactionId ∷ ByteArray
      , sidechainParams ∷ SidechainParams
      , sidechainAddresses ∷ SidechainAddresses
      }
  | InitResp
      { transactionId ∷ ByteArray
      , sidechainParams ∷ SidechainParams
      , sidechainAddresses ∷ SidechainAddresses
      }
  | SaveCheckpointResp { transactionId ∷ ByteArray }
  | EcdsaSecp256k1KeyGenResp
      { publicKey ∷ EcdsaSecp256k1PubKey
      , privateKey ∷ EcdsaSecp256k1PrivateKey
      }
  | SchnorrSecp256k1KeyGenResp
      { publicKey ∷ SchnorrSecp256k1PublicKey
      , privateKey ∷ SchnorrSecp256k1PrivateKey
      }
  | EcdsaSecp256k1SignResp
      { publicKey ∷ EcdsaSecp256k1PubKey
      , signature ∷ EcdsaSecp256k1Signature
      , signedMessage ∷ ByteArray
      }
  | SchnorrSecp256k1SignResp
      { publicKey ∷ SchnorrSecp256k1PublicKey
      , signature ∷ SchnorrSecp256k1Signature
      , signedMessage ∷ ByteArray
      }
  | CborUpdateCommitteeMessageResp
      { plutusData ∷ PlutusData
      }
  | CborBlockProducerRegistrationMessageResp
      { plutusData ∷ PlutusData
      }
  | CborMerkleRootInsertionMessageResp
      { plutusData ∷ PlutusData
      }
  | CborMerkleTreeEntryResp
      { plutusData ∷ PlutusData
      }
  | CborMerkleTreeResp
      { merkleRootHash ∷ RootHash
      , merkleTree ∷ MerkleTree
      }
  | CborCombinedMerkleProofResp
      { combinedMerkleProof ∷ CombinedMerkleProof
      }
  | CborPlainAggregatePublicKeysResp
      { aggregatedPublicKeys ∷ PlutusData
      }

-- | `serialisePlutusDataToHex` serialises plutus data to CBOR, and shows the
-- | hex encoded CBOR.
serialisePlutusDataToHex ∷ ∀ a. ToData a ⇒ a → String
serialisePlutusDataToHex = byteArrayToHex <<< cborBytesToByteArray <<<
  PlutusData.serializeData

-- | Codec of the endpoint response data. Only includes an encoder, we don't need a decoder
endpointRespCodec ∷ CA.JsonCodec EndpointResp
endpointRespCodec = CA.prismaticCodec "EndpointResp" dec enc CA.json
  where
  dec ∷ Json → Maybe EndpointResp
  dec _ = Nothing

  enc ∷ EndpointResp → Json
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
    CandidatePermissionTokenResp
      { transactionId, candidatePermissionCurrencySymbol } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CandidatePermissionToken"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        , "candidatePermissionCurrencySymbol"
            /\ J.fromString
              ( GetSidechainAddresses.currencySymbolToHex
                  candidatePermissionCurrencySymbol
              )
        ]
    GetAddrsResp { sidechainAddresses } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "GetAddrs"
        , "addresses" /\ J.fromObject
            ( Object.fromFoldable
                (map (rmap J.fromString) sidechainAddresses.addresses)
            )
        , "cborEncodedAddresses" /\ J.fromObject
            ( Object.fromFoldable
                (map (rmap J.fromString) sidechainAddresses.cborEncodedAddresses)
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
    InitTokensResp { transactionId, sidechainParams, sidechainAddresses } →
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "Init"
          , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
          , "sidechainParams" /\ CA.encode scParamsCodec sidechainParams
          , "addresses" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.addresses)
              )
          , "cborEncodedAddresses" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.cborEncodedAddresses)
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.mintingPolicies)
              )
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
          , "cborEncodedAddresses" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.cborEncodedAddresses)
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  (map (rmap J.fromString) sidechainAddresses.mintingPolicies)
              )
          ]
    SaveCheckpointResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "SaveCheckpoint"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    EcdsaSecp256k1KeyGenResp { publicKey, privateKey } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "EcdsaSecp256k1KeyGen"
        , "rawHexPublicKey" /\ J.fromString
            (Utils.Crypto.serialiseEcdsaSecp256k1PubKey publicKey)
        , "rawHexPrivateKey" /\ J.fromString
            (Utils.Crypto.serialiseEcdsaSecp256k1PrivateKey privateKey)
        ]
    SchnorrSecp256k1KeyGenResp { publicKey, privateKey } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "SchnorrSecp256k1KeyGen"
        , "rawHexPublicKey" /\ J.fromString
            (Utils.SchnorrSecp256k1.serializePublicKey publicKey)
        , "rawHexPrivateKey" /\ J.fromString
            (Utils.SchnorrSecp256k1.serializePrivateKey privateKey)
        ]
    EcdsaSecp256k1SignResp { publicKey, signature, signedMessage } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "EcdsaSecp256k1Sign"
        , "rawHexPublicKey" /\ J.fromString
            (Utils.Crypto.serialiseEcdsaSecp256k1PubKey publicKey)
        , "rawHexSignature" /\ J.fromString
            (Utils.Crypto.serialiseEcdsaSecp256k1Signature signature)
        , "rawHexSignedMessage" /\ J.fromString (byteArrayToHex signedMessage)
        ]
    SchnorrSecp256k1SignResp { publicKey, signature, signedMessage } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "SchnorrSecp256k1Sign"
        , "rawHexPublicKey" /\ J.fromString
            (Utils.SchnorrSecp256k1.serializePublicKey publicKey)
        , "rawHexSignature" /\ J.fromString
            (Utils.SchnorrSecp256k1.serializeSignature signature)
        , "rawHexSignedMessage" /\ J.fromString (byteArrayToHex signedMessage)
        ]
    CborUpdateCommitteeMessageResp { plutusData } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborUpdateCommitteeMessage"
        , "cborHexUpdateCommitteeMessage" /\ J.fromString
            (serialisePlutusDataToHex plutusData)
        ]
    CborMerkleRootInsertionMessageResp { plutusData } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborMerkleRootInsertionMessage"
        , "cborHexMerkleRootInsertionMessage" /\ J.fromString
            (serialisePlutusDataToHex plutusData)
        ]
    CborBlockProducerRegistrationMessageResp { plutusData } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborBlockProducerRegistrationMessage"
        , "cborHexBlockProducerRegistrationMessage" /\ J.fromString
            (serialisePlutusDataToHex plutusData)
        ]
    CborMerkleTreeEntryResp { plutusData } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborMerkleTreeEntry"
        , "cborHexMerkleTreeEntry" /\ J.fromString
            (serialisePlutusDataToHex plutusData)
        ]
    CborMerkleTreeResp
      { merkleRootHash
      , merkleTree
      } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborMerkleTree"
        , "rawHexMerkleRoot" /\ J.fromString
            (byteArrayToHex (unRootHash merkleRootHash))
        , "cborHexMerkleTree" /\ J.fromString
            (serialisePlutusDataToHex merkleTree)
        ]
    CborCombinedMerkleProofResp
      { combinedMerkleProof
      } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborCombinedMerkleProof"
        , "cborHexCombinedMerkleProof" /\ J.fromString
            (serialisePlutusDataToHex combinedMerkleProof)
        ]
    CborPlainAggregatePublicKeysResp
      { aggregatedPublicKeys
      } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "CborPlainAggregatePublicKeys"
        , "cborHexPlainAggregatedPublicKeys" /\ J.fromString
            (serialisePlutusDataToHex aggregatedPublicKeys)
        ]

-- | Encode the endpoint response to a json object
encodeEndpointResp ∷ EndpointResp → J.Json
encodeEndpointResp = CA.encode endpointRespCodec

-- | Encode the endpoint response to a json encoded string
stringifyEndpointResp ∷ EndpointResp → String
stringifyEndpointResp = encodeEndpointResp >>> J.stringify
