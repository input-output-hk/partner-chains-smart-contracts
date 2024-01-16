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
import TrustlessSidechain.FUELMintingPolicy.V1
  ( CombinedMerkleProof
  )
import TrustlessSidechain.GetSidechainAddresses (SidechainAddresses)
import TrustlessSidechain.MerkleTree
  ( MerkleTree
  , RootHash
  , unRootHash
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (currencySymbolToHex)
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
  = ClaimActRespV1 { transactionId ∷ ByteArray }
  | BurnActRespV1 { transactionId ∷ ByteArray }
  | ClaimActRespV2 { transactionId ∷ ByteArray }
  | BurnActRespV2 { transactionId ∷ ByteArray }
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
      , versioningTransactionIds ∷ Array ByteArray
      , sidechainParams ∷ SidechainParams
      , sidechainAddresses ∷ SidechainAddresses
      }
  | InitResp
      { transactionId ∷ ByteArray
      , versioningTransactionIds ∷ Array ByteArray
      , sidechainParams ∷ SidechainParams
      , sidechainAddresses ∷ SidechainAddresses
      }
  | SaveCheckpointResp { transactionId ∷ ByteArray }
  | InsertVersionResp { versioningTransactionIds ∷ Array ByteArray }
  | UpdateVersionResp { versioningTransactionIds ∷ Array ByteArray }
  | InvalidateVersionResp { versioningTransactionIds ∷ Array ByteArray }
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
  | InsertDParameterResp
      { transactionId ∷ ByteArray }
  | UpdateDParameterResp
      { transactionId ∷ ByteArray }
  | RemoveDParameterResp
      { transactionId ∷ ByteArray }
  | UpdatePermissionedCandidatesResp
      { transactionId ∷ ByteArray }
  | BurnNFTsResp
      { transactionId ∷ ByteArray }

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
    ClaimActRespV1 { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "ClaimActV1"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    BurnActRespV1 { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "BurnActV1"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    ClaimActRespV2 { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "ClaimActV2"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    BurnActRespV2 { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "BurnActV2"
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
              ( currencySymbolToHex candidatePermissionCurrencySymbol
              )
        ]
    GetAddrsResp { sidechainAddresses } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "GetAddrs"
        , "addresses" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.addresses
                )
            )
        , "validatorHashes" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.validatorHashes
                )
            )
        , "mintingPolicies" /\ J.fromObject
            ( Object.fromFoldable
                ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                    sidechainAddresses.mintingPolicies
                )
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
    InitTokensResp
      { transactionId
      , sidechainParams
      , sidechainAddresses
      , versioningTransactionIds
      } →
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "Init"
          , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
          , "versioningTransactionIds" /\ J.fromArray
              (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
          , "sidechainParams" /\ CA.encode scParamsCodec sidechainParams
          , "addresses" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.addresses
                  )
              )
          , "validatorHashes" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.validatorHashes
                  )
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.mintingPolicies
                  )
              )
          ]
    InitResp
      { transactionId
      , versioningTransactionIds
      , sidechainParams
      , sidechainAddresses
      } →
      J.fromObject $
        Object.fromFoldable
          [ "endpoint" /\ J.fromString "Init"
          , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
          , "versioningTransactionIds" /\ J.fromArray
              (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
          , "sidechainParams" /\ CA.encode scParamsCodec sidechainParams
          , "addresses" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.addresses
                  )
              )
          , "validatorHashes" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.validatorHashes
                  )
              )
          , "mintingPolicies" /\ J.fromObject
              ( Object.fromFoldable
                  ( map ((\(a /\ b) → show a /\ b) >>> rmap J.fromString)
                      sidechainAddresses.mintingPolicies
                  )
              )
          ]
    SaveCheckpointResp { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "SaveCheckpoint"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]
    InsertVersionResp { versioningTransactionIds } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InitVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
        ]
    UpdateVersionResp { versioningTransactionIds } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdateVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
        ]
    InvalidateVersionResp { versioningTransactionIds } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InvalidateVersion"
        , "versioningTransactionIds" /\ J.fromArray
            (map (J.fromString <<< byteArrayToHex) versioningTransactionIds)
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
    InsertDParameterResp
      { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "InsertDParameter"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    UpdateDParameterResp
      { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdateDParameter"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    RemoveDParameterResp
      { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "RemoveDParameter"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    UpdatePermissionedCandidatesResp
      { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "UpdatePermissionedCandidates"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

    BurnNFTsResp
      { transactionId } →
      J.fromObject $ Object.fromFoldable
        [ "endpoint" /\ J.fromString "BurnNFTs"
        , "transactionId" /\ J.fromString (byteArrayToHex transactionId)
        ]

-- | Encode the endpoint response to a json object
encodeEndpointResp ∷ EndpointResp → J.Json
encodeEndpointResp = CA.encode endpointRespCodec

-- | Encode the endpoint response to a json encoded string
stringifyEndpointResp ∷ EndpointResp → String
stringifyEndpointResp = encodeEndpointResp >>> J.stringify
