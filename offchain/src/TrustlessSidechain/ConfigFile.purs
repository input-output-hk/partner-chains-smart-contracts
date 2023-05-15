module TrustlessSidechain.ConfigFile
  ( decodeCommitteeSignatures
  , decodeCommittee
  , decodeConfig
  , optExample
  , readJson
  , getCommitteeSignatures
  , getCommittee
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Codec.Argonaut as CA
import Data.List (List)
import Data.UInt as UInt
import Effect.Exception as Exception
import Node.Buffer.Class as Buff
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (exists, readFile, readTextFile)
import Node.Path (FilePath)
import TrustlessSidechain.ConfigFile.Codecs
  ( committeeCodec
  , committeeSignaturesCodec
  , configCodec
  )
import TrustlessSidechain.Options.Types
  ( CommitteeInput(Committee, CommitteeFilePath)
  , CommitteeSignatures
  , CommitteeSignaturesInput(CommitteeSignatures, CommitteeSignaturesFilePath)
  , Config
  )
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey)

optExample ∷ Config
optExample =
  { sidechainParameters: Just
      { chainId: Just 1
      , genesisHash: Just $ hexToByteArrayUnsafe "genesisHash"
      , genesisUtxo: Just $ TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe "TxHash")
          , index: UInt.fromInt 2
          }
      , threshold: Just
          { numerator: 2
          , denominator: 3
          }
      }
  , paymentSigningKeyFile: Just "/absolute/path/to/payment.skey"
  , stakeSigningKeyFile: Nothing
  , runtimeConfig: Nothing
  }

decodeConfig ∷ J.Json → Either CA.JsonDecodeError Config
decodeConfig = CA.decode configCodec

decodeCommitteeSignatures ∷
  J.Json → Either CA.JsonDecodeError CommitteeSignatures
decodeCommitteeSignatures = CA.decode committeeSignaturesCodec

decodeCommittee ∷ J.Json → Either CA.JsonDecodeError (List SidechainPublicKey)
decodeCommittee = CA.decode committeeCodec

-- | `getCommitteeSignatures` grabs the committee from
-- | `CommitteeSignaturesInput` either by:
-- |    - just grabbing the committee provided; or
-- |    - doing the associated file IO to read the file / decode the json.
getCommitteeSignatures ∷ CommitteeSignaturesInput → Effect CommitteeSignatures
getCommitteeSignatures = case _ of
  CommitteeSignatures committee → pure committee
  CommitteeSignaturesFilePath filePath → do
    fileInput ← readTextFile ASCII filePath
    case jsonParser fileInput of
      Left errMsg → Exception.throw
        $ "Failed JSON parsing for committee signatures: "
        <> errMsg
      Right json → case decodeCommitteeSignatures json of
        Left err → Exception.throw
          $ "Failed decoding JSON committee signatures: "
          <> CA.printJsonDecodeError err
        Right committee → pure committee

-- | `getCommittee` grabs the committee from `CommitteeInput` either by
getCommittee ∷ CommitteeInput → Effect (List SidechainPublicKey)
getCommittee = case _ of
  Committee committee → pure committee
  CommitteeFilePath filePath → do
    -- Start of duplicated code from `getCommitteeSignatures` (only the
    -- decoder and error messages are different)
    fileInput ← readTextFile ASCII filePath
    case jsonParser fileInput of
      Left errMsg → Exception.throw
        $ "Failed JSON parsing for committee: "
        <> errMsg
      Right json → case decodeCommittee json of
        Left err → Exception.throw
          $ "Failed decoding JSON committee: "
          <> CA.printJsonDecodeError err
        Right committee → pure committee

-- End of duplicated code from `getCommitteeSignatures`

readJson ∷ FilePath → Effect (Either String J.Json)
readJson path = do
  hasConfig ← exists path
  if hasConfig then do
    file ← Buff.toString ASCII =<< readFile path
    pure $ jsonParser file
  else
    pure $ Left "No configuration file found."
