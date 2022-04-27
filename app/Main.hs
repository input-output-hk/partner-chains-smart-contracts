{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import BotPlutusInterface qualified
import BotPlutusInterface.Types (
  CLILocation (Local),
  HasDefinitions (..),
  LogLevel (Debug),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
 )
import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Data.Aeson qualified as JSON
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Http))
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  CommitteeCandidateRegistrySchema,
  DeregisterParams,
  RegisterParams,
  deregister,
  register,
 )
import Prelude

instance HasDefinitions TrustlessSidechainContracts where
  getDefinitions :: [TrustlessSidechainContracts]
  getDefinitions = []

  getSchema :: TrustlessSidechainContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @CommitteeCandidateRegistrySchema

  getContract :: (TrustlessSidechainContracts -> SomeBuiltin)
  getContract = \case
    RegisterCommitteeCandidate params -> SomeBuiltin $ register params
    DeregisterCommitteeCandidate params -> SomeBuiltin $ deregister params

data TrustlessSidechainContracts
  = RegisterCommitteeCandidate RegisterParams
  | DeregisterCommitteeCandidate DeregisterParams
  deriving stock (Show)

$(deriveJSON defaultOptions ''TrustlessSidechainContracts)

main :: IO ()
main = do
  protocolParams <-
    fromMaybe (error "protocol.json file not found") . JSON.decode
      <$> LazyByteString.readFile "protocol.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 1097911063)
          , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
          , pcPort = 9080
          , pcProtocolParams = protocolParams
          , pcTipPollingInterval = 10_000_000
          , pcSlotConfig = def
          , pcOwnPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
          , pcOwnStakePubKeyHash = Nothing
          , pcScriptFileDir = "./data"
          , pcSigningKeyFileDir = "./signing-keys"
          , pcTxFileDir = "./txs"
          , pcDryRun = False
          , pcLogLevel = Debug
          , pcProtocolParamsFile = "./protocol.json"
          , pcForceBudget = Just (8_000_000, 40000)
          , pcEnableTxEndpoint = True
          }
  BotPlutusInterface.runPAB @TrustlessSidechainContracts pabConf
