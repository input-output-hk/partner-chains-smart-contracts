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
import TrustlessSidechain.OffChain.CommitteeCandidateValidator (deregister, registerWithMock)
import TrustlessSidechain.OffChain.FUELMintingPolicy (burn, mint)
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (BurnParams, DeregisterParams, MintParams, RegisterParams, UpdateCommitteeHashParams)
import TrustlessSidechain.OffChain.UpdateCommitteeHash (updateCommitteeHash)

import Prelude

instance HasDefinitions TrustlessSidechainContracts where
  getDefinitions :: [TrustlessSidechainContracts]
  getDefinitions = []

  getSchema :: TrustlessSidechainContracts -> [FunctionSchema FormSchema]
  getSchema _ = endpointsToSchemas @TrustlessSidechainSchema

  getContract :: (TrustlessSidechainContracts -> SomeBuiltin)
  getContract = \case
    RegisterCommitteeCandidate params -> SomeBuiltin $ registerWithMock params
    DeregisterCommitteeCandidate params -> SomeBuiltin $ deregister params
    MintFUELToken params -> SomeBuiltin $ mint params
    BurnFUELToken params -> SomeBuiltin $ burn params
    UpdateCommitteeHash params -> SomeBuiltin $ updateCommitteeHash params

data TrustlessSidechainContracts
  = RegisterCommitteeCandidate RegisterParams
  | DeregisterCommitteeCandidate DeregisterParams
  | MintFUELToken MintParams
  | BurnFUELToken BurnParams
  | UpdateCommitteeHash UpdateCommitteeHashParams
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
          , pcCollectStats = False
          , pcOwnPubKeyHash = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
          , pcOwnStakePubKeyHash = Nothing
          , pcScriptFileDir = "./data"
          , pcSigningKeyFileDir = "./signing-keys"
          , pcTxFileDir = "./txs"
          , pcDryRun = False
          , pcLogLevel = Debug
          , pcProtocolParamsFile = "./protocol.json"
          , pcEnableTxEndpoint = True
          }
  BotPlutusInterface.runPAB @TrustlessSidechainContracts pabConf
