module TrustlessSidechain.OffChain.Schema where

import Plutus.Contract (Endpoint, type (.\/))
import TrustlessSidechain.OffChain.Types (DeregisterParams, RegisterParams)

type CommitteeCandidateRegistrySchema =
  Endpoint "register" RegisterParams .\/ Endpoint "deregister" DeregisterParams
