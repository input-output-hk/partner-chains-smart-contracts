module TrustlessSidechain.OffChain.Schema where

import TrustlessSidechain.OffChain.Types (RegisterParams, DeregisterParams)
import Plutus.Contract (Endpoint, type (.\/))

type CommitteeCandidateRegistrySchema =
  Endpoint "register" RegisterParams .\/ Endpoint "deregister" DeregisterParams

