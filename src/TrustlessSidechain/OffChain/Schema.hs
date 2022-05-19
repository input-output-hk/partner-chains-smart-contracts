module TrustlessSidechain.OffChain.Schema where

import Plutus.Contract (Endpoint, type (.\/))
import TrustlessSidechain.OffChain.Types (BurnParams, DeregisterParams, MintParams, RegisterParams)

type TrustlessSidechainSchema =
  Endpoint "register" RegisterParams
    .\/ Endpoint "deregister" DeregisterParams
    .\/ Endpoint "burn" BurnParams
    .\/ Endpoint "mint" MintParams
