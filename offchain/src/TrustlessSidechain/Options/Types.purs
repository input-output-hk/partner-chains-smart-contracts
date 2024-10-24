module TrustlessSidechain.Options.Types
  ( Config(..)
  , InputArgOrFile(..)
  , Options(..)
  , RuntimeConfig(..)
  , SidechainEndpointParams(..)
  , TxEndpoint(..)
  ) where

import Contract.Prelude

import Cardano.Types.Asset (Asset)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.NetworkId (NetworkId)
import Contract.Config (ContractParams, ServerConfig)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Data.List (List)
import JS.BigInt (BigInt)
import Node.Path (FilePath)
import TrustlessSidechain.CommitteeCandidateValidator
  ( StakeOwnership
  )
import TrustlessSidechain.GetSidechainAddresses (SidechainAddressesExtra)
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings
  , MutableReserveSettings
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (PubKey)

-- | `SidechainEndpointParams` is an offchain type for grabbing information
-- | related to the sidechain.
-- | This is essentially `SidechainParams` with a little bit more information.
newtype SidechainEndpointParams = SidechainEndpointParams
  { sidechainParams :: SidechainParams
  }

derive instance Newtype SidechainEndpointParams _

-- | CLI arguments providing an interface to contract endpoints
data Options
  =
    -- | `TxOptions` are the options for endpoints which build / submit a
    -- | transaction to the blockchain.
    -- | In particular, these endpoints need to be in the `Contract` monad
    TxOptions
      { sidechainEndpointParams :: SidechainEndpointParams
      , endpoint :: TxEndpoint
      , contractParams :: ContractParams
      }
  | CLIVersion

-- | Sidechain configuration file including common parameters.
-- Any parameter can be set `null` requiring a CLI argument instead
type Config =
  { -- | Sidechain parameters (defining the sidechain which we will interact with)
    sidechainParameters ::
      Maybe
        { chainId :: Maybe Int
        , genesisUtxo :: Maybe TransactionInput
        , threshold ::
            Maybe
              { numerator :: Int
              , denominator :: Int
              }
        -- governanceAuthority should really be a PubKeyHash but there's no
        -- (easy) way of pulling a dummy PubKeyHash value out of thin air in
        -- TrustlessSidechain.ConfigFile.optExample
        , governanceAuthority :: Maybe ByteArray
        }
  , -- | Filepath of the payment signing key of the wallet owner
    paymentSigningKeyFile :: Maybe FilePath
  , -- | Filepath of the stake signing key of the wallet owner
    stakeSigningKeyFile :: Maybe FilePath
  , -- | Network configuration of the runtime dependencies (kupo, ogmios)
    runtimeConfig :: Maybe RuntimeConfig
  }

-- | Data for CLI endpoints which submit a transaction to the blockchain.
data TxEndpoint
  = CommitteeCandidateReg
      { stakeOwnership :: StakeOwnership
      , sidechainPubKey :: ByteArray
      , sidechainSig :: ByteArray
      , inputUtxo :: TransactionInput
      , auraKey :: ByteArray
      , grandpaKey :: ByteArray
      }
  | CommitteeCandidateDereg { spoPubKey :: Maybe PubKey }
  | GetAddrs
      SidechainAddressesExtra
  | InitTokensMint
      { version :: Int }
  | InitReserveManagement
      { version :: Int
      }

  -- See Note [Supporting version insertion beyond version 2]
  | InsertVersion2
  | UpdateVersion
      { oldVersion :: Int
      , newVersion :: Int
      }
  | InvalidateVersion
      { version :: Int
      }
  | ListVersionedScripts
      { version :: Int
      }
  | InsertDParameter
      { permissionedCandidatesCount :: BigInt
      , registeredCandidatesCount :: BigInt
      }
  | UpdateDParameter
      { permissionedCandidatesCount :: BigInt
      , registeredCandidatesCount :: BigInt
      }
  | UpdatePermissionedCandidates
      { permissionedCandidatesToAdd ::
          List
            { sidechainKey :: ByteArray
            , auraKey :: ByteArray
            , grandpaKey :: ByteArray
            }
      , permissionedCandidatesToRemove ::
          Maybe
            ( List
                { sidechainKey :: ByteArray
                , auraKey :: ByteArray
                , grandpaKey :: ByteArray
                }
            )
      }

  -- | reserve initialization for an asset class
  | InitTokenStatus

  -- | CLI entpoints for reserve initialization for an asset class
  | CreateReserve
      { mutableReserveSettings :: MutableReserveSettings
      , immutableReserveSettings :: ImmutableReserveSettings
      , depositAmount :: BigNum
      }

  -- | update of a reserve
  | UpdateReserveSettings
      { mutableReserveSettings :: MutableReserveSettings }

  -- | deposit to a reserve
  | DepositReserve
      { asset :: Asset
      , depositAmount :: BigNum
      }

  -- | transfer from a reserve to illiquid circulation supply
  | ReleaseReserveFunds
      { totalAccruedTillNow :: Int
      , transactionInput :: TransactionInput
      }

  -- | handover of a reserve
  | HandoverReserve

-- | `InputArgOrFile` represents that we may either allow an option as input
-- | via a CLI argument or a filepath of a JSON file
data InputArgOrFile (a :: Type)
  = InputFromArg a
  | InputFromFile FilePath

-- | Network configuration of the runtime dependencies
-- Any parameter can be set `null` falling back to its default value
type RuntimeConfig =
  { ogmios :: Maybe ServerConfig
  , kupo :: Maybe ServerConfig
  , network :: Maybe NetworkId
  }
