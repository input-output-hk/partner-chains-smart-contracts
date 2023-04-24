{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

{- | "Ctl" defines a monad which allows one to conveniently call CLI ctl
 commands / generate required data.
-}
module Ctl (
  CtlRegistration (..),
  CtlDeregistration (..),
  CtlUpdateCommitteeHash (..),
  CtlSaveRoot (..),
  CtlInitSidechain (..),
  CtlClaim (..),
  CtlCommon (..),
  ctlCommonFlags,
  ctlInitSidechainFlags,
  ctlRegistrationFlags,
  ctlDeregistrationFlags,
  ctlUpdateCommitteeHash,
  ctlSaveRootFlags,
  ctlClaimFlags,
  generateFreshCommittee,
) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class (SignKeyDSIGN)
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO.Class
import Crypto.Secp256k1 qualified as SECP
import Crypto.Secp256k1 qualified as Secp256k1
import Data.List qualified as List
import Plutus.V2.Ledger.Api (TxOutRef)
import TrustlessSidechain.MerkleTree (RootHash, unRootHash)
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg (BlockProducerRegistrationMsg),
  CombinedMerkleProof,
  MerkleRootInsertionMessage (MerkleRootInsertionMessage),
  SidechainParams (SidechainParams),
  SidechainPubKey,
  UpdateCommitteeHashMessage (UpdateCommitteeHashMessage),
  bprmInputUtxo,
  bprmSidechainParams,
  bprmSidechainPubKey,
  chainId,
  genesisHash,
  genesisUtxo,
  mrimMerkleRoot,
  mrimPreviousMerkleRoot,
  mrimSidechainParams,
  thresholdDenominator,
  thresholdNumerator,
  uchmNewCommitteePubKeys,
  uchmPreviousMerkleRoot,
  uchmSidechainEpoch,
  uchmSidechainParams,
 )
import Prelude

-- * Various product types to represent the parameters needed for the corresponding ctl command

--  | 'CtlRegistration' provides a wrapper type for the parameters required
--  to register an spo
data CtlRegistration = CtlRegistration
  { crSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
  , crSidechainPrvKey :: Secp256k1.SecKey
  , crRegistrationUtxo :: TxOutRef
  }

--  | 'CtlDeregistration' provides a wrapper type for the parameters required
--  to deregister an spo
data CtlDeregistration = CtlDeregistration
  { cdrSpoPubKey :: VerKeyDSIGN Ed25519DSIGN
  }

--  | 'CtlUpdateCommitteeHash' provides a wrapper type for the parameters required
--  to update the committee hash
data CtlUpdateCommitteeHash = CtlUpdateCommitteeHash
  { cuchCurrentCommitteePrvKeys :: [Secp256k1.SecKey]
  , cuchNewCommitteePubKeys :: [SidechainPubKey]
  , cuchSidechainEpoch :: Integer
  , cuchPreviousMerkleRoot :: Maybe RootHash
  }

--  | 'CtlSaveRoot' provides a wrapper type for the parameters required
--  to save a merkle root
data CtlSaveRoot = CtlSaveRoot
  { csrMerkleRoot :: RootHash
  , csrCurrentCommitteePrivKeys :: [Secp256k1.SecKey]
  , csrPreviousMerkleRoot :: Maybe RootHash
  }

--  | 'CtlInitSidechain' provides a wrapper type for the parameters required
--  to inialise a sidechain
data CtlInitSidechain = CtlInitSidechain
  { cisInitCommitteePubKeys :: [SidechainPubKey]
  , cisSidechainEpoch :: Integer
  }

--  | 'CtlClaim' provides a wrapper type for the parameters required
--  to claim tokens
newtype CtlClaim = CtlClaim
  { ccCombinedMerkleProof :: CombinedMerkleProof
  }

--  | 'CtlBurn' provides a wrapper type for the parameters required
--  to burn tokens
-- TODO: put this in later.
-- @
-- data CtlBurn = CtlBurn
--   { cbAmount :: Integer
--   , cbRecipient :: BuiltinByteString
--   }
-- @

-- * Functions for generating ctl commands

--

{- $ctlFlags
 These functions provide a means to generate the flags for each CTL command.
 As an example use case, if we wanted to generate the complete CLI command
 for intializing the Sidechain, we'd type something like:
 @
 Data.List.intercalate " "
  $ concat
      [ [ "nix run .#sidechain-main-cli --" ]
      , ctlInitSidechainFlags (CtlInitSidechain{ {\- initalize this.. -\} })
      , ctlCommonFlags (CtlCommon{ {\- initalize this.. -\} })
      ]
 @
-}

--  | 'CtlCommon' provides the data of required flags for every CTL command.
data CtlCommon = CtlCommon
  { -- | 'ccSigningKeyFile' is the 'FilePath' to the signing key
    ccSigningKeyFile :: FilePath
  , -- | 'ccSidechainParams' are the sidechain parameters
    ccSidechainParams :: SidechainParams
  }

{- | 'ctlCommonFlags' generates the CLI flags that corresponds to sidechain
 parameters
-}
ctlCommonFlags :: CtlCommon -> [String]
ctlCommonFlags CtlCommon {..} =
  let SidechainParams {..} = ccSidechainParams
   in map
        List.unwords
        [ ["--payment-signing-key-file", ccSigningKeyFile]
        , ["--genesis-committee-hash-utxo", OffChain.showTxOutRef genesisUtxo]
        , ["--sidechain-id", show chainId]
        , ["--sidechain-genesis-hash", OffChain.showGenesisHash genesisHash]
        , ["--threshold", OffChain.showThreshold thresholdNumerator thresholdDenominator]
        ]

{- | 'ctlInitSidechainFlags' generates the CLI flags that corresponds to init
 sidechain command
-}
ctlInitSidechainFlags :: CtlInitSidechain -> [String]
ctlInitSidechainFlags CtlInitSidechain {..} =
  map List.unwords $
    mappend
      [ ["init"]
      , ["--sidechain-epoch", show cisSidechainEpoch]
      ]
      $ flip map cisInitCommitteePubKeys $
        \pubKey ->
          ["--committee-pub-key", OffChain.showScPubKey pubKey]

{- | 'ctlRegistrationFlags' generates the CLI flags that corresponds to register
 command
-}
ctlRegistrationFlags :: SidechainParams -> CtlRegistration -> [String]
ctlRegistrationFlags scParams CtlRegistration {..} =
  let msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = scParams
          , bprmSidechainPubKey = OffChain.toSidechainPubKey crSidechainPrvKey
          , bprmInputUtxo = crRegistrationUtxo
          }
   in map
        List.unwords
        [ ["register"]
        , ["--sidechain-public-key", OffChain.showScPubKey $ OffChain.toSidechainPubKey crSidechainPrvKey]
        , ["--spo-signature", OffChain.showSig $ OffChain.signWithSPOKey crSpoPrivKey msg]
        , ["--sidechain-signature", OffChain.showSig $ OffChain.signWithSidechainKey crSidechainPrvKey msg]
        , ["--registration-utxo", OffChain.showTxOutRef crRegistrationUtxo]
        ]

{- | 'ctlDeregistrationFlags' generates the CLI flags that corresponds to deregister
 command
-}
ctlDeregistrationFlags :: CtlDeregistration -> [String]
ctlDeregistrationFlags CtlDeregistration {..} =
  map
    List.unwords
    [ ["deregister"]
    , ["--spo-public-key", OffChain.showPubKey $ OffChain.vKeyToSpoPubKey cdrSpoPubKey]
    ]

{- | 'ctlUpdateCommitteeHash' generates the CLI flags that corresponds to the
 update committee hash command
-}
ctlUpdateCommitteeHash :: SidechainParams -> CtlUpdateCommitteeHash -> [String]
ctlUpdateCommitteeHash scParams CtlUpdateCommitteeHash {..} =
  let msg =
        UpdateCommitteeHashMessage
          { uchmSidechainParams = scParams
          , uchmNewCommitteePubKeys = List.sort cuchNewCommitteePubKeys
          , uchmPreviousMerkleRoot = unRootHash <$> cuchPreviousMerkleRoot
          , uchmSidechainEpoch = cuchSidechainEpoch
          }
      currentCommitteePubKeysAndSigsFlags =
        map
          ( \sidechainPrvKey ->
              [ "--committee-pub-key-and-signature"
              , OffChain.showScPubKeyAndSig
                  (OffChain.toSidechainPubKey sidechainPrvKey)
                  (OffChain.signWithSidechainKey sidechainPrvKey msg)
              ]
          )
          cuchCurrentCommitteePrvKeys
      newCommitteeFlags =
        map
          ( \pubKey ->
              [ "--new-committee-pub-key"
              , OffChain.showScPubKey pubKey
              ]
          )
          cuchNewCommitteePubKeys
   in map List.unwords $
        [["committee-hash"]]
          ++ currentCommitteePubKeysAndSigsFlags
          ++ newCommitteeFlags
          ++ [["--sidechain-epoch", show cuchSidechainEpoch]]
          ++ maybe
            []
            (\bs -> [["--previous-merkle-root", OffChain.showBuiltinBS $ unRootHash bs]])
            cuchPreviousMerkleRoot

{- | 'ctlSaveRootFlags' generates the CLI flags that corresponds to the
 save root command
-}
ctlSaveRootFlags :: SidechainParams -> CtlSaveRoot -> [String]
ctlSaveRootFlags scParams CtlSaveRoot {..} =
  let msg =
        MerkleRootInsertionMessage
          { mrimSidechainParams = scParams
          , mrimMerkleRoot = unRootHash csrMerkleRoot
          , mrimPreviousMerkleRoot = fmap unRootHash csrPreviousMerkleRoot
          }
      currentCommitteePubKeysAndSigsFlags =
        map
          ( \sidechainPrvKey ->
              [ "--committee-pub-key-and-signature"
              , OffChain.showScPubKeyAndSig
                  (OffChain.toSidechainPubKey sidechainPrvKey)
                  (OffChain.signWithSidechainKey sidechainPrvKey msg)
              ]
          )
          csrCurrentCommitteePrivKeys
   in map List.unwords $
        [["save-root"]]
          ++ currentCommitteePubKeysAndSigsFlags
          ++ [["--merkle-root", OffChain.showBuiltinBS $ unRootHash csrMerkleRoot]]
          ++ maybe [] (\bs -> [["--previous-merkle-root", OffChain.showBuiltinBS $ unRootHash bs]]) csrPreviousMerkleRoot

{- | 'ctlClaimFlags' generates the CLI flags that corresponds to the
 claim command (minting FUEL)
-}
ctlClaimFlags :: CtlClaim -> [String]
ctlClaimFlags CtlClaim {..} =
  map
    List.unwords
    [ ["claim"]
    , ["--combined-proof", OffChain.showCombinedMerkleProof ccCombinedMerkleProof]
    ]

{- | 'ctlBurnFlags' generates the CLI flags that corresponds to the
 claim command (minting FUEL)
 TODO: Put this together later
 @
 ctlBurnFlags :: CtlBurn -> [String]
 ctlBurnFlags CtlBurn{..} =
     map List.unwords
         [ [ "burn" ]
         , [ "--combined-merkle-proof", OffChain.showCombinedMerkleProof ccCombinedMerkleProof]
         ]
 @
-}

-- * Some utility functions

-- | 'generateFreshCommittee' generates a fresh sidechain committee of the given size
generateFreshCommittee :: MonadIO m => Int -> m [(SECP.SecKey, SidechainPubKey)]
generateFreshCommittee n = IO.Class.liftIO $ do
  prvKeys <- Monad.replicateM n OffChain.generateRandomSecpPrivKey
  return $ map (\prvKey -> (prvKey, OffChain.toSidechainPubKey prvKey)) prvKeys
