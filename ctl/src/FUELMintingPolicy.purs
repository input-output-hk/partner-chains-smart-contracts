module FUELMintingPolicy
  ( CombinedMerkleProof(..)
  , FUELMint(..)
  , FuelParams(..)
  , MerkleTreeEntry(..)
  , fuelMintingPolicy
  , getFuelMintingPolicy
  , passiveBridgeMintParams
  , runFuelMP
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash(..)
  , StakePubKeyHash(..)
  , getNetworkId
  , ownPaymentPubKeyHash
  , toPubKeyHash
  , toStakingCredential
  )
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Hashing (blake2b256Hash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(..)
  , PlutusData(Constr)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints
  ( DatumPresence(..)
  , TxConstraint(..)
  , TxConstraints(..)
  , singleton
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, Value, getTokenName, mkTokenName)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import DistributedSet (dsConfValidator)
import DistributedSet as DistributedSet
import MPTRoot
  ( SignedMerkleRootMint(..)
  , findMptRootTokenUtxo
  , mptRootTokenValidator
  )
import MPTRoot.Utils as MPTRoot
import MerkleTree (MerkleProof(..), RootHash, rootMp, unRootHash)
import Plutus.Conversion.Address (fromPlutusAddress)
import RawScripts (rawFUELMintingPolicy)
import Serialization.Address (addressBytes)
import SidechainParams (SidechainParams)
import Types.Scripts (plutusV2Script)
import UpdateCommitteeHash (getCommitteeHashPolicy)
import Utils.Logging as Logging
import Utils.SerialiseData (serialiseData)

-- | `FUELMint` is the data type to parameterize the minting policy. See
-- | `mkMintingPolicy` for details of why we need the datum in `FUELMint`.
-- | `mptRootTokenCurrencySymbol` is the `CurrencySymbol` of a token
-- | which contains a merkle root in the `TokenName`. See
-- | `TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy` for details.
-- | `sidechainParams` is the sidechain parameters.
-- | `dsKeyCurrencySymbol` is th currency symbol for the tokens which
-- | hold the key for the distributed set. In particular, this allows the
-- | FUEL minting policy to verify if a string has _just been inserted_ into
-- | the distributed set.
-- `mptRootTokenValidator` is the hash of the validator script
-- which _should_ have a token which has the merkle root in the token
-- name. See `TrustlessSidechain.OnChain.MPTRootTokenValidator` for
-- details.
-- > mptRootTokenValidator :: ValidatorHash
-- N.B. We don't need this! We're really only interested in the token,
-- and indeed; anyone can pay a token to this script so there really
-- isn't a reason to use this validator script as the "identifier" for
-- MPTRootTokens.
newtype FUELMint = FUELMint
  { mptRootTokenCurrencySymbol ∷ CurrencySymbol
  , sidechainParams ∷ SidechainParams
  , dsKeyCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic FUELMint _
derive instance Newtype FUELMint _
instance ToData FUELMint where
  toData
    ( FUELMint
        { mptRootTokenCurrencySymbol, sidechainParams, dsKeyCurrencySymbol }
    ) =
    Constr zero
      [ toData mptRootTokenCurrencySymbol
      , toData sidechainParams
      , toData dsKeyCurrencySymbol
      ]

-- | `MerkleTreeEntry` (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
-- | for the MPTRootToken.
-- | `index`: 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
-- | `amount`: 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
-- | `recipient`: arbitrary length bytestring that represents decoded bech32 cardano address. See
-- | [here](https://cips.cardano.org/cips/cip19/) for more details of bech32.
-- | `previousMerkleRoot`: if a previous merkle root exists, used to ensure uniqueness of entries.
newtype MerkleTreeEntry = MerkleTreeEntry
  { index ∷ BigInt
  , amount ∷ BigInt
  , recipient ∷ ByteArray
  , previousMerkleRoot ∷ Maybe ByteArray
  }

instance FromData MerkleTreeEntry where
  fromData (Constr n [ a, b, c, d ]) | n == zero = ado
    index ← fromData a
    amount ← fromData b
    recipient ← fromData c
    previousMerkleRoot ← fromData d
    in MerkleTreeEntry { index, amount, recipient, previousMerkleRoot }
  fromData _ = Nothing

derive instance Generic MerkleTreeEntry _
derive instance Newtype MerkleTreeEntry _
instance ToData MerkleTreeEntry where
  toData
    ( MerkleTreeEntry
        { index, amount, recipient, previousMerkleRoot }
    ) =
    Constr zero
      [ toData index
      , toData amount
      , toData recipient
      , toData previousMerkleRoot
      ]

instance Show MerkleTreeEntry where
  show = genericShow

-- | `CombinedMerkleProof` contains both the `MerkleTreeEntry` and its
-- | corresponding `MerkleProof`. See #249 for details.
newtype CombinedMerkleProof = CombinedMerkleProof
  { transaction ∷ MerkleTreeEntry
  , merkleProof ∷ MerkleProof
  }

instance Show CombinedMerkleProof where
  show = genericShow

derive instance Generic CombinedMerkleProof _
derive instance Newtype CombinedMerkleProof _
instance ToData CombinedMerkleProof where
  toData
    ( CombinedMerkleProof
        { transaction, merkleProof }
    ) =
    Constr zero
      [ toData transaction
      , toData merkleProof
      ]

instance FromData CombinedMerkleProof where
  fromData (Constr n [ a, b ]) | n == zero = ado
    transaction ← fromData a
    merkleProof ← fromData b
    in CombinedMerkleProof { transaction, merkleProof }
  fromData _ = Nothing

data FUELRedeemer
  = MainToSide ByteArray -- recipient sidechain (addr , signature)
  | SideToMain MerkleTreeEntry MerkleProof

derive instance Generic FUELRedeemer _
instance ToData FUELRedeemer where
  toData (MainToSide s1) = Constr zero [ toData s1 ]
  toData (SideToMain s1 s2) = Constr one
    [ toData s1
    , toData s2
    ]

-- Applies SidechainParams to the minting policy
fuelMintingPolicy ∷ FUELMint → Contract () MintingPolicy
fuelMintingPolicy fm = do
  fuelMPUnapplied ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawFUELMintingPolicy
    PlutusScriptV2
  liftedE (applyArgs fuelMPUnapplied [ toData fm ])

-- | `getFuelMintingPolicy` creates the parameter `FUELMint`
-- | (as required by the onchain mintng policy) via the given sidechain params, and calls
-- | `fuelMintingPolicy` to give us the minting policy
getFuelMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
getFuelMintingPolicy sidechainParams = do
  { mptRootTokenMintingPolicyCurrencySymbol } ← getMptRootTokenPolicy
    sidechainParams
  { dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy sidechainParams

  fuelMintingPolicy $
    FUELMint
      { sidechainParams
      , mptRootTokenCurrencySymbol: mptRootTokenMintingPolicyCurrencySymbol
      , dsKeyCurrencySymbol: dsKeyPolicyCurrencySymbol
      }

data FuelParams
  = Mint
      { amount ∷ BigInt
      , recipient ∷ Address
      , merkleProof ∷ MerkleProof
      , sidechainParams ∷ SidechainParams
      , index ∷ BigInt
      , previousMerkleRoot ∷ Maybe ByteArray
      }
  | Burn { amount ∷ BigInt, recipient ∷ ByteArray }

runFuelMP ∷ SidechainParams → FuelParams → Contract () TransactionHash
runFuelMP sp fp = do
  let msg = Logging.mkReport { mod: "FUELMintingPolicy", fun: "runFuelMP" }

  fuelMP ← getFuelMintingPolicy sp

  { lookups, constraints } ← case fp of
    Burn params →
      burnFUEL fuelMP params
    Mint params →
      if isJust (unwrap sp).genesisMint then
        mintFUEL fuelMP params
      else
        claimFUEL fuelMP params

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceAndSignTxE ubTx)
  txId ← submit bsTx
  logInfo' $ msg ("Submitted Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "Tx submitted successfully!"

  pure txId

-- | Mint FUEL tokens using the Passive Bridge configuration, consuming the genesis utxo
-- | This minting strategy allows only one mint per sidechain, and will be deprecated once the
-- | active bridge claim script is stablisied
mintFUEL ∷
  MintingPolicy →
  { amount ∷ BigInt
  , recipient ∷ Address
  , merkleProof ∷ MerkleProof
  , sidechainParams ∷ SidechainParams
  , index ∷ BigInt
  , previousMerkleRoot ∷ Maybe ByteArray
  } →
  Contract ()
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mintFUEL
  fuelMP
  { amount, recipient, merkleProof, sidechainParams, index, previousMerkleRoot } =
  do
    let msg = Logging.mkReport { mod: "FUELMintingPolicy", fun: "mintFUEL" }
    ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
    netId ← getNetworkId

    cs ← liftContractM (msg "Cannot get currency symbol") $
      Value.scriptCurrencySymbol fuelMP
    tn ← liftContractM (msg "Cannot get token name")
      (Value.mkTokenName =<< byteArrayFromAscii "FUEL")

    recipientPkh ←
      liftContractM (msg "Couldn't derive payment public key hash from address")
        $ PaymentPubKeyHash
        <$> toPubKeyHash recipient

    let recipientSt = toStakePubKeyHash recipient

    -- Find the passive bridge genesis mint utxo
    let inputTxIn = (unwrap sidechainParams).genesisMint
    inputUtxo ← inputTxIn # traverse \txIn → do
      txOut ← liftedM (msg "Cannot find genesis mint UTxO") $ getUtxo txIn
      pure $ Map.singleton txIn $ TransactionOutputWithRefScript
        { output: txOut, scriptRef: Nothing }

    let
      value = Value.singleton cs tn amount
      redeemer = wrap (toData (SideToMain merkleTreeEntry merkleProof))
      merkleTreeEntry = MerkleTreeEntry
        { recipient: unwrap (addressBytes (fromPlutusAddress netId recipient))
        , previousMerkleRoot
        , amount
        , index
        }

    pure
      { lookups: Lookups.mintingPolicy fuelMP
          <> maybe mempty Lookups.unspentOutputs inputUtxo
      , constraints: Constraints.mustMintValueWithRedeemer redeemer value
          <> mustPayToPubKeyAddress recipientPkh recipientSt value
          <> Constraints.mustBeSignedBy ownPkh
          <> maybe mempty Constraints.mustSpendPubKeyOutput inputTxIn
      }

-- | Mint FUEL tokens using the Active Bridge configuration, verifying the Merkle proof
claimFUEL ∷
  MintingPolicy →
  { amount ∷ BigInt
  , recipient ∷ Address
  , merkleProof ∷ MerkleProof
  , sidechainParams ∷ SidechainParams
  , index ∷ BigInt
  , previousMerkleRoot ∷ Maybe ByteArray
  } →
  Contract ()
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
claimFUEL
  fuelMP
  { amount, recipient, merkleProof, sidechainParams, index, previousMerkleRoot } =
  do
    let msg = Logging.mkReport { mod: "FUELMintingPolicy", fun: "mintFUEL" }
    ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
    netId ← getNetworkId

    cs ← liftContractM (msg "Cannot get currency symbol") $
      Value.scriptCurrencySymbol fuelMP
    tn ← liftContractM (msg "Cannot get token name")
      (Value.mkTokenName =<< byteArrayFromAscii "FUEL")

    ds ← DistributedSet.getDs sidechainParams

    let
      merkleTreeEntry =
        MerkleTreeEntry
          { index
          , amount
          , previousMerkleRoot
          , recipient: unwrap (addressBytes (fromPlutusAddress netId recipient))
          }

    entryBytes ← liftContractM (msg "Cannot serialise merkle tree entry")
      $ serialiseData
      $ toData
          merkleTreeEntry
    let
      rootHash = rootMp entryBytes merkleProof

    cborMteHashedTn ← liftContractM (msg "Token name exceeds size limet")
      $ mkTokenName
      $ blake2b256Hash entryBytes

    { index: mptUtxo, value: mptUtxoOut } ←
      liftContractM
        (msg "Couldn't find the parent Merkle tree root hash of the transaction")
        =<< findMptRootTokenUtxoByRootHash sidechainParams rootHash

    { inUtxo:
        { nodeRef
        , oNode
        , datNode
        , tnNode
        }
    , nodes: DistributedSet.Ib { unIb: nodeA /\ nodeB }
    } ← liftedM (msg "Couldn't find distributed set nodes") $
      DistributedSet.findDsOutput ds cborMteHashedTn

    { confRef, confO } ← DistributedSet.findDsConfOutput ds

    insertValidator ← DistributedSet.insertValidator ds
    let insertValidatorHash = Scripts.validatorHash insertValidator
    { dsKeyPolicy, dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy
      sidechainParams

    dsConfV ← dsConfValidator ds
    rootTokenValidator ← mptRootTokenValidator sidechainParams

    recipientPkh ←
      liftContractM (msg "Couldn't derive payment public key hash from address")
        $ PaymentPubKeyHash
        <$> toPubKeyHash recipient

    let recipientSt = toStakePubKeyHash recipient

    let
      node = DistributedSet.mkNode (getTokenName tnNode) datNode
      value = Value.singleton cs tn amount
      redeemer = wrap (toData (SideToMain merkleTreeEntry merkleProof))
      -- silence missing stake key warning

      mkNodeConstraints n = do
        nTn ← liftContractM "Couldn't convert node token name"
          $ mkTokenName
          $ (unwrap n).nKey

        let val = Value.singleton dsKeyPolicyCurrencySymbol nTn (BigInt.fromInt 1)
        if getTokenName nTn == (unwrap node).nKey then
          pure $ Constraints.mustPayToScript
            insertValidatorHash
            (Datum (toData (DistributedSet.nodeToDatum n)))
            DatumInline
            val
        else
          pure
            $ Constraints.mustPayToScript
                insertValidatorHash
                (Datum (toData (DistributedSet.nodeToDatum n)))
                DatumInline
                val
            <> Constraints.mustMintValue val

    mustAddDSNodeA ← mkNodeConstraints nodeA
    mustAddDSNodeB ← mkNodeConstraints nodeB

    pure
      { lookups:
          Lookups.mintingPolicy fuelMP
            <> Lookups.validator rootTokenValidator
            <> Lookups.unspentOutputs (Map.singleton mptUtxo mptUtxoOut)

            <> Lookups.validator dsConfV
            <> Lookups.unspentOutputs (Map.singleton confRef confO)

            <> Lookups.mintingPolicy dsKeyPolicy
            <> Lookups.validator insertValidator
            <> Lookups.unspentOutputs (Map.singleton nodeRef oNode)

      , constraints:
          Constraints.mustMintValueWithRedeemer redeemer value -- minting the FUEL

            <> mustPayToPubKeyAddress recipientPkh recipientSt value
            <> Constraints.mustBeSignedBy ownPkh
            <> Constraints.mustReferenceOutput mptUtxo
            <> Constraints.mustReferenceOutput confRef

            <> Constraints.mustSpendScriptOutput nodeRef unitRedeemer

            <> mustAddDSNodeA
            <> mustAddDSNodeB
      }

burnFUEL ∷
  MintingPolicy →
  { amount ∷ BigInt, recipient ∷ ByteArray } →
  Contract ()
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
burnFUEL fuelMP { amount, recipient } = do
  let msg = Logging.mkReport { mod: "FUELMintingPolicy", fun: "burnFUEL" }

  cs ← liftContractM (msg "Cannot get currency symbol") $
    Value.scriptCurrencySymbol fuelMP
  logInfo' $ msg ("fuelMP currency symbol: " <> show cs)
  tn ← liftContractM (msg "Cannot get token name")
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")

  let
    value = Value.singleton cs tn (-amount)
    redeemer = wrap (toData (MainToSide recipient))
  pure
    { lookups: Lookups.mintingPolicy fuelMP
    , constraints: Constraints.mustMintValueWithRedeemer redeemer value
    }

-- | Mocking unused data for Passive Bridge minting, where we use genesis minting
passiveBridgeMintParams ∷
  SidechainParams →
  { amount ∷ BigInt, recipient ∷ Address } →
  FuelParams
passiveBridgeMintParams sidechainParams { amount, recipient } =
  Mint
    { amount
    , recipient
    , sidechainParams
    , merkleProof: MerkleProof []
    , index: BigInt.fromInt 0
    , previousMerkleRoot: Nothing
    }

-- TODO: refactor to utility module
findMptRootTokenUtxoByRootHash ∷
  SidechainParams →
  RootHash →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMptRootTokenUtxoByRootHash sidechainParams rootHash = do
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy sidechainParams

  -- Then, we get the mpt root token minting policy..
  let

    msg = Logging.mkReport
      { mod: "FUELMintingPolicy", fun: "findMptRootTokenUtxoByRootHash" }
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  merkleRootTokenName ←
    liftContractM
      (msg "Invalid merkle root TokenName for mptRootTokenMintingPolicy")
      $ Value.mkTokenName
      $ unRootHash rootHash
  findMptRootTokenUtxo merkleRootTokenName smrm

-- | 'getMptRootTokenPolicy' grabs the mpt root token policy and currency
-- | symbol (potentially throwing an error if this is not possible).
-- TODO: refactor to utility module
getMptRootTokenPolicy ∷
  SidechainParams →
  Contract
    ()
    { mptRootTokenMintingPolicy ∷ MintingPolicy
    , mptRootTokenMintingPolicyCurrencySymbol ∷ CurrencySymbol
    }
getMptRootTokenPolicy sidechainParams = do
  let
    msg = Logging.mkReport
      { mod: "FUELMintingPolicy", fun: "getMptRootTokenPolicy" }

  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy sidechainParams

  -- Then, we get the mpt root token minting policy..
  mptRootTokenMintingPolicy ← MPTRoot.mptRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  mptRootTokenMintingPolicyCurrencySymbol ←
    liftContractM
      (msg "Failed to get mptRootToken CurrencySymbol")
      $ Value.scriptCurrencySymbol mptRootTokenMintingPolicy

  pure { mptRootTokenMintingPolicy, mptRootTokenMintingPolicyCurrencySymbol }

toStakePubKeyHash ∷ Address → Maybe StakePubKeyHash
toStakePubKeyHash addr =
  case toStakingCredential addr of
    Just (StakingHash (PubKeyCredential pkh)) → Just (StakePubKeyHash pkh)
    _ → Nothing

mustPayToPubKeyAddress ∷
  PaymentPubKeyHash → Maybe StakePubKeyHash → Value → TxConstraints Void Void
mustPayToPubKeyAddress pkh mStPkh =
  singleton <<< MustPayToPubKeyAddress pkh mStPkh Nothing Nothing
