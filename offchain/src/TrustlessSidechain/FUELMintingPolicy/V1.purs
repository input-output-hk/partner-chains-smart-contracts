module TrustlessSidechain.FUELMintingPolicy.V1
  ( MerkleTreeEntry(..)
  , CombinedMerkleProof(..)
  , FuelMintParams(..)
  , FUELMintingRedeemer(..)
  , mkMintFuelLookupAndConstraints
  , getFuelMintingPolicy
  , fuelTokenName
  , combinedMerkleProofToFuelParams
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  , toPubKeyHash
  , toStakingCredential
  )
import Contract.Credential
  ( Credential(PubKeyCredential)
  , StakingCredential(StakingHash)
  )
import Contract.Hashing (blake2b256Hash)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(Datum)
  , PlutusData(Constr)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , getTokenName
  , mkTokenName
  )
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Error
  ( InternalError(InvalidData, NotFoundUtxo, InvalidScript)
  , OffchainError(InternalError, InvalidInputError)
  )
import TrustlessSidechain.MerkleRoot
  ( findMerkleRootTokenUtxo
  )
import TrustlessSidechain.MerkleTree (MerkleProof, RootHash, rootMp, unRootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( Bech32Bytes
  , addressFromBech32Bytes
  , bech32BytesFromAddress
  , getOwnPaymentPubKeyHash
  )
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( FUELMintingPolicy
      , MerkleRootTokenPolicy
      , DsKeyPolicy
      )
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

fuelTokenName ∷ TokenName
fuelTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "FUEL"

-- | `MerkleTreeEntry` (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
-- | for the MerkleRootToken. It contains:
-- | - `index`: 32 bit unsigned integer, used to provide uniqueness among
-- | transactions within the tree
-- | - `amount`: 256 bit unsigned integer that represents amount of tokens
-- | being sent out of the bridge
-- | - `recipient`: arbitrary length bytestring that represents decoded bech32
-- | cardano address. See [here](https://cips.cardano.org/cips/cip19/) for more
-- | details of bech32.
-- | - `previousMerkleRoot`: if a previous merkle root exists, used to ensure
-- | uniqueness of entries.
newtype MerkleTreeEntry = MerkleTreeEntry
  { index ∷ BigInt
  , amount ∷ BigInt
  , recipient ∷ Bech32Bytes
  , previousMerkleRoot ∷ Maybe RootHash
  }

instance FromData MerkleTreeEntry where
  fromData (Constr n [ a, b, c, d ]) | n == BigNum.fromInt 0 = ado
    index ← fromData a
    amount ← fromData b
    recipient ← fromData c
    previousMerkleRoot ← fromData d
    in MerkleTreeEntry { index, amount, recipient, previousMerkleRoot }
  fromData _ = Nothing

derive instance Generic MerkleTreeEntry _

derive instance Newtype MerkleTreeEntry _

derive newtype instance Eq MerkleTreeEntry

instance ToData MerkleTreeEntry where
  toData
    ( MerkleTreeEntry
        { index, amount, recipient, previousMerkleRoot }
    ) =
    Constr (BigNum.fromInt 0)
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

derive newtype instance Eq CombinedMerkleProof

-- | `combinedMerkleProofToFuelParams` converts `SidechainParams` and
-- | `CombinedMerkleProof` to a `Mint` of `FuelParams`.
-- | This is a modestly convenient wrapper to help call the `runFuelMP `
-- | endpoint for internal tests.
combinedMerkleProofToFuelParams ∷
  { sidechainParams ∷ SidechainParams
  , combinedMerkleProof ∷ CombinedMerkleProof
  } →
  Maybe FuelMintParams
combinedMerkleProofToFuelParams
  { sidechainParams
  , combinedMerkleProof: CombinedMerkleProof { transaction, merkleProof }
  } = do
  let transaction' = unwrap transaction

  recipient ← addressFromBech32Bytes transaction'.recipient
  pure $ FuelMintParams
    { amount: transaction'.amount
    , recipient
    , merkleProof
    , sidechainParams
    , index: transaction'.index
    , previousMerkleRoot: transaction'.previousMerkleRoot
    , dsUtxo: Nothing
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
    Constr (BigNum.fromInt 0)
      [ toData transaction
      , toData merkleProof
      ]

instance FromData CombinedMerkleProof where
  fromData (Constr n [ a, b ]) | n == BigNum.fromInt 0 = ado
    transaction ← fromData a
    merkleProof ← fromData b
    in CombinedMerkleProof { transaction, merkleProof }
  fromData _ = Nothing

data FUELMintingRedeemer
  = FUELMintingRedeemer MerkleTreeEntry MerkleProof
  | FUELBurningRedeemer

derive instance Generic FUELMintingRedeemer _
instance ToData FUELMintingRedeemer where
  toData (FUELMintingRedeemer s1 s2) = Constr (BigNum.fromInt 0)
    [ toData s1
    , toData s2
    ]
  toData FUELBurningRedeemer = Constr (BigNum.fromInt 1) []

-- | Gets the FUELMintingPolicy by applying `FUELMint` to the FUEL minting
-- | policy
decodeFuelMintingPolicy ∷ SidechainParams → Contract MintingPolicy
decodeFuelMintingPolicy sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkMintingPolicyWithParams FUELMintingPolicy
    [ toData sidechainParams, toData versionOracleConfig ]

-- | `getFuelMintingPolicy` creates the parameter `FUELMint`
-- | (as required by the onchain mintng policy) via the given
-- | `SidechainParams`, and calls `fuelMintingPolicy` to give us the minting
-- | policy
getFuelMintingPolicy ∷
  SidechainParams →
  Contract
    { fuelMintingPolicy ∷ MintingPolicy
    , fuelMintingCurrencySymbol ∷ CurrencySymbol
    }
getFuelMintingPolicy sidechainParams = do
  fuelMintingPolicy ← decodeFuelMintingPolicy sidechainParams
  fuelMintingCurrencySymbol ←
    liftContractM (show (InvalidScript "Fuel V1 minting policy")) $
      Value.scriptCurrencySymbol fuelMintingPolicy
  pure { fuelMintingPolicy, fuelMintingCurrencySymbol }

-- | `FuelMintParams` is the data for the FUEL mint endpoint.
data FuelMintParams = FuelMintParams
  { amount ∷ BigInt
  , recipient ∷ Address
  , merkleProof ∷ MerkleProof
  , sidechainParams ∷ SidechainParams
  , index ∷ BigInt
  , previousMerkleRoot ∷ Maybe RootHash
  , dsUtxo ∷ Maybe TransactionInput
  }

-- | Mint FUEL tokens using the Active Bridge configuration, verifying the
-- | Merkle proof
mkMintFuelLookupAndConstraints ∷
  SidechainParams →
  FuelMintParams →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mkMintFuelLookupAndConstraints
  sp
  ( FuelMintParams
      { amount
      , recipient
      , merkleProof
      , sidechainParams
      , index
      , previousMerkleRoot
      , dsUtxo
      }
  ) =
  do
    ownPkh ← getOwnPaymentPubKeyHash

    { fuelMintingPolicy, fuelMintingCurrencySymbol } ← getFuelMintingPolicy sp

    ds ← DistributedSet.getDs (unwrap sidechainParams).genesisUtxo

    bech32BytesRecipient ←
      liftContractM
        ( show
            (InternalError (InvalidData "Cannot convert address to bech 32 bytes"))
        )
        $ bech32BytesFromAddress recipient
    let
      merkleTreeEntry =
        MerkleTreeEntry
          { index
          , amount
          , previousMerkleRoot
          , recipient: bech32BytesRecipient
          }

    let
      entryBytes = unwrap $ PlutusData.serializeData merkleTreeEntry
      cborMteHashed = blake2b256Hash entryBytes
      rootHash = rootMp entryBytes merkleProof

    cborMteHashedTn ←
      liftContractM
        (show (InternalError (InvalidData "Token name exceeds size limit")))
        $ mkTokenName
        $ cborMteHashed

    { index: mptUtxo, value: mptTxOut } ←
      liftContractM
        ( show
            ( InvalidInputError
                "Couldn't find the parent Merkle tree root hash of the transaction"
            )
        )
        =<< findMerkleRootTokenUtxoByRootHash sidechainParams rootHash

    { inUtxo:
        { nodeRef
        , oNode
        , datNode
        , tnNode
        }
    , nodes: DistributedSet.Ib { unIb: nodeA /\ nodeB }
    } ← case dsUtxo of
      Nothing →
        liftedM
          ( show
              (InternalError (NotFoundUtxo "Couldn't find distributed set nodes"))
          ) $ DistributedSet.slowFindDsOutput ds cborMteHashedTn
      Just dsTxInput → DistributedSet.findDsOutput ds cborMteHashedTn dsTxInput

    { confRef, confO } ← DistributedSet.findDsConfOutput ds

    insertValidator ← DistributedSet.insertValidator ds
    let insertValidatorHash = Scripts.validatorHash insertValidator

    { dsKeyPolicy, dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy ds

    recipientPkh ←
      liftContractM
        ( show
            ( InternalError
                (InvalidData "Couldn't convert recipient to public key hash")
            )
        )
        $ PaymentPubKeyHash
        <$> toPubKeyHash recipient

    let recipientSt = toStakePubKeyHash recipient

    let
      node = DistributedSet.mkNode (getTokenName tnNode) datNode
      value = Value.singleton fuelMintingCurrencySymbol fuelTokenName amount
      redeemer = wrap (toData (FUELMintingRedeemer merkleTreeEntry merkleProof))

    (scriptRefTxInput /\ scriptRefTxOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigInt.fromInt 1, scriptId: FUELMintingPolicy }
        )

    (merkleRootVersioningInput /\ merkleRootVersioningOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigInt.fromInt 1, scriptId: MerkleRootTokenPolicy }
        )

    (dsKeyVersioningInput /\ dsKeyVersioningOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigInt.fromInt 1, scriptId: DsKeyPolicy }
        )

    let
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
            <> Constraints.mustMintCurrencyUsingScriptRef
              (Scripts.mintingPolicyHash dsKeyPolicy)
              nTn
              (BigInt.fromInt 1)
              ( RefInput $ mkTxUnspentOut dsKeyVersioningInput
                  dsKeyVersioningOutput
              )

    mustAddDSNodeA ← mkNodeConstraints nodeA
    mustAddDSNodeB ← mkNodeConstraints nodeB

    pure
      { lookups:
          Lookups.mintingPolicy dsKeyPolicy
            <> Lookups.validator insertValidator
            <> Lookups.unspentOutputs (Map.singleton mptUtxo mptTxOut)
            <> Lookups.unspentOutputs (Map.singleton confRef confO)
            <> Lookups.unspentOutputs (Map.singleton nodeRef oNode)
            <> Lookups.unspentOutputs
              (Map.singleton merkleRootVersioningInput merkleRootVersioningOutput)
            <> Lookups.unspentOutputs
              (Map.singleton dsKeyVersioningInput dsKeyVersioningOutput)
      , constraints:
          -- Minting the FUEL tokens
          Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
            (Scripts.mintingPolicyHash fuelMintingPolicy)
            redeemer
            fuelTokenName
            amount
            (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
            <> mustPayToPubKeyAddress' recipientPkh recipientSt value
            <> Constraints.mustBeSignedBy ownPkh

            -- Referencing Merkle root
            <> Constraints.mustReferenceOutput mptUtxo

            -- Updating the distributed set
            <> Constraints.mustReferenceOutput confRef
            <> Constraints.mustSpendScriptOutput nodeRef unitRedeemer
            <> mustAddDSNodeA
            <> mustAddDSNodeB

            -- Referencing versioning utxos
            <> Constraints.mustReferenceOutput merkleRootVersioningInput
            <> Constraints.mustReferenceOutput dsKeyVersioningInput
      }

-- | `findMerkleRootTokenUtxoByRootHash` attempts to find a UTxO with MerkleRootToken
-- | as given by the `RootHash`
-- TODO: refactor to utility module
findMerkleRootTokenUtxoByRootHash ∷
  SidechainParams →
  RootHash →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMerkleRootTokenUtxoByRootHash sidechainParams rootHash = do
  merkleRootTokenName ←
    liftContractM
      ( show
          ( InternalError
              ( InvalidData
                  "Invalid Merkle root TokenName for MerkleRootTokenMintingPolicy"
              )
          )
      )
      $ Value.mkTokenName
      $ unRootHash rootHash
  findMerkleRootTokenUtxo merkleRootTokenName sidechainParams

-- | Derive the stake key hash from a public key address
toStakePubKeyHash ∷ Address → Maybe StakePubKeyHash
toStakePubKeyHash addr =
  case toStakingCredential addr of
    Just (StakingHash (PubKeyCredential pkh)) → Just (StakePubKeyHash pkh)
    _ → Nothing

-- | Pay values to a public key address (with optional staking key)
mustPayToPubKeyAddress' ∷
  PaymentPubKeyHash → Maybe StakePubKeyHash → Value → TxConstraints Void Void
mustPayToPubKeyAddress' pkh = case _ of
  Just skh → Constraints.mustPayToPubKeyAddress pkh skh
  Nothing → Constraints.mustPayToPubKey pkh
