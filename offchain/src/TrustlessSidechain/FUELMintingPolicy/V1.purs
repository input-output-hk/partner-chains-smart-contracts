module TrustlessSidechain.FUELMintingPolicy.V1
  ( MerkleTreeEntry(..)
  , CombinedMerkleProof(..)
  , FuelMintParams(..)
  , FUELMintingRedeemer(..)
  , mkMintFuelLookupAndConstraints
  , getFuelMintingPolicy
  , fuelAssetName
  , combinedMerkleProofToFuelParams
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Address (getPaymentCredential, getStakeCredential)
import Cardano.Types.AssetName (AssetName, mkAssetName, unAssetName)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.Value (Value)
import Cardano.Types.Value as Value
import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Data.ByteArray (ByteArray)
import Data.Map as Map
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logWarn') as Effect
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
      ( InvalidData
      , NotFoundUtxo
      , InvalidAddress
      , GenericInternalError
      )
  )
import TrustlessSidechain.MerkleRoot
  ( findMerkleRootTokenUtxo
  )
import TrustlessSidechain.MerkleTree
  ( MerkleProof
  , RootHash
  , rootMp
  , unRootHash
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( addressFromBech32Bytes
  , getOwnPaymentPubKeyHash
  )
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName)
import TrustlessSidechain.Utils.Crypto (blake2b256Hash)
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
import Type.Row (type (+))

fuelAssetName ∷ AssetName
fuelAssetName = unsafeMkAssetName "FUEL"

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
  , recipient ∷ ByteArray
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

derive instance Eq FUELMintingRedeemer

derive instance Generic FUELMintingRedeemer _

instance Show FUELMintingRedeemer where
  show = genericShow

instance ToData FUELMintingRedeemer where
  toData (FUELMintingRedeemer s1 s2) = Constr (BigNum.fromInt 0)
    [ toData s1
    , toData s2
    ]
  toData FUELBurningRedeemer = Constr (BigNum.fromInt 1) []

instance FromData FUELMintingRedeemer where
  fromData = case _ of
    Constr tag [ t1, t2 ] | tag == BigNum.fromInt 0 → do
      s1 ← fromData t1
      s2 ← fromData t2
      pure $ FUELMintingRedeemer s1 s2
    Constr tag [] | tag == BigNum.fromInt 1 → pure FUELBurningRedeemer
    _ → Nothing

-- | Gets the FUELMintingPolicy by applying `FUELMint` to the FUEL minting
-- | policy
decodeFuelMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeFuelMintingPolicy sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkMintingPolicyWithParams FUELMintingPolicy
    [ toData sidechainParams, toData versionOracleConfig ]

-- | `getFuelMintingPolicy` creates the parameter `FUELMint`
-- | (as required by the onchain mintng policy) via the given
-- | `SidechainParams`, and calls `fuelMintingPolicy` to give us the minting
-- | policy
getFuelMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { fuelMintingPolicy ∷ PlutusScript
    , fuelMintingCurrencySymbol ∷ ScriptHash
    }
getFuelMintingPolicy sidechainParams = do
  fuelMintingPolicy ← decodeFuelMintingPolicy sidechainParams
  let fuelMintingCurrencySymbol = PlutusScript.hash fuelMintingPolicy
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
  ∀ r.
  SidechainParams →
  FuelMintParams →
  Run (APP + r)
    { lookups ∷ ScriptLookups, constraints ∷ TxConstraints }
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

    ds ← DistributedSet.getDs sidechainParams

    let bech32BytesRecipient = unwrap $ encodeCbor recipient
    let
      merkleTreeEntry =
        MerkleTreeEntry
          { index
          , amount
          , previousMerkleRoot
          , recipient: bech32BytesRecipient
          }

    let
      entryBytes = unwrap $ encodeCbor $ toData merkleTreeEntry
      cborMteHashed = blake2b256Hash entryBytes
      rootHash = rootMp entryBytes merkleProof

    cborMteHashedTn ←
      Run.note
        (InvalidData "Token name exceeds size limit")
        $ mkAssetName
        $ cborMteHashed

    { index: mptUtxo, value: mptTxOut } ←
      Effect.fromMaybeThrow
        ( NotFoundUtxo
            "Couldn't find the parent Merkle tree root hash of the transaction"

        )
        $ findMerkleRootTokenUtxoByRootHash sidechainParams rootHash

    { inUtxo:
        { nodeRef
        , oNode
        , datNode
        , tnNode
        }
    , nodes: DistributedSet.Ib { unIb: nodeA /\ nodeB }
    } ← case dsUtxo of
      Nothing →
        Effect.fromMaybeThrow
          (NotFoundUtxo "Couldn't find distributed set nodes")
          $ DistributedSet.slowFindDsOutput ds cborMteHashedTn
      Just dsTxInput → DistributedSet.findDsOutput ds cborMteHashedTn dsTxInput

    { confRef, confO } ← DistributedSet.findDsConfOutput ds

    insertValidator ← DistributedSet.insertValidator ds
    let insertValidatorHash = PlutusScript.hash insertValidator

    { mintingPolicy, currencySymbol } ← DistributedSet.getDsKeyPolicy ds

    recipientPkh ←
      Run.note
        ( InvalidAddress "Couldn't convert recipient to public key hash: "
            recipient
        )
        $ toPaymentPubKeyHash recipient

    let recipientSt = toStakePubKeyHash recipient

    when (isNothing recipientSt) $
      Effect.logWarn' "Recipient address does not contain staking key."

    let
      node = DistributedSet.mkNode (unAssetName tnNode) datNode
      amount' = BigNum.fromStringUnsafe $ BigInt.toString amount
      value = Value.singleton fuelMintingCurrencySymbol fuelAssetName amount'
      redeemer = wrap (toData (FUELMintingRedeemer merkleTreeEntry merkleProof))

    (scriptRefTxInput /\ scriptRefTxOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigNum.fromInt 1, scriptId: FUELMintingPolicy }
        )

    (merkleRootVersioningInput /\ merkleRootVersioningOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigNum.fromInt 1, scriptId: MerkleRootTokenPolicy }
        )

    (dsKeyVersioningInput /\ dsKeyVersioningOutput) ←
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { version: BigNum.fromInt 1, scriptId: DsKeyPolicy }
        )

    let
      mkNodeConstraints n = do
        nTn ←
          Run.note
            ( GenericInternalError $ "Couldn't convert node key to token "
                <> "name.  The key is "
                <> show (unwrap n).nKey
            )
            $ mkAssetName
            $ (unwrap n).nKey

        let val = Value.singleton currencySymbol nTn (BigNum.fromInt 1)
        if unAssetName nTn == (unwrap node).nKey then
          pure $ Constraints.mustPayToScript
            insertValidatorHash
            (toData (DistributedSet.nodeToDatum n))
            DatumInline
            val
        else
          pure
            $ Constraints.mustPayToScript
                insertValidatorHash
                (toData (DistributedSet.nodeToDatum n))
                DatumInline
                val
            <> Constraints.mustMintCurrencyUsingScriptRef
              (PlutusScript.hash mintingPolicy)
              nTn
              (Int.fromInt 1)
              ( RefInput $ TransactionUnspentOutput
                  { input: dsKeyVersioningInput
                  , output: dsKeyVersioningOutput
                  }
              )

    mustAddDSNodeA ← mkNodeConstraints nodeA
    mustAddDSNodeB ← mkNodeConstraints nodeB
    let
      mintAmount = unsafePartial $ fromJust $ Int.fromString $ BigInt.toString
        amount
    pure
      { lookups:
          Lookups.plutusMintingPolicy mintingPolicy
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
            (PlutusScript.hash fuelMintingPolicy)
            redeemer
            fuelAssetName
            mintAmount
            ( RefInput $ TransactionUnspentOutput
                { input: scriptRefTxInput
                , output: scriptRefTxOutput
                }
            )
            <> mustPayToPubKeyAddress' recipientPkh recipientSt value
            <> Constraints.mustBeSignedBy ownPkh

            -- Referencing Merkle root
            <> Constraints.mustReferenceOutput mptUtxo

            -- Updating the distributed set
            <> Constraints.mustReferenceOutput confRef
            <> Constraints.mustSpendScriptOutput nodeRef (wrap $ PlutusData.unit)
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
  ∀ r.
  SidechainParams →
  RootHash →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findMerkleRootTokenUtxoByRootHash sidechainParams rootHash = do
  merkleRootTokenName ←
    Run.note
      ( InvalidData
          "Invalid Merkle root TokenName for MerkleRootTokenMintingPolicy"
      )
      $ mkAssetName
      $ unRootHash rootHash
  findMerkleRootTokenUtxo merkleRootTokenName sidechainParams

-- | Derive the public key hash from a public key address
toPaymentPubKeyHash ∷ Address → Maybe PaymentPubKeyHash
toPaymentPubKeyHash addr = wrap <$>
  ((asPubKeyHash <<< unwrap) =<< getPaymentCredential addr)

-- | Derive the stake key hash from a public key address
toStakePubKeyHash ∷ Address → Maybe StakePubKeyHash
toStakePubKeyHash addr = wrap <$>
  ((asPubKeyHash <<< unwrap) =<< getStakeCredential addr)

-- | Pay values to a public key address (with optional staking key)
mustPayToPubKeyAddress' ∷
  PaymentPubKeyHash → Maybe StakePubKeyHash → Value → TxConstraints
mustPayToPubKeyAddress' pkh = case _ of
  Just skh → Constraints.mustPayToPubKeyAddress pkh skh
  Nothing → Constraints.mustPayToPubKey pkh
