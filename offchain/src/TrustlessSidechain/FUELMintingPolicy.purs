module TrustlessSidechain.FUELMintingPolicy
  ( CombinedMerkleProof(..)
  , FUELMint(..)
  , FuelParams(..)
  , FuelMintOrFuelBurnParams(..)
  , MerkleTreeEntry(..)
  , fuelMintingPolicy
  , getFuelMintingPolicy
  , runFuelMP
  , combinedMerkleProofToFuelParams
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash(..)
  , StakePubKeyHash(..)
  , ownPaymentPubKeyHash
  , toPubKeyHash
  , toStakingCredential
  )
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Hashing (blake2b256Hash)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(..)
  , PlutusData(Constr)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
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
import Record as Record
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeOraclePolicy (getCommitteeOraclePolicy)
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.MerkleRoot
  ( SignedMerkleRootMint(..)
  , findMerkleRootTokenUtxo
  )
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleTree (MerkleProof, RootHash, rootMp, unRootHash)
import TrustlessSidechain.RawScripts (rawFUELMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( Bech32Bytes
  , addressFromBech32Bytes
  , bech32BytesFromAddress
  )
import TrustlessSidechain.Utils.Logging
  ( InternalError
      ( NotFoundOwnPubKeyHash
      , InvalidScript
      , InvalidData
      , NotFoundUtxo
      )
  , OffchainError(InternalError, InvalidInputError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `FUELMint` is the data type to parameterize the minting policy.
-- | Note: this matches the haskell onchain data type.
newtype FUELMint = FUELMint
  { merkleRootTokenCurrencySymbol ∷ CurrencySymbol
  , sidechainParams ∷ SidechainParams
  , dsKeyCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic FUELMint _
derive instance Newtype FUELMint _
instance ToData FUELMint where
  toData
    ( FUELMint
        { merkleRootTokenCurrencySymbol, sidechainParams, dsKeyCurrencySymbol }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData merkleRootTokenCurrencySymbol
      , toData sidechainParams
      , toData dsKeyCurrencySymbol
      ]

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

-- | `combinedMerkleProofToFuelParams` converts `SidechainParams` and
-- | `CombinedMerkleProof` to a `Mint` of `FuelParams`.
-- | This is a modestly convenient wrapper to help call the `runFuelMP `
-- | endpoint for internal tests.
combinedMerkleProofToFuelParams ∷
  { sidechainParams ∷ SidechainParams
  , combinedMerkleProof ∷ CombinedMerkleProof
  , atmsKind ∷ ATMSKinds
  } →
  Maybe FuelParams
combinedMerkleProofToFuelParams
  { sidechainParams
  , combinedMerkleProof: CombinedMerkleProof { transaction, merkleProof }
  , atmsKind
  } = do
  let transaction' = unwrap transaction

  recipient ← addressFromBech32Bytes transaction'.recipient
  pure $
    FuelParams
      { sidechainParams
      , atmsKind
      , fuelMintOrFuelBurnParams:
          Mint
            { amount: transaction'.amount
            , recipient
            , merkleProof
            , index: transaction'.index
            , previousMerkleRoot: transaction'.previousMerkleRoot
            , dsUtxo: Nothing
            }
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

data FUELRedeemer
  = MainToSide ByteArray -- recipient sidechain (addr , signature)
  | SideToMain MerkleTreeEntry MerkleProof

derive instance Generic FUELRedeemer _
instance ToData FUELRedeemer where
  toData (MainToSide s1) = Constr (BigNum.fromInt 0) [ toData s1 ]
  toData (SideToMain s1 s2) = Constr (BigNum.fromInt 1)
    [ toData s1
    , toData s2
    ]

-- | Gets the FUELMintingPolicy by applying `FUELMint` to the FUEL minting
-- | policy
fuelMintingPolicy ∷ FUELMint → Contract MintingPolicy
fuelMintingPolicy fm = do
  let
    script = decodeTextEnvelope rawFUELMintingPolicy >>=
      plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied [ toData fm ]
  pure $ PlutusMintingPolicy applied

-- | `getFuelMintingPolicy` creates the parameter `FUELMint`
-- | (as required by the onchain mintng policy) via the given
-- | `SidechainParams`, and calls `fuelMintingPolicy` to give us the minting
-- | policy
getFuelMintingPolicy ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Contract
    { fuelMintingPolicy ∷ MintingPolicy
    , fuelMintingPolicyCurrencySymbol ∷ CurrencySymbol
    }
getFuelMintingPolicy { sidechainParams, atmsKind } = do
  { committeeOracleCurrencySymbol } ← getCommitteeOraclePolicy sidechainParams

  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      ( CommitteeCertificateMint
          { committeeOraclePolicy: committeeOracleCurrencySymbol
          , thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
          , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
          }
      )
      atmsKind

  { merkleRootTokenCurrencySymbol } ← MerkleRoot.getMerkleRootTokenMintingPolicy
    { sidechainParams, committeeCertificateVerificationCurrencySymbol }

  ds ← DistributedSet.getDs (unwrap sidechainParams).genesisUtxo

  { dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy ds

  policy ← fuelMintingPolicy $
    FUELMint
      { sidechainParams
      , merkleRootTokenCurrencySymbol
      , dsKeyCurrencySymbol: dsKeyPolicyCurrencySymbol
      }
  fuelMintingPolicyCurrencySymbol ←
    liftContractM
      (show (InternalError (InvalidScript "fuel minting policy is invalid"))) $
      Value.scriptCurrencySymbol policy
  pure
    { fuelMintingPolicy: policy
    , fuelMintingPolicyCurrencySymbol
    }

-- | `FuelParams` is the data for the FUEL mint / burn endpoint.
newtype FuelParams = FuelParams
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  , fuelMintOrFuelBurnParams ∷ FuelMintOrFuelBurnParams
  }

data FuelMintOrFuelBurnParams
  = Mint
      { amount ∷ BigInt
      , recipient ∷ Address
      , merkleProof ∷ MerkleProof
      , index ∷ BigInt
      , previousMerkleRoot ∷ Maybe RootHash
      , dsUtxo ∷ Maybe TransactionInput
      }
  | Burn { amount ∷ BigInt, recipient ∷ ByteArray }

-- | `runFuelMP` executes the FUEL mint / burn endpoint.
runFuelMP ∷ FuelParams → Contract TransactionHash
runFuelMP
  (FuelParams { sidechainParams: sp, atmsKind, fuelMintOrFuelBurnParams: fp }) =
  do
    { fuelMintingPolicy: fuelMP } ← getFuelMintingPolicy
      { sidechainParams: sp
      , atmsKind
      }

    { lookups, constraints } ← case fp of
      Burn params →
        burnFUEL fuelMP params
      Mint params → claimFUEL fuelMP
        $ Record.disjointUnion
            params
            { sidechainParams: sp
            , atmsKind
            }

    let
      txName = case fp of
        Burn _ → "FUEL token burn"
        Mint _ → "FUEL token mint"

    balanceSignAndSubmit txName lookups constraints

-- | Mint FUEL tokens using the Active Bridge configuration, verifying the
-- | Merkle proof
claimFUEL ∷
  MintingPolicy →
  { amount ∷ BigInt
  , recipient ∷ Address
  , merkleProof ∷ MerkleProof
  , sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  , index ∷ BigInt
  , previousMerkleRoot ∷ Maybe RootHash
  , dsUtxo ∷ Maybe TransactionInput
  } →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
claimFUEL
  fuelMP
  { amount
  , recipient
  , merkleProof
  , sidechainParams
  , atmsKind
  , index
  , previousMerkleRoot
  , dsUtxo
  } =
  do
    ownPkh ← liftedM (show (InternalError NotFoundOwnPubKeyHash))
      ownPaymentPubKeyHash

    cs /\ tn ← getFuelAssetClass fuelMP

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
        =<< findMerkleRootTokenUtxoByRootHash
          { sidechainParams
          , rootHash
          , atmsKind
          }

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
          ) $
          DistributedSet.slowFindDsOutput ds cborMteHashedTn
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
            <> Lookups.mintingPolicy dsKeyPolicy
            <> Lookups.validator insertValidator
            <> Lookups.unspentOutputs (Map.singleton mptUtxo mptTxOut)
            <> Lookups.unspentOutputs (Map.singleton confRef confO)
            <> Lookups.unspentOutputs (Map.singleton nodeRef oNode)

      , constraints:
          -- Minting the FUEL tokens
          Constraints.mustMintValueWithRedeemer redeemer value
            <> mustPayToPubKeyAddress' recipientPkh recipientSt value
            <> Constraints.mustBeSignedBy ownPkh

            -- Referencing Merkle root
            <> Constraints.mustReferenceOutput mptUtxo

            -- Updating the distributed set
            <> Constraints.mustReferenceOutput confRef
            <> Constraints.mustSpendScriptOutput nodeRef unitRedeemer
            <> mustAddDSNodeA
            <> mustAddDSNodeB
      }

-- | `burnFUEL` burns the given FUEL amount.
burnFUEL ∷
  MintingPolicy →
  { amount ∷ BigInt, recipient ∷ ByteArray } →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
burnFUEL fuelMP { amount, recipient } = do
  cs /\ tn ← getFuelAssetClass fuelMP

  let
    value = Value.singleton cs tn (-amount)
    redeemer = wrap (toData (MainToSide recipient))
  pure
    { lookups: Lookups.mintingPolicy fuelMP
    , constraints: Constraints.mustMintValueWithRedeemer redeemer value
    }

-- | `findMerkleRootTokenUtxoByRootHash` attempts to find a UTxO with MerkleRootToken
-- | as given by the `RootHash`
-- TODO: refactor to utility module
findMerkleRootTokenUtxoByRootHash ∷
  { sidechainParams ∷ SidechainParams
  , rootHash ∷ RootHash
  , atmsKind ∷ ATMSKinds
  } →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMerkleRootTokenUtxoByRootHash { sidechainParams, rootHash, atmsKind } = do
  { committeeOracleCurrencySymbol } ← getCommitteeOraclePolicy sidechainParams

  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      ( CommitteeCertificateMint
          { committeeOraclePolicy: committeeOracleCurrencySymbol
          , thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
          , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
          }
      )
      atmsKind

  -- Then, we get the merkle root token validator hash / minting policy..
  merkleRootValidatorHash ← map Scripts.validatorHash $
    MerkleRoot.merkleRootTokenValidator sidechainParams
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , committeeCertificateVerificationCurrencySymbol
      , merkleRootValidatorHash
      }
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
  findMerkleRootTokenUtxo merkleRootTokenName smrm

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

-- | Return the currency symbol and token name of the FUEL token
getFuelAssetClass ∷ MintingPolicy → Contract (CurrencySymbol /\ TokenName)
getFuelAssetClass fuelMP = do
  cs ← liftContractM "Cannot get FUEL currency symbol" $
    Value.scriptCurrencySymbol fuelMP
  tn ← liftContractM "Cannot get FUEL token name"
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")

  pure (cs /\ tn)
