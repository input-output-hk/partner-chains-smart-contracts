module FUELMintingPolicy
  ( runFuelMP
  , FUELMint(..)
  , FuelParams(..)
  , passiveBridgeMintParams
  , fuelMintingPolicy
  , getFuelMintingPolicy
  , MerkleTreeEntry(MerkleTreeEntry)
  , CombinedMerkleProof(CombinedMerkleProof)
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromAscii
  , hexToByteArrayUnsafe
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints (TxConstraint(..), singleton)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import MerkleTree (MerkleProof(..))
import RawScripts (rawFUELMintingPolicy)
import Serialization.Hash (ed25519KeyHashToBytes)
import SidechainParams (SidechainParams)
import Types.Scripts (plutusV2Script)
import Utils.Logging as Logging

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

-- | `CombinedMerkleProof` contains both the `MerkleTreeEntry` and its
-- | corresponding `MerkleProof`. See #249 for details.
newtype CombinedMerkleProof = CombinedMerkleProof
  { transaction ∷ MerkleTreeEntry
  , merkleProof ∷ MerkleProof
  }

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
-- TODO: the "creation of `FUELMint` via the given sidechain params" needs more
-- work i.e. we should actually fill up the currency symbols with their proper
-- currency symbols instead of `dummyCS`.
-- For now, we copy what the offchain code is doing.
getFuelMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
getFuelMintingPolicy sidechainParams = do
  fuelMintingPolicy $
    FUELMint
      { sidechainParams
      , mptRootTokenCurrencySymbol: dummyCS
      , dsKeyCurrencySymbol: dummyCS
      }

data FuelParams
  = Mint
      { amount ∷ BigInt
      , recipient ∷ PaymentPubKeyHash
      , merkleProof ∷ MerkleProof
      , sidechainParams ∷ SidechainParams
      , index ∷ BigInt
      , previousMerkleRoot ∷ Maybe ByteArray
      , entryHash ∷ ByteArray
      }
  | Burn { amount ∷ BigInt, recipient ∷ ByteArray }

runFuelMP ∷ SidechainParams → FuelParams → Contract () TransactionHash
runFuelMP sp fp = do
  let
    msg = Logging.mkReport { mod: "FUELMintingPolicy", fun: "runFuelMP" }

    inputTxIn = case fp of
      Mint _ → (unwrap sp).genesisMint
      Burn _ → Nothing

    fm =
      FUELMint
        { sidechainParams: sp
        , mptRootTokenCurrencySymbol: dummyCS
        , dsKeyCurrencySymbol: dummyCS
        }

  ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
  fuelMP ← fuelMintingPolicy fm

  inputUtxo ← inputTxIn # traverse \txIn → do
    txOut ← liftedM (msg "Cannot find genesis mint UTxO") $ getUtxo txIn
    pure $ Map.singleton txIn $ TransactionOutputWithRefScript
      { output: txOut, scriptRef: Nothing }

  cs ← liftContractM (msg "Cannot get currency symbol") $
    Value.scriptCurrencySymbol fuelMP
  logInfo' $ msg ("fuelMP currency symbol: " <> show cs)
  tn ← liftContractM (msg "Cannot get token name")
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")
  let
    mkValue i = Value.singleton cs tn i

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = case fp of
      Burn { recipient, amount } →
        let
          value = mkValue (-amount)
          redeemer = wrap (toData (MainToSide recipient))
        in
          Constraints.mustMintValueWithRedeemer redeemer value
      Mint { index, recipient, amount, previousMerkleRoot, merkleProof } →
        let
          value = mkValue amount
          redeemer = wrap (toData (SideToMain merkleTreeEntry merkleProof))
          merkleTreeEntry = MerkleTreeEntry
            { recipient: unwrap $ ed25519KeyHashToBytes $ unwrap $ unwrap recipient
            , previousMerkleRoot
            , amount
            , index
            }
          -- silence missing stake key warning
          mustPayToPubKey p =
            singleton <<< MustPayToPubKeyAddress p Nothing Nothing Nothing
        in
          Constraints.mustMintValueWithRedeemer redeemer value
            <> mustPayToPubKey recipient value
            <> Constraints.mustBeSignedBy ownPkh
            <> maybe mempty Constraints.mustSpendPubKeyOutput inputTxIn

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy fuelMP
      <> maybe mempty Lookups.unspentOutputs inputUtxo

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceAndSignTxE ubTx)
  txId ← submit bsTx
  logInfo' $ msg ("Submitted Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "Tx submitted successfully!"

  pure txId

-- | Empty currency symbol to be used with Passive Bridge transactions,
-- | where these tokens are not used
-- NOTE: Just adaSymbol == mkCurrencySymbol (hexToByteArrayUnsafe "")
dummyCS ∷ CurrencySymbol
dummyCS = Value.adaSymbol

-- | Mocking unused data for Passive Bridge minting, where we use genesis minting
passiveBridgeMintParams ∷
  SidechainParams →
  { amount ∷ BigInt, recipient ∷ PaymentPubKeyHash } →
  FuelParams
passiveBridgeMintParams sidechainParams { amount, recipient } =
  Mint
    { merkleProof: MerkleProof []
    , index: BigInt.fromInt 0
    , previousMerkleRoot: Nothing
    , entryHash: hexToByteArrayUnsafe ""
    , sidechainParams
    , recipient
    , amount
    }
