module FUELMintingPolicy
  ( runFuelMP
  , FUELMint(..)
  , FuelParams(..)
  , passiveBridgeMintParams
  , fuelMintingPolicy
  , MerkleTreeEntry(MerkleTreeEntry)
  , getCurrencySymbolHex
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromAscii
  , byteArrayToHex
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
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, getCurrencySymbol, mkCurrencySymbol)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import MerkleTree (MerkleProof(..))
import Partial.Unsafe (unsafePartial)
import RawScripts (rawFUELMintingPolicy)
import Serialization.Hash (ed25519KeyHashToBytes)
import SidechainParams (SidechainParams)
import Types.Scripts (plutusV2Script)

{- | 'FUELMint' is the data type to parameterize the minting policy. See
 'mkMintingPolicy' for details of why we need the datum in 'FUELMint'
-}
newtype FUELMint = FUELMint
  { -- 'fmMptRootTokenValidator' is the hash of the validator script
    -- which /should/ have a token which has the merkle root in the token
    -- name. See 'TrustlessSidechain.OnChain.MPTRootTokenValidator' for
    -- details.
    -- > fmMptRootTokenValidator :: ValidatorHash
    -- N.B. We don't need this! We're really only interested in the token,
    -- and indeed; anyone can pay a token to this script so there really
    -- isn't a reason to use this validator script as the "identifier" for
    -- MPTRootTokens.

    -- | 'fmMptRootTokenCurrencySymbol' is the 'CurrencySymbol' of a token
    -- which contains a merkle root in the 'TokenName'. See
    -- 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy' for details.
    mptRootTokenCurrencySymbol ∷ CurrencySymbol
  , -- | 'fmSidechainParams' is the sidechain parameters
    sidechainParams ∷ SidechainParams
  , -- | 'fmDsKeyCurrencySymbol' is th currency symbol for the tokens which
    -- hold the key for the distributed set. In particular, this allows the
    -- FUEL minting policy to verify if a string has /just been inserted/ into
    -- the distributed set.
    dsKeyCurrencySymbol ∷ CurrencySymbol
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

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MPTRootToken.
-}
newtype MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    index ∷ BigInt
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    amount ∷ BigInt
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    recipient ∷ ByteArray
  , -- | the previous merkle root (if it exists). (used to ensure uniquness of entries)
    previousMerkleRoot ∷ Maybe ByteArray
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

-- | 'getCurrencySymbolHex' returns the hex encoded string of the currency
-- | symbol of the FUEL minting policy.
getCurrencySymbolHex ∷ SidechainParams → Contract () String
getCurrencySymbolHex sp = do
  let
    fm =
      FUELMint
        { sidechainParams: sp
        , mptRootTokenCurrencySymbol: dummyCS
        , dsKeyCurrencySymbol: dummyCS
        }

  fuelMP ← fuelMintingPolicy fm
  cs ← liftContractM "Cannot get currency symbol" $ Value.scriptCurrencySymbol
    fuelMP
  pure $ byteArrayToHex $ getCurrencySymbol cs

runFuelMP ∷ SidechainParams → FuelParams → Contract () TransactionHash
runFuelMP sp fp = do
  ownPkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash

  let
    inputTxIn = case fp of
      Mint _ → (unwrap (unwrap fm).sidechainParams).genesisMint
      Burn _ → Nothing

    fm =
      FUELMint
        { sidechainParams: sp
        , mptRootTokenCurrencySymbol: dummyCS
        , dsKeyCurrencySymbol: dummyCS
        }

  fuelMP ← fuelMintingPolicy fm

  inputUtxo ← traverse
    ( \txIn → do
        txOut ← liftedM "Cannot find genesis mint UTxO" $ getUtxo txIn
        pure $ Map.singleton txIn $ TransactionOutputWithRefScript
          { output: txOut, scriptRef: Nothing }
    )
    inputTxIn

  cs ← liftContractM "Cannot get currency symbol" $
    Value.scriptCurrencySymbol fuelMP
  logInfo' ("fuelMP currency symbol: " <> show cs)
  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")
  let
    mkValue i = Value.singleton cs tn i

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = case fp of
      Burn bp →
        let
          redeemer = wrap (toData (MainToSide bp.recipient))
        in
          Constraints.mustMintValueWithRedeemer redeemer (mkValue (-bp.amount))
      Mint mp →
        let
          value = mkValue mp.amount
          merkleTreeEntry =
            MerkleTreeEntry
              { index: mp.index
              , amount: mp.amount
              , recipient: unwrap $ ed25519KeyHashToBytes $ unwrap $ unwrap
                  mp.recipient
              , previousMerkleRoot: mp.previousMerkleRoot
              }
        in
          Constraints.mustMintValueWithRedeemer
            (wrap (toData (SideToMain merkleTreeEntry mp.merkleProof)))
            value
            <> Constraints.mustPayToPubKey mp.recipient value
            <> Constraints.mustBeSignedBy ownPkh
            <> (maybe mempty Constraints.mustSpendPubKeyOutput inputTxIn)

    lookups ∷ Lookups.ScriptLookups Void
    lookups = (maybe mempty Lookups.unspentOutputs inputUtxo) <>
      Lookups.mintingPolicy fuelMP

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted fuelMP Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "fuelMP Tx submitted successfully!"

  pure txId

{- | Empty currency symbol to be used with Passive Bridge transactions,
  where these tokens are not used
-}
dummyCS ∷ CurrencySymbol
dummyCS = unsafePartial $ fromJust $ mkCurrencySymbol $
  hexToByteArrayUnsafe ""

{- | Mocking unused data for Passive Bridge minting, where we use genesis minting -}
passiveBridgeMintParams ∷
  SidechainParams →
  { amount ∷ BigInt, recipient ∷ PaymentPubKeyHash } →
  FuelParams
passiveBridgeMintParams sidechainParams { amount, recipient } =
  Mint
    { amount
    , recipient
    , merkleProof: MerkleProof []
    , sidechainParams
    , index: BigInt.fromInt 0
    , previousMerkleRoot: Nothing
    , entryHash: hexToByteArrayUnsafe ""
    }
