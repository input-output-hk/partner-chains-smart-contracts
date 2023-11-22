module TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(..)
  , getCandidatePermissionMintingPolicy
  , candidatePermissionMintingPolicy
  , CandidatePermissionMintParams(..)
  , CandidatePermissionTokenInfo
  , CandidatePermissionTokenMintInfo
  , candidatePermissionTokenLookupsAndConstraints
  , runCandidatePermissionToken
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData
  ( class FromData
  , class ToData
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Utxos as Utxos
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.Map as Map
import TrustlessSidechain.RawScripts (rawCandidatePermissionMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )
import TrustlessSidechain.Utils.Error
  ( InternalError(NotFoundUtxo, InvalidScript)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

--------------------------------
-- Working with the onchain code
--------------------------------
-- | `CandidatePermissionMint` is the parameter for
-- | `candidatePermissionMintingPolicy`
newtype CandidatePermissionMint = CandidatePermissionMint
  { sidechainParams ∷ SidechainParams
  , candidatePermissionTokenUtxo ∷ TransactionInput
  }

derive instance Generic CandidatePermissionMint _

derive instance Newtype CandidatePermissionMint _

derive newtype instance Eq CandidatePermissionMint

instance Show CandidatePermissionMint where
  show = genericShow

instance ToData CandidatePermissionMint where
  toData
    (CandidatePermissionMint { sidechainParams, candidatePermissionTokenUtxo }) =
    productToData2 sidechainParams candidatePermissionTokenUtxo

instance FromData CandidatePermissionMint where
  fromData = productFromData2
    ( \sidechainParams candidatePermissionTokenUtxo →
        CandidatePermissionMint
          { sidechainParams
          , candidatePermissionTokenUtxo
          }
    )

-- | `getCandidatePermissionMintingPolicy` grabs both the minting policy /
-- | currency symbol for the candidate permission minting policy.
getCandidatePermissionMintingPolicy ∷
  CandidatePermissionMint →
  Contract
    { candidatePermissionPolicy ∷ MintingPolicy
    , candidatePermissionCurrencySymbol ∷ CurrencySymbol
    }
getCandidatePermissionMintingPolicy cpm = do
  candidatePermissionPolicy ← candidatePermissionMintingPolicy cpm
  candidatePermissionCurrencySymbol ← Monad.liftContractM
    (show (InternalError (InvalidScript "CandidatePermissionToken")))
    (Value.scriptCurrencySymbol candidatePermissionPolicy)
  pure
    { candidatePermissionPolicy
    , candidatePermissionCurrencySymbol
    }

-- | `candidatePermissionMintingPolicy` gets the minting policy for the
-- | candidate permission minting policy
candidatePermissionMintingPolicy ∷
  CandidatePermissionMint → Contract MintingPolicy
candidatePermissionMintingPolicy cpm = do
  mkMintingPolicyWithParams rawCandidatePermissionMintingPolicy [ toData cpm ]

--------------------------------
-- Endpoint code
--------------------------------

-- | `CandidatePermissionMintParams` is the endpoint parameters for the
-- | candidate permission token.
newtype CandidatePermissionMintParams = CandidatePermissionMintParams
  { candidateMintPermissionMint ∷ CandidatePermissionMint
  , candidatePermissionTokenName ∷ TokenName
  , amount ∷ BigInt
  }

-- | `CandidatePermissionTokenInfo` wraps up some of the required information for
-- | referring to a candidate permission token. This isn't used onchain, but used
-- | offchain for wrapping up this data consistently
type CandidatePermissionTokenInfo =
  { candidatePermissionTokenUtxo ∷ TransactionInput
  , candidatePermissionTokenName ∷ TokenName
  }

-- | `CandidatePermissionTokenMintInfo` wraps up some of the required information for
-- | minting a candidate permission token. This isn't used onchain, but used
-- | offchain for wrapping up this data consistently
type CandidatePermissionTokenMintInfo =
  { amount ∷ BigInt
  , permissionToken ∷ CandidatePermissionTokenInfo
  }

-- | `candidatePermissionTokenLookupsAndConstraints` creates the required
-- | lookups and constraints to build the transaction to mint the tokens.
candidatePermissionTokenLookupsAndConstraints ∷
  CandidatePermissionMintParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
candidatePermissionTokenLookupsAndConstraints
  ( CandidatePermissionMintParams
      { candidateMintPermissionMint, candidatePermissionTokenName, amount }
  ) = do
  { candidatePermissionPolicy
  , candidatePermissionCurrencySymbol
  } ← getCandidatePermissionMintingPolicy
    candidateMintPermissionMint

  let txIn = (unwrap candidateMintPermissionMint).candidatePermissionTokenUtxo
  txOut ← Monad.liftedM (show (InternalError (NotFoundUtxo "Genesis UTxO"))) $
    Utxos.getUtxo
      txIn

  let
    value = Value.singleton
      candidatePermissionCurrencySymbol
      candidatePermissionTokenName
      amount

    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.mintingPolicy candidatePermissionPolicy
        <>
          Lookups.unspentOutputs
            ( Map.singleton txIn
                ( TransactionOutputWithRefScript
                    { output: txOut, scriptRef: Nothing }
                )
            )

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer
        unitRedeemer
        value
        <> TxConstraints.mustSpendPubKeyOutput txIn
  pure { lookups, constraints }

-- | `runCandidatePermissionToken` is the endpoint for minting candidate
-- | permission tokens.
runCandidatePermissionToken ∷
  CandidatePermissionMintParams →
  Contract
    { transactionId ∷ TransactionHash
    , candidatePermissionCurrencySymbol ∷ CurrencySymbol
    }
runCandidatePermissionToken
  cpmp@
    ( CandidatePermissionMintParams
        { candidateMintPermissionMint }
    ) = do
  { candidatePermissionCurrencySymbol } ← getCandidatePermissionMintingPolicy
    candidateMintPermissionMint

  txId ← candidatePermissionTokenLookupsAndConstraints cpmp >>=
    balanceSignAndSubmit "Mint CandidatePermissionToken"

  pure { transactionId: txId, candidatePermissionCurrencySymbol }
