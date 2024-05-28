-- | `TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
module TrustlessSidechain.GarbageCollector
  ( mkBurnNFTsLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  , toData
  , unitRedeemer
  )
import Cardano.Types.PlutusScript as PlutusScript
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts as Scripts
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Partial.Unsafe (unsafePartial)
import Cardano.Types.Int as Int
import Contract.TxConstraints as TxConstraints
import Cardano.Types.Value (getMultiAsset)
import Cardano.Types.MultiAsset (flatten)
import Contract.Numeric.BigNum as BigNum
import Data.Array (filter)
import JS.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as EcdsaATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as SchnorrATMSPolicy
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELBurningPolicy.V1 as BurningV1
import TrustlessSidechain.FUELMintingPolicy.V1 as MintingV1
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Utxos (getOwnUTxOsTotalValue)
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( FUELMintingPolicy
      , FUELBurningPolicy
      , CommitteeCertificateVerificationPolicy
      )
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | `burnNFTsLookupsAndConstraints` burns all waste NFTs found on users wallet address
mkBurnNFTsLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mkBurnNFTsLookupsAndConstraints sidechainParams = do
  ownValue ← getOwnUTxOsTotalValue
  let
    committeeCertificateMint =
      EcdsaATMSPolicy.committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams
        sidechainParams

  { mintingPolicy: committeePlainEcdsaSecp256k1ATMSCurrencyInfo
  , currencySymbol: committeePlainEcdsaSecp256k1ATMSCurrencySymbol
  } ← EcdsaATMSPolicy.committeePlainEcdsaSecp256k1ATMSCurrencyInfo
    { committeeCertificateMint, sidechainParams }

  { mintingPolicy: committeePlainSchnorrSecp256k1ATMSCurrencyInfo
  , currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol
  } ← SchnorrATMSPolicy.committeePlainSchnorrSecp256k1ATMSCurrencyInfo
    { committeeCertificateMint, sidechainParams }

  { fuelMintingPolicy
  , fuelMintingCurrencySymbol
  } ← MintingV1.getFuelMintingPolicy sidechainParams

  { fuelBurningPolicy
  , fuelBurningCurrencySymbol
  } ← BurningV1.getFuelBurningPolicy sidechainParams

  ( committeeCertificateVerificationVersioningInput /\
      committeeCertificateVerificationVersioningOutput
  ) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1
          , scriptId: CommitteeCertificateVerificationPolicy
          }
      )

  (fuelMintingPolicyRefTxInput /\ fuelMintingPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: FUELMintingPolicy }
      )

  (fuelBurningPolicyRefTxInput /\ fuelBurningPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: FUELBurningPolicy }
      )

  let
    currencySymbolsToBurn =
      [ { currencySymbol: committeePlainEcdsaSecp256k1ATMSCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                ( PlutusScript.hash
                    committeePlainEcdsaSecp256k1ATMSCurrencyInfo
                )
                (RedeemerDatum $ toData EcdsaATMSPolicy.ATMSBurn)
                tokenName
                (Int.negate amount)
                ( RefInput $ TransactionUnspentOutput
                    { input: committeeCertificateVerificationVersioningInput
                    , output: committeeCertificateVerificationVersioningOutput
                    }
                )
            )
        }
      , { currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                ( PlutusScript.hash
                    committeePlainSchnorrSecp256k1ATMSCurrencyInfo
                )
                (RedeemerDatum $ toData SchnorrATMSPolicy.ATMSBurn)
                tokenName
                (Int.negate amount)
                ( RefInput $ TransactionUnspentOutput
                    { input: committeeCertificateVerificationVersioningInput
                    , output: committeeCertificateVerificationVersioningOutput
                    }
                )
            )
        }
      , { currencySymbol: fuelMintingCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                (PlutusScript.hash fuelMintingPolicy)
                (RedeemerDatum $ toData MintingV1.FUELBurningRedeemer)
                tokenName
                (Int.negate amount)
                ( RefInput $ TransactionUnspentOutput
                    { input: fuelMintingPolicyRefTxInput
                    , output: fuelMintingPolicyRefTxOutput
                    }
                )
            )
        }
      , { currencySymbol: fuelBurningCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                (PlutusScript.hash fuelBurningPolicy)
                unitRedeemer
                tokenName
                (Int.negate amount)
                ( RefInput $ TransactionUnspentOutput
                    { input: fuelBurningPolicyRefTxInput
                    , output: fuelBurningPolicyRefTxOutput
                    }
                )
            )
        }
      ]

  let
    constraints = fold $ do
      { currencySymbol, mkConstraint } ← currencySymbolsToBurn
      (_ /\ tokenName /\ amount) ←
        -- Filtering the entire list for each currency symbol is probably
        -- suboptimal. If possible this should be optimised.
        filter
          (\(cs /\ _ /\ _) → cs == currencySymbol)
          (flatten $ getMultiAsset ownValue)
      pure $ mkConstraint { tokenName, amount: unsafePartial $ fromJust $ Int.fromString $ BigNum.toString amount}

  pure
    { constraints
    , lookups: mempty
    }
