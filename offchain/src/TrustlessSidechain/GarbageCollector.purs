-- | `TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
module TrustlessSidechain.GarbageCollector
  ( mkBurnNFTsLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( mkTxUnspentOut
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Ctl.Internal.Plutus.Types.Value (flattenValue)
import Data.Array (filter)
import Data.BigInt as BigInt
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as EcdsaATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as SchnorrATMSPolicy
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

-- | `burnNFTsLookupsAndConstraints` burns all waste NFTs found on users wallet address
mkBurnNFTsLookupsAndConstraints ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkBurnNFTsLookupsAndConstraints sidechainParams = do

  ownValue ← getOwnUTxOsTotalValue
  committeeCertificateMint ←
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
          { version: BigInt.fromInt 1
          , scriptId: CommitteeCertificateVerificationPolicy
          }
      )

  (fuelMintingPolicyRefTxInput /\ fuelMintingPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: FUELMintingPolicy }
      )

  (fuelBurningPolicyRefTxInput /\ fuelBurningPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: FUELBurningPolicy }
      )

  let
    currencySymbolsToBurn =
      [ { currencySymbol: committeePlainEcdsaSecp256k1ATMSCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                ( Scripts.mintingPolicyHash
                    committeePlainEcdsaSecp256k1ATMSCurrencyInfo
                )
                (Redeemer $ toData EcdsaATMSPolicy.ATMSBurn)
                tokenName
                (-amount)
                ( RefInput $ mkTxUnspentOut
                    committeeCertificateVerificationVersioningInput
                    committeeCertificateVerificationVersioningOutput
                )
            )
        }
      , { currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                ( Scripts.mintingPolicyHash
                    committeePlainSchnorrSecp256k1ATMSCurrencyInfo
                )
                (Redeemer $ toData SchnorrATMSPolicy.ATMSBurn)
                tokenName
                (-amount)
                ( RefInput $ mkTxUnspentOut
                    committeeCertificateVerificationVersioningInput
                    committeeCertificateVerificationVersioningOutput
                )
            )
        }
      , { currencySymbol: fuelMintingCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                (Scripts.mintingPolicyHash fuelMintingPolicy)
                (Redeemer $ toData MintingV1.FUELBurningRedeemer)
                tokenName
                (-amount)
                ( RefInput $ mkTxUnspentOut
                    fuelMintingPolicyRefTxInput
                    fuelMintingPolicyRefTxOutput
                )
            )
        }
      , { currencySymbol: fuelBurningCurrencySymbol
        , mkConstraint: \{ tokenName, amount } →
            ( TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
                (Scripts.mintingPolicyHash fuelBurningPolicy)
                unitRedeemer
                tokenName
                (-amount)
                ( RefInput $ mkTxUnspentOut
                    fuelBurningPolicyRefTxInput
                    fuelBurningPolicyRefTxOutput
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
          (flattenValue ownValue)
      pure $ mkConstraint { tokenName, amount }

  pure
    { constraints
    , lookups: mempty
    }
