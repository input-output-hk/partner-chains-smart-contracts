-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.textEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module Main (main) where

import Cardano.Api (PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Foldable (traverse_)
import Ledger (Script, Versioned (unversioned), scriptHash)
import Plutonomy.UPLC qualified
import TrustlessSidechain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.PoCECDSA qualified as PoCECDSA
import TrustlessSidechain.PoCInlineDatum qualified as PoCInlineDatum
import TrustlessSidechain.PoCReferenceInput qualified as PoCReferenceInput
import TrustlessSidechain.PoCReferenceScript qualified as PoCReferenceScript
import TrustlessSidechain.PoCSerialiseData qualified as PoCSerialiseData
import TrustlessSidechain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude

-- CTL uses the usual TextEnvelope format now.
serialiseScript :: FilePath -> Versioned Script -> IO ()
serialiseScript name script =
  -- Plutonomy.UPLC.optimizeUPLC
  let out =
        PlutusScriptSerialised @PlutusScriptV2
          . toShort
          . toStrict
          . serialise
          . Plutonomy.UPLC.optimizeUPLC
          $ unversioned script
      file = "ctl/Scripts/" <> name <> ".plutus"
   in do
        putStrLn $ "serialising " <> name <> ",\thash = " <> show (scriptHash script)
        writeFileTextEnvelope file Nothing out >>= either print pure

main :: IO ()
main =
  traverse_
    (uncurry serialiseScript)
    [ ("FUELMintingPolicy", FUELMintingPolicy.serialisableMintingPolicy)
    , ("MPTRootTokenValidator", MPTRootTokenValidator.serialisableValidator)
    , ("MPTRootTokenMintingPolicy", MPTRootTokenMintingPolicy.serialisableMintingPolicy)
    , ("CommitteeCandidateValidator", CommitteeCandidateValidator.serialisableValidator)
    , ("CommitteeHashPolicy", UpdateCommitteeHash.serialisableCommitteeHashPolicy)
    , ("CommitteeHashValidator", UpdateCommitteeHash.serialisableCommitteeHashValidator)
    , -- Distributed set validators / minting policies
      ("InsertValidator", DistributedSet.serialisableInsertValidator)
    , ("DsConfValidator", DistributedSet.serialisableDsConfValidator)
    , ("DsConfPolicy", DistributedSet.serialisableDsConfPolicy)
    , ("DsKeyPolicy", DistributedSet.serialisableDsKeyPolicy)
    , -- Validators for proof of concept tests.
      ("PoCInlineDatum", PoCInlineDatum.serialisablePoCInlineDatumValidator)
    , ("PoCToReferenceInput", PoCReferenceInput.serialisablePoCToReferenceInputValidator)
    , ("PoCReferenceInput", PoCReferenceInput.serialisablePoCReferenceInputValidator)
    , ("PoCToReferenceScript", PoCReferenceScript.serialisablePoCToReferenceScriptValidator)
    , ("PoCReferenceScript", PoCReferenceScript.serialisablePoCReferenceScriptValidator)
    , ("PoCSerialiseData", PoCSerialiseData.serialisablePoCSerialiseData)
    , ("PoCECDSA", PoCECDSA.serialisableValidator)
    ]
