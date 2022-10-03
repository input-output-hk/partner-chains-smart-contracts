{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.TrustlessSidechain.OnChain.MerkleTree where

import TrustlessSidechain.OffChain.Types (RegisterParams (..), SidechainParams)

import TrustlessSidechain.MerkleTree qualified as MerkleTree

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

-- import Cardano.Crypto.Wallet qualified as Wallet
-- import Codec.Serialise (serialise)
import Data.ByteString qualified as ByteString


--import Data.ByteString.Lazy qualified as LBS
--import Data.ByteString.Short qualified as SBS
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (TxOutRef (TxOutRef))
import Ledger.TxId (TxId (TxId))
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
 )

import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, submitTxConstraintsWith, utxosAt)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))

import Test.Plutip.Contract (assertExecution, initAda, withContract, withContractAs)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)

import Test.Plutip.Internal.Types qualified as PlutipInternal

import Prelude (Semigroup ((<>)))
import Prelude qualified

-- import Prelude
import Data.Row (Empty)
import Data.Map (keys)
import Data.Text (Text)

data MerkleRedeemer = MerkleRedeemer
  { rootHash :: MerkleTree.RootHash
  , leaf :: BuiltinByteString
  , merkleProof :: MerkleTree.MerkleProof
  }

PlutusTx.makeIsDataIndexed ''MerkleRedeemer [('MerkleRedeemer, 0)]

{-# INLINEABLE mkMerkleTreeValidator #-}
mkMerkleTreeValidator :: () -> MerkleRedeemer -> Ledger.ScriptContext -> Bool
mkMerkleTreeValidator _ (MerkleRedeemer root l mp) _ =
  traceIfFalse "MerkProof is invalid" $ MerkleTree.memberMp l mp root

merkleTreeValidator :: TypedValidator MerkleValidator
merkleTreeValidator =
  Scripts.mkTypedValidator @MerkleValidator
    ($$(PlutusTx.compile [||mkMerkleTreeValidator||]))
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @MerkleRedeemer

data MerkleValidator
instance ValidatorTypes MerkleValidator where
  type DatumType MerkleValidator = ()
  type RedeemerType MerkleValidator = MerkleRedeemer
  

lock :: [BuiltinByteString] -> Contract () Empty Text Ledger.CardanoTx -- Text seemed like the right type to use here but might need to change
lock bs = do
  let val = Ada.lovelaceValueOf 1
      validator = merkleTreeValidator
      lookups = Constraints.typedValidatorLookups validator
      tx = Constraints.mustPayToTheScript () val

  submitTxConstraintsWith @MerkleValidator lookups tx

unlock :: BuiltinByteString -> [BuiltinByteString] -> Contract () Empty Text Ledger.CardanoTx
unlock bs listBs = do
  let merkleTree = MerkleTree.fromList listBs -- this needed to be listBs instead of bs
      rootH = MerkleTree.rootHash merkleTree
      mProof = MerkleTree.lookupMp bs merkleTree

      red = MerkleRedeemer {rootHash = rootH, leaf = bs, merkleProof = mProof}

      validator = merkleTreeValidator
      lookups = Constraints.typedValidatorLookups validator
      valAddr = Scripts.validatorAddress validator

  valUtxos <- utxosAt valAddr

  let tx =
        mconcat $
          map
            (`Constraints.mustSpendScriptOutput` red)
            (keys valUtxos) -- might need Map.keys

  submitTxConstraintsWith @MerkleValidator lookups tx
