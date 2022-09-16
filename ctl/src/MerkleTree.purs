module MerkleTree where

import Contract.Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Types as ArrayBuffer
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.String.Common as String
import Deserialization.FromBytes as Deserialization
import Deserialization.PlutusData as Deserialization
import Effect.Exception (Error)
import Effect.Exception as Exception
import Node.ChildProcess as ChildProcess
import Partial.Unsafe as Unsafe
import Serialization as Serialization
import Serialization.PlutusData as Serialization
import Serialization.Types as Serialization
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData)
import Types.PlutusData as PlutusData
import Unsafe.Coerce as Coerce
import Untagged.Union as Union

-- * Serialisation / deserialisation functions for 'PlutusData'

-- | 'serialisePlutusData' uses 'Serialization.PlutusData.convertPlutusData' internally to convert
-- the given plutus data into the internal cbor'd serialized version of the
-- data.
--
-- Note that we use the UK English spelling instead of the normal English spelling
-- because this corresponds to the spelling given in plutus-apps.
serialisePlutusData ∷ PlutusData → Maybe ByteArray
serialisePlutusData plutusData = Serialization.toBytes <<< Union.asOneOf <$>
  Serialization.convertPlutusData plutusData

-- | 'unsafeSerialiseData' is 'serialisePlutusData' but throws an error in the case
-- of 'Nothing' via 'Maybe.fromJust'.
-- This is useful to play around with in PSCI.
unsafeSerialiseData ∷ PlutusData → ByteArray
unsafeSerialiseData plutusData = Unsafe.unsafePartial
  ((Maybe.fromJust <<< serialisePlutusData) plutusData)

-- | 'deserialisePlutusData' deserializes cbor encoded PlutusData.
deserialisePlutusData ∷ ByteArray → Maybe PlutusData
deserialisePlutusData cbor =
  (Deserialization.fromBytes cbor ∷ Maybe (Serialization.PlutusData))
    >>= Deserialization.convertPlutusData

-- TODO: in the feature, apparently
-- `Deserialization.fromBytes :: Serialization.PlutusData -> Maybe Serialization.PlutusData`
-- will be removed, and this should be updated accordingly...

-- | 'unsafeDeserialiseData' is 'deserialisePlutusData' but throws an error in the case
-- of 'Nothing'
-- This is useful to play around with in PSCI
unsafeDeserialiseData ∷ ByteArray → PlutusData
unsafeDeserialiseData cbor =
  (Unsafe.unsafePartial (Maybe.fromJust <<< deserialisePlutusData)) cbor

-- * Merkle tree data types
newtype RootHash = RootHash ByteArray

derive instance Generic RootHash _
derive instance Newtype RootHash _

instance Show RootHash where
  show = genericShow

unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash s) = s

data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

instance Show MerkleTree where
  show (Bin h l r) = String.joinWith " " [ "Bin", show h, show l, show r ]
  show (Tip h) = String.joinWith " " [ "Tip", show h ]

-- TODO: need to add the rest of the instances and data types here
-- (`PlutusData` representation, etc.)

-- * Merkle tree functionality
-- | 'merkleTreeExecutable' is the name of the executable to run.
--
-- Currently, it goes back into the previous projects flake, and `nix run`s it.
merkleTreeExecutable ∷ String
merkleTreeExecutable =
  "nix run ..#trustless-sidechain:exe:trustless-sidechain-merkle-tree"

-- * Internal functions for calling the CLI

-- | @'spawnSyncMerkleTree' merkleCmds stdin@ is an internal function which
-- executes the CLI interface with @merkleCmds@ as argument, and piping @stdin@
-- in.
spawnSyncMerkleTree ∷ Array String → ByteArray → Either Error ByteArray
spawnSyncMerkleTree merkleCmds stdin =
  let
    result = spawnSyncImpl merkleTreeExecutable merkleCmds
      { input: (toBufferImpl (Newtype.unwrap stdin ∷ Uint8Array))
      , encoding: "buffer"
      }
    err = Left result.error
  in
    if isNullImpl result.status then err
    else if result.status == 0 -- if the process doesn't fail..
    then Right (Newtype.wrap (fromBufferImpl result.stdout) ∷ ByteArray)
    else err

-- | 'spawnSyncImpl' corresponds exactly to
-- [spawnSync](https://nodejs.org/api/child_process.html#child_processspawnsynccommand-args-options).
--
-- N.B. HUGE WARNING: there is no type safety with this... refer to the `node.js`
-- documentation for what this expects.
foreign import spawnSyncImpl ∷ ∀ a. a

-- | 'Buffer' corresponds to Node's [Buffer](https://nodejs.org/api/buffer.html) type.
foreign import data Buffer ∷ Type

-- | 'toBufferImpl' converts a 'Uint8Array' into 'Buffer'. N.B. internally, this
-- will share the same allocated memory as 'Uint8Array'. See
-- [here](https://nodejs.org/api/buffer.html#buffers-and-typedarrays)
foreign import toBufferImpl ∷ Uint8Array → Buffer

-- | 'fromBufferImpl' converts a 'Buffer' into 'Uint8Array'. N.B. internally, this
-- will share the same allocated memory as 'Buffer'. See
-- [here](https://nodejs.org/api/buffer.html#buffers-and-typedarrays)
foreign import fromBufferImpl ∷ Buffer → Uint8Array

-- | 'isNullImpl' returns true iff the argument is @_ === null@ i.e., JavaScript's @null@.
-- This is useful for 'spawnSyncImpl' where various arguments may be
-- javascript's @null@ to indicate the presence of an error.
foreign import isNullImpl ∷ ∀ a. a → Boolean

-- internal function used to help debug things..
foreign import id ∷ ∀ a. a → a

-- Notes.
-- Why we didn't use the prepackaged function `Node.ChildProcess.execSync` for
-- 'spawnSyncMerkleTree'?
-- That version of the function only accepts `String`s for `stdin` (i.e., should
-- be valid UTF8), and we will be dumping binary output to it. a bit of a library
-- oversight perhaps....
