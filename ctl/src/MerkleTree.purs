module MerkleTree where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.String.Common as String
import Deserialization.FromBytes as Deserialization.FromBytes
import Deserialization.PlutusData as Deserialization.PlutusData
import Effect.Exception (Error)
import Effect.Exception as Exception
import Partial.Unsafe as Unsafe
import Serialization as Serialization
import Serialization.PlutusData as Serialization.PlutusData
import Serialization.Types as Serialization.Types
import Types.ByteArray (ByteArray)
import Types.ByteArray as ByteArray
import Types.PlutusData (PlutusData)
import Untagged.Union as Union

-- * Serialisation / deserialisation functions for 'PlutusData'

-- | 'serialisePlutusData' uses 'Serialization.PlutusData.convertPlutusData' internally to convert
-- the given plutus data into the internal cbor'd serialized version of the
-- data.
--
-- Note that we use the UK English spelling instead of the normal English spelling
-- because this corresponds to the spelling given in plutus-apps :^).
serialisePlutusData ∷ PlutusData → Maybe ByteArray
serialisePlutusData plutusData = Serialization.toBytes <<< Union.asOneOf <$>
  Serialization.PlutusData.convertPlutusData plutusData

-- | 'unsafeSerialiseData' is 'serialisePlutusData' but throws an error in the case
-- of 'Nothing' via 'Maybe.fromJust'.
-- This is useful to play around with in PSCI.
unsafeSerialiseData ∷ PlutusData → ByteArray
unsafeSerialiseData plutusData = Unsafe.unsafePartial
  ((Maybe.fromJust <<< serialisePlutusData) plutusData)

-- | 'deserialisePlutusData' deserializes cbor encoded PlutusData.
deserialisePlutusData ∷ ByteArray → Maybe PlutusData
deserialisePlutusData cbor =
  ( Deserialization.FromBytes.fromBytes cbor ∷
      Maybe (Serialization.Types.PlutusData)
  )
    >>= Deserialization.PlutusData.convertPlutusData

-- TODO: in the feature, apparently
-- `Deserialization.FromBytes.fromBytes :: Serialization.Types.PlutusData -> Maybe Serialization.Types.PlutusData`
-- will be removed, and this should be updated accordingly...

-- | 'unsafeDeserialiseData' is 'deserialisePlutusData' but throws an error in the case
-- of 'Nothing'
-- This is useful to play around with in PSCI
unsafeDeserialiseData ∷ ByteArray → PlutusData
unsafeDeserialiseData cbor =
  (Unsafe.unsafePartial (Maybe.fromJust <<< deserialisePlutusData)) cbor

-- | 'unsafeByteArrayFromAscii' is a partial function which wraps 'Types.ByteArray.byteArrayFromAscii'
unsafeByteArrayFromAscii ∷ String → ByteArray
unsafeByteArrayFromAscii = Unsafe.unsafePartial
  (Maybe.fromJust <<< ByteArray.byteArrayFromAscii)

-- * Merkle tree data types / helper functions

-- | 'Triple' is used as the argument to the exposed merkle tree functionality
-- that takes three arguments.
data Triple a b c = Triple a b c

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype RootHash = RootHash ByteArray

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data Side
  = L
  | R

-- | See `src/TrustlessSidechain/MerkleTree.hs`
data MerkleTree
  = Bin RootHash MerkleTree MerkleTree
  | Tip RootHash

-- | See `src/TrustlessSidechain/MerkleTree.hs`
rootHash ∷ MerkleTree → RootHash
rootHash (Bin roothash _ _) = roothash
rootHash (Tip roothash) = roothash

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype Up = Up { siblingSide ∷ Side, sibling ∷ RootHash }

-- | See `src/TrustlessSidechain/MerkleTree.hs`
newtype MerkleProof = MerkleProof (Array Up)

unRootHash ∷ RootHash → ByteArray
unRootHash (RootHash ba) = ba

instance Show MerkleTree where
  show (Bin h l r) = String.joinWith " " [ "Bin", show h, show l, show r ]
  show (Tip h) = String.joinWith " " [ "Tip", show h ]

derive instance Generic RootHash _
derive instance Newtype RootHash _
derive instance Eq RootHash
derive instance Ord RootHash

derive instance Generic MerkleProof _
derive instance Newtype MerkleProof _
derive instance Eq MerkleProof

derive instance Generic Up _
derive instance Newtype Up _
derive instance Eq Up

instance Eq Side where
  eq L L = true
  eq R R = true
  eq _ _ = false

instance Show Side where
  show L = "L"
  show R = "R"

instance Show Up where
  show = genericShow

instance Eq MerkleTree where
  eq (Bin rh0 l0 r0) (Bin rh1 l1 r1) =
    rh0 == rh1 && l0 == l1 && r0 == r1
  eq (Tip rh0) (Tip rh1) = rh0 == rh1
  eq _ _ = false

instance Show RootHash where
  show = genericShow

instance Show MerkleProof where
  show = genericShow

-- Note ['ToData' / 'FromData' Instances of the Merkle Tree]
-- All of these instances should correspond to `/src/TrustlessSidechain/MerkleTree.hs`
instance ToData RootHash where
  -- TODO: maybe for these newtype instances we should just get rid of them
  -- (as an optimization) so there's no need to have the 'Constr'
  toData (RootHash ba) = Constr zero [ toData ba ]

instance FromData RootHash where
  fromData plutusData = case plutusData of
    Constr n [ ba ] | n == zero → RootHash <$> fromData ba
    _ → Nothing

instance ToData Side where
  toData L = Constr zero []
  toData R = Constr one []

instance FromData Side where
  fromData plutusData = case plutusData of
    Constr n []
      | n == zero → Just L
      | n == one → Just R
    _ → Nothing

instance ToData Up where
  toData (Up record) = Constr zero
    [ toData record.siblingSide, toData record.sibling ]

instance FromData Up where
  fromData plutusData = case plutusData of
    Constr n [ a, b ]
      | n == zero →
          Up <$> (({ siblingSide: _, sibling: _ }) <$> fromData a <*> fromData b)
    _ → Nothing

instance ToData MerkleProof where
  toData (MerkleProof prf) = Constr zero [ toData prf ]

instance FromData MerkleProof where
  fromData plutusData = case plutusData of
    Constr n [ a ]
      | n == zero → MerkleProof <$> fromData a
    _ → Nothing

instance ToData MerkleTree where
  toData (Bin roothash l r) =
    Constr zero [ toData roothash, toData l, toData r ]
  toData (Tip roothash) =
    Constr one [ toData roothash ]

instance FromData MerkleTree where
  fromData plutusData = case plutusData of
    Constr n args
      | n == zero → case args of
          [ roothash, l, r ] → Bin <$> fromData roothash <*> fromData l <*>
            fromData r
          _ → Nothing
      | n == one → case args of
          [ roothash ] → Tip <$> fromData roothash
          _ → Nothing
    _ → Nothing

instance (ToData a, ToData b, ToData c) ⇒ ToData (Triple a b c) where
  toData (Triple a b c) = Constr zero [ toData a, toData b, toData c ]

instance (FromData a, FromData b, FromData c) ⇒ FromData (Triple a b c) where
  fromData plutusData = case plutusData of
    Constr n [ a, b, c ] | n == zero → Triple <$> fromData a <*> fromData b <*>
      fromData c
    _ → Nothing

-- * Merkle tree functionality
-- | 'merkleTreeExecutable' is the name of the executable to run.
--
-- Currently, it goes back into the previous projects flake, and `nix run`s it.
-- N.B. to run the executable without having it already present in the path
-- (helpful for debugging sometimes), type
-- > nix run ..#trustless-sidechain:exe:trustless-sidechain-merkle-tree
merkleTreeExecutable ∷ String
merkleTreeExecutable =
  "trustless-sidechain-merkle-tree"

-- | See `src/TrustlessSidechain/MerkleTree.hs`
fromList ∷ Array ByteArray → Either Error MerkleTree
fromList inputArray = do
  plutusData ←
    Maybe.maybe
      (Left (Exception.error "MerkleTree.fromList serialisation failure"))
      pure
      $ serialisePlutusData (toData inputArray)

  outputByteArray ← spawnSyncMerkleTree [ "fromList" ] plutusData

  -- the impossible case here *should* never happen as the CLI interface
  -- should only dump out valid data
  merkleTree ←
    Maybe.maybe
      (Left (Exception.error "MerkleTree.fromList deserialisation failure"))
      pure
      $ deserialisePlutusData outputByteArray
      >>= fromData
  pure merkleTree

-- | See `src/TrustlessSidechain/MerkleTree.hs`
rootHashFromList ∷ Array ByteArray → Either Error RootHash
rootHashFromList inputArray = do
  plutusData ←
    Maybe.maybe
      (Left (Exception.error "MerkleTree.rootHashFromList serialisation failure"))
      pure
      $ serialisePlutusData (toData inputArray)

  outputByteArray ← spawnSyncMerkleTree [ "rootHashFromList" ] plutusData

  -- the impossible case here *should* never happen as the CLI interface
  -- should only dump out valid data
  roothash ←
    Maybe.maybe
      ( Left
          (Exception.error "MerkleTree.rootHashFromList deserialisation failure")
      )
      pure
      $ deserialisePlutusData outputByteArray
      >>= fromData
  pure roothash

-- | See `src/TrustlessSidechain/MerkleTree.hs`
lookupMp ∷ ByteArray → MerkleTree → Either Error (Maybe MerkleProof)
lookupMp leaf merkleTree = do
  plutusData ←
    Maybe.maybe
      (Left (Exception.error "MerkleTree.rootHashFromList serialisation failure"))
      pure
      $ serialisePlutusData (toData (leaf /\ merkleTree))

  outputByteArray ← spawnSyncMerkleTree [ "lookupMp" ] plutusData

  -- the impossible case here *should* never happen as the CLI interface
  -- should only dump out valid data
  merkleProof ←
    Maybe.maybe
      ( Left
          (Exception.error "MerkleTree.rootHashFromList deserialisation failure")
      )
      pure
      $ deserialisePlutusData outputByteArray
      >>= fromData
  pure merkleProof

-- | See `src/TrustlessSidechain/MerkleTree.hs`
memberMp ∷ ByteArray → MerkleProof → RootHash → Either Error Boolean
memberMp leaf proof roothash = do
  plutusData ←
    Maybe.maybe
      (Left (Exception.error "MerkleTree.rootHashFromList serialisation failure"))
      pure
      $ serialisePlutusData (toData (Triple leaf proof roothash))

  outputByteArray ← spawnSyncMerkleTree [ "memberMp" ] plutusData

  -- the impossible case here *should* never happen as the CLI interface
  -- should only dump out valid data
  isInRootHash ←
    Maybe.maybe
      ( Left
          (Exception.error "MerkleTree.rootHashFromList deserialisation failure")
      )
      pure
      $ deserialisePlutusData outputByteArray
      >>= fromData
  pure isInRootHash

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
  in
    if isNullImpl result.status then -- recall the documentation says that it is `null` in the case that the subprocess terminated due to a signal

      Left (Exception.error result.signal)
    else if result.status == 0 -- if the process doesn't fail..
    then Right (Newtype.wrap (fromBufferImpl result.stdout) ∷ ByteArray)
    else Left (Exception.error (bufferToStringImpl result.stderr))

-- else Left (Exception.error (fromBufferImpl result.stderr))

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

-- | 'bufferToStringImpl' corresponds to [here](https://nodejs.org/api/buffer.html#buftostringencoding-start-end).
-- We include some sane defaults such as: decoding to utf8 (see documentation for caveats if it isn't valid utf8).
foreign import bufferToStringImpl ∷ Buffer → String

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
