{- |
 Module      : Main
 Description : A CLI for 'TrustlessSidechain.MerkleTree'

 This is is a CLI for 'TrustlessSidechain.MerkleTree' supporting input via cbor
 encoded BuiltinData.
-}
module Main (main, serialiseBuiltinData) where

import Codec.Serialise qualified as Serialise
import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Applicative
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.IsData.Class (FromData, ToData)
import PlutusTx.IsData.Class qualified as IsData
import System.IO (Handle, IOMode (ReadMode, WriteMode))
import System.IO qualified as IO
import TrustlessSidechain.MerkleTree (MerkleProof, MerkleTree, RootHash)
import TrustlessSidechain.MerkleTree qualified as MerkleTree

-- N.B. technically, the @STANDARDS.md@ says something like we shouldn't
-- import 'Prelude' unqualified, but this little CLI thing is more of a
-- 'Prelude' project instead of a 'PlutusTx' sorta project and it makes
-- more sense to just use 'Prelude' here..
import Prelude

-- * Utility functions

{- | 'serialiseBuiltinData'
 converts the given data to its 'BuiltinData' instance, and serializes that
 to cbor i.e.,
 > 'serialiseBuiltinData' = 'Builtins.serialiseData' . 'PlutusTx.IsData.Class.toBuiltinData'

 Why is this here? Well, it was a helpful wrapper used in GHCi when developing
 this.

 Some notes on [cbor](https://www.rfc-editor.org/rfc/rfc8949.html) (although
 for a detailed treatment, just see the RFC). It appears that Plutus encodes
 lists as indefinite length lists i.e., lists will be prefixed by @0x9f...@
 and terminated with @0xff@.
-}
serialiseBuiltinData :: ToData a => a -> BuiltinByteString
serialiseBuiltinData = Builtins.serialiseData . IsData.toBuiltinData

{- | @'unBuiltinByteString' bs@ unwraps the 'BuiltinByteString' type to give
 the underlying (strict) 'ByteString'.
-}
unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString bs) = bs

-- * Parsing

{- | 'Input' is a newtype wrapper for @Maybe FilePath@ where @'Just' 'FilePath'@
 denotes parsing a 'FilePath', and 'Nothing' denotes no input was provided
 and we are hence interested in reading input from stdin.
-}
newtype Input = Input (Maybe FilePath)
  deriving newtype (Show, Eq)

{- | 'Output' is a newtype wrapper for @Maybe FilePath@ and is identical to
 'Input' except 'Nothing' denotes no input was provided and we are hence
 interested in writing output to stdout.
-}
newtype Output = Output (Maybe FilePath)
  deriving newtype (Show, Eq)

{- | 'pFileInput' parses an input file and returns 'Nothing' as the default.
 The 'Nothing' is used to represent stdin.
-}
pFileInput :: Parser Input
pFileInput =
  fmap Input $
    Applicative.option (fmap Just Applicative.str) $
      Applicative.long "input"
        <> Applicative.short 'i'
        <> Applicative.value Nothing
        <> Applicative.metavar "FILENAME"
        <> Applicative.help "Input file (defaults to stdin)"

{- | 'pFileOutput' parses an output file and returns 'Nothing' as the default.
 The 'Nothing' is used to represent stdout.

 N.B. this is essentially duplicated code from 'pFileOutput' aside from
 changes to the help text.
-}
pFileOutput :: Parser Output
pFileOutput =
  fmap Output $
    Applicative.option (fmap Just Applicative.str) $
      Applicative.long "output"
        <> Applicative.short 'o'
        <> Applicative.value Nothing
        <> Applicative.metavar "FILENAME"
        <> Applicative.help "Output file (defaults to stdout)"

-- * Readers

{- $readers
 *Readers* are methods used to read data from input
-}

{- | 'MerkleTreeAction' is a (hopefully) convenient intermediate type which
 represents the merkle tree function we wish to execute and its associated
 arguments.
-}
data MerkleTreeAction
  = -- | Correponds to @\bs -> 'MerkleTree.fromList' bs@
    FromList [BuiltinByteString]
  | -- | Correponds to @\bs -> 'MerkleTree.rootHashFromList' bs@
    RootHashFromList [BuiltinByteString]
  | -- | Correponds to @\(bs, mt) -> 'MerkleTree.lookupMp' bs mt@
    LookupMp (BuiltinByteString, MerkleTree)
  | -- | Correponds to @\(bs, mp, rh) -> 'MerkleTree.memberMp' bs mp rh@
    MemberMp (BuiltinByteString, MerkleProof, RootHash)
  | -- | Correponds to @\bs -> 'MerkleTree.lookupsMpFromList' bs@
    LookupsMpFromList [BuiltinByteString]

{- | 'readBuiltinDataCbor' reads a cbor encoded 'BuiltinData' representation
 of the given arguments e.g. if the argument is @["pomeranian", "maltese"]@
 (say, correponding to 'FromList' in 'MerkleTreeAction'), then this will
 attempt to read @cbor(toBuiltinData(["pomeranian", "maltese"]))@
-}
readBuiltinDataCbor :: MerkleTreeOption -> Handle -> IO MerkleTreeAction
readBuiltinDataCbor opt handle = case opt of
  EncodeFromList -> FromList <$> arg
  EncodeRootHashFromList -> RootHashFromList <$> arg
  EncodeLookupMp -> LookupMp <$> arg
  EncodeMemberMp -> MemberMp <$> arg
  EncodeLookupsMpFromList -> LookupsMpFromList <$> arg
  where
    -- Generic function which reads from input and deserializes to BuiltinData,
    -- and transforms that to the corresponding Haskell data type
    arg :: FromData a => IO a
    arg =
      Lazy.hGetContents handle
        >>= ( \builtinData ->
                case IsData.fromBuiltinData builtinData of
                  Just deserializedArg -> return deserializedArg
                  Nothing ->
                    Exception.throwIO $
                      userError ("BuiltinData deserialization failed: " ++ show builtinData)
            )
          . Serialise.deserialise

-- * Writers

{- $writers
 *Writers* are methods used to write the data to the given handle.
-}

{- | @'writeCborBuiltinData' mta handle@ will execute the associated merkle
 tree command given by the 'MerkleTreeAction' @mta@, and write the result on
 the given handle encoded as @cbor(toBuiltinData(result))@.
-}
writeCborBuiltinData :: MerkleTreeAction -> Handle -> IO ()
writeCborBuiltinData mta handle = case mta of
  FromList arg -> writer $ MerkleTree.fromList arg
  RootHashFromList arg -> writer $ MerkleTree.rootHashFromList arg
  LookupMp arg -> writer $ uncurry MerkleTree.lookupMp arg
  MemberMp arg -> writer $ (\(bs, mp, rh) -> MerkleTree.memberMp bs mp rh) arg
  LookupsMpFromList arg -> writer $ MerkleTree.lookupsMpFromList arg
  where
    writer :: ToData a => a -> IO ()
    writer =
      ByteString.hPutStr handle
        . unBuiltinByteString
        . Builtins.serialiseData
        . IsData.toBuiltinData

-- * Input / Output

{- $inputoutput
 This section includes functions help work with the 'Input' and 'Output' types.
-}

{- | @'withInputOutput' input output go@ opens up two 'Handle's from @input@
 (either a file [using 'IO.withFile'] or stdin) and @output@ (either a file
 [using 'IO.withFile'] or stdout) resp; and executes @go@ with the
 aforementioned 'Handle's.

 This is a helper function for the remaining functions in this section.
-}
withInputOutput :: Input -> Output -> (Handle -> Handle -> IO a) -> IO a
withInputOutput (Input input) (Output output) go = case input of
  Just inFp -> case output of
    Just outFp -> IO.withFile inFp ReadMode $
      \inHandle -> IO.withFile outFp WriteMode $ go inHandle
    Nothing -> IO.withFile inFp ReadMode (`go` IO.stdout)
  Nothing -> case output of
    Just outFp -> IO.withFile outFp WriteMode $ go IO.stdin
    Nothing -> go IO.stdin IO.stdout

-- * Main

-- | 'MerkleTreeCliOptions' is a pure representation of parsed cli input.
data MerkleTreeCliOptions = MerkleTreeCliOptions
  { -- | 'mtoAction' indicates which merkle tree action we are interested
    -- in performing -- see 'MerkleTreeOption'
    mtoAction :: MerkleTreeOption
  , -- | 'mtoInput' is the 'Input' handle to get the arguments for the
    -- given 'mtoAction'.
    mtoInput :: Input
  , -- | 'mtoOutput' is the 'Output' handle to put the result of the given
    -- 'mtoAction'.
    mtoOutput :: Output
  }
  deriving stock (Show, Eq)

{- | 'MerkleTreeOption' is a sum type representing the action to execute for
 the merkle tree.
-}
data MerkleTreeOption
  = -- | Correponds to 'MerkleTree.fromList'
    EncodeFromList
  | -- | Correponds to 'MerkleTree.rootHashFromList'
    EncodeRootHashFromList
  | -- | Correponds to @\(bs, mt) -> 'MerkleTree.lookupMp' bs mt@
    EncodeLookupMp
  | -- | Correponds to @\(bs, mp, mt) -> 'MerkleTree.memberMp' bs mp mt@
    EncodeMemberMp
  | -- | Correponds to @\bs -> 'MerkleTree.lookupsMpFromList' bs@
    EncodeLookupsMpFromList
  deriving stock (Show, Eq)

{- | 'options' is the main runner of the application. It returns
 'MerkleTreeCliOptions' which is a pure representation of what the rest of the
 application should do.
-}
options :: Parser MerkleTreeCliOptions
options =
  Applicative.subparser $
    Prelude.mconcat
      [ Applicative.command "fromList" $
          Applicative.info
            (MerkleTreeCliOptions EncodeFromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "fromList"
                <> Applicative.progDesc
                  "Given a list of `BuiltinByteString`s `bs` encoded as `cbor(toBuiltinData(bs))`, returns the cbor encoding of the `BuiltinData` of the corresponding merkle tree."
            )
      , Applicative.command "rootHashFromList" $
          Applicative.info
            (MerkleTreeCliOptions EncodeRootHashFromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "rootHashFromList"
                <> Applicative.progDesc
                  "Given a list of `BuiltinByteString`s `bs` encoded as `cbor(toBuiltinData(bs))`, returns the cbor encoding of the `BuiltinData` of the root hash of the corresponding merkle tree."
            )
      , Applicative.command "lookupMp" $
          Applicative.info
            (MerkleTreeCliOptions EncodeLookupMp <$> pFileInput <*> pFileOutput)
            ( Applicative.header "lookupMp"
                <> Applicative.progDesc
                  "Given `cbor( toBuiltinData (bs,merkleTree) )`, this returns `cbor(toBuiltinData(lookupMp bs merkleTree))`"
            )
      , Applicative.command "memberMp" $
          Applicative.info
            (MerkleTreeCliOptions EncodeMemberMp <$> pFileInput <*> pFileOutput)
            ( Applicative.header "memberMp"
                <> Applicative.progDesc
                  "Given `cbor(toBuiltinData (bs,merkleProof,rootHash) )`, this returns `cbor(toBuiltinData(memberMp bs merkleProof rootHash))`"
            )
      , Applicative.command "lookupsMpFromList" $
          Applicative.info
            (MerkleTreeCliOptions EncodeMemberMp <$> pFileInput <*> pFileOutput)
            ( Applicative.header "lookupsMpFromList"
                <> Applicative.progDesc
                  "Given `cbor(toBuiltinData(bs))`, this returns `cbor(toBuiltinData(lookupsMpFromList bs))`"
            )
      ]

{- | 'main' is the main function.

 For help in this repo, type
 > cabal run trustless-sidechain-merkle-tree -- --help

 Example. Computing the cbor encoded BuiltinData of a merkle tree.
 Let @input@ be the file of the binary representation of

 > cbor (toBuiltinData ["pomeranian", "maltese", "yorkie"])

 To get the root hash of the corresponding merkle tree on stdout, type

 > cabal run trustless-sidechain-merkle-tree -- fromList --input=input

 Alternatively, you could write it to a file @output@ via

 > cabal run trustless-sidechain-merkle-tree -- fromList --input=input --output=output

 Moreover, you could pipe @input@ in as stdin (and dump the binary output to a
 file @output@) via

 > cat input | cabal run trustless-sidechain-merkle-tree -- fromList --output=output
-}
main :: IO ()
main =
  let opts :: ParserInfo MerkleTreeCliOptions
      opts = Applicative.info (Applicative.helper <*> options) Applicative.idm
   in Applicative.execParser opts >>= \cliOpts ->
        withInputOutput (mtoInput cliOpts) (mtoOutput cliOpts) $
          \inputHandle outputHandle ->
            readBuiltinDataCbor (mtoAction cliOpts) inputHandle
              >>= \action -> writeCborBuiltinData action outputHandle
