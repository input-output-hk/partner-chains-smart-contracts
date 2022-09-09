{- |
 Module      : Main
 Description : A CLI for 'TrustlessSidechain.MerkleTree'

 This is is a CLI for 'TrustlessSidechain.MerkleTree' supporting input via cbor
 encoded BuiltinData.
-}
module Main (main) where

import Codec.Serialise qualified as Serialise
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Applicative
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.IsData.Class (FromData, ToData)
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude qualified as PlutusTx
import System.IO (Handle, IOMode (ReadMode, WriteMode))
import System.IO qualified as IO
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
 > 'serialiseBuiltinData' = 'Builtins.serialiseData' . 'Class.toBuiltinData'

 Some notes on [cbor](https://www.rfc-editor.org/rfc/rfc8949.html) (although
 for a detailed treatment, just see the RFC). It appears that Plutus encodes
 lists as indefinite length lists i.e., lists will be prefixed by @0x9f...@
 and terminated with @0xff@
-}
serialiseBuiltinData :: ToData a => a -> BuiltinByteString
serialiseBuiltinData = Builtins.serialiseData . Class.toBuiltinData

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
newtype Input = Input {unInput :: Maybe FilePath}

{- | 'Output' is a newtype wrapper for @Maybe FilePath@ and is identical to
 'Input' except 'Nothing' denotes no input was provided and we are hence
 interested in writing output to stdout.
-}
newtype Output = Output {unOutput :: Maybe FilePath}

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

{- | 'readerBuiltinDataCbor' reads a cbor encoded 'BuiltinData' representation
 of the given arguments e.g. if the argument is @["pomeranian", "maltese"]@,
 then this will read @cbor(toBuiltinData(["pomeranian", "maltese"]))@
-}
readerBuiltinDataCbor :: FromData a => Handle -> IO a
readerBuiltinDataCbor input =
  Lazy.hGetContents input
    >>= PlutusTx.maybe
      (Exception.throwIO $ userError "BuiltinData deserialization failed")
      return
      . Class.fromBuiltinData
      . Serialise.deserialise

-- * Writers

{- $writers
 *Writers* are methods used to write the data to the given handle.
-}

{- | @'writerCborBuiltinData' handle a@ will write @cbor(toBuiltinData(a))@ to
 @handle@.
-}
writerCborBuiltinData :: ToData a => Handle -> a -> IO ()
writerCborBuiltinData output =
  ByteString.hPutStr output
    . unBuiltinByteString
    . Builtins.serialiseData
    . Class.toBuiltinData

-- * Exposed Functionality

{- $exposedFunctionality
 This section includes functions which expose functionality of the
 'TrustlessSidechain.MerkleTree'

 In the future, we can provide various commands for various input and output
 formats.
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

-- | 'fromList' is a wrapper for 'MerkleTree.fromList'.
fromList :: Input -> Output -> IO ()
fromList input output = withInputOutput input output $ \inHandle outHandle ->
  readerBuiltinDataCbor inHandle >>= writerCborBuiltinData outHandle . MerkleTree.fromList

-- | 'rootHashFromList' is a wrapper for 'MerkleTree.rootHashFromList'.
rootHashFromList :: Input -> Output -> IO ()
rootHashFromList input output = withInputOutput input output $ \inHandle outHandle ->
  readerBuiltinDataCbor inHandle >>= writerCborBuiltinData outHandle . MerkleTree.rootHashFromList

{- | 'lookupMp' is a wrapper for @uncurry . MerkleTree.lookupMp@. Note that
 this expects the arguments as a cbor encoded tuple.
-}
lookupMp :: Input -> Output -> IO ()
lookupMp input output = withInputOutput input output $ \inHandle outHandle ->
  readerBuiltinDataCbor inHandle
    >>= writerCborBuiltinData outHandle . uncurry MerkleTree.lookupMp

{- | 'memberMp' is a wrapper for @\(bs, merkleProof, rootHash) ->
 MerkleTree.memberMp bs merkleProof rootHash@.
 Note that the arguments are expected to be a cbor encoded triplet.
-}
memberMp :: Input -> Output -> IO ()
memberMp input output = withInputOutput input output $ \inHandle outHandle ->
  readerBuiltinDataCbor inHandle
    >>= writerCborBuiltinData outHandle
      . (\(bs, merkleProof, rootHash) -> MerkleTree.memberMp bs merkleProof rootHash)

-- * Main

{- | 'options' is the main runner of the application. It returns the IO actions
 which correspond to the exposed functionality of the merkle tree.
-}
options :: Parser (IO ())
options =
  Applicative.subparser $
    Prelude.mconcat
      [ Applicative.command "fromList" $
          Applicative.info
            (fromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "fromList"
                <> Applicative.progDesc
                  "Given a list of `BuiltinByteString`s `bs` encoded as `cbor(toBuiltinData(bs))`, returns the cbor encoding of the `BuiltinData` of the corresponding merkle tree."
            )
      , Applicative.command "rootHashFromList" $
          Applicative.info
            (rootHashFromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "rootHashFromList"
                <> Applicative.progDesc
                  "Given a list of `BuiltinByteString`s `bs` encoded as `cbor(toBuiltinData(bs))`, returns the cbor encoding of the `BuiltinData` of the root hash of the corresponding merkle tree."
            )
      , Applicative.command "lookupMp" $
          Applicative.info
            (rootHashFromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "lookupMp"
                <> Applicative.progDesc
                  "Given `cbor( toBuiltinData (bs,merkleTree) )`, this returns `cbor(toBuiltinData(lookupMp bs merkleTree))`"
            )
      , Applicative.command "memberMp" $
          Applicative.info
            (rootHashFromList <$> pFileInput <*> pFileOutput)
            ( Applicative.header "memberMp"
                <> Applicative.progDesc
                  "Given `cbor(toBuiltinData (bs,merkleProof,rootHash) )`, this returns `cbor(toBuiltinData(memberMp bs merkleProof rootHash))`"
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
  let opts :: ParserInfo (IO ())
      opts =
        Applicative.info
          (Applicative.helper <*> options)
          Applicative.idm
   in Monad.join $ Applicative.execParser opts
