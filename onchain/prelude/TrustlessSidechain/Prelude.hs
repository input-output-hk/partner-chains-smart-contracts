{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrustlessSidechain.Prelude (
  -- * Types

  -- ** Basic
  Type,
  Constraint,
  Bool.Bool (..),
  Ord.Ordering (..),
  Char.Char,
  Maybe.Maybe (..),
  Either.Either (..),
  These.These (..),
  IO.IO,
  Proxy.Proxy (..),

  -- ** Collections
  NonEmpty.NonEmpty (..),
  Map.Map,
  Vector.Vector,
  Set.Set,
  Text.Text,
  ByteString.ByteString,

  -- ** Numerical
  Int.Int,
  Int.Int8,
  Int.Int16,
  Int.Int32,
  Int.Int64,
  Integer.Integer,
  Word.Word,
  Word.Word8,
  Word.Word16,
  Word.Word32,
  Word.Word64,
  Natural.Natural,
  Float.Float,
  Float.Double,

  -- * Type classes

  -- ** Basic
  Eq.Eq (..),
  Ord.Ord (..),
  Show.Show,
  Read.Read,

  -- ** Semigroup hierarchy
  Semigroup.Semigroup (..),
  Monoid.Monoid (mempty, mconcat),

  -- ** Numerical
  Semiring.Semiring (..),
  Semiring.Ring (..),
  Euclidean.Euclidean (..),
  Euclidean.Field,
  Euclidean.GcdDomain (..),

  -- ** Functor hierarchy
  Functor.Functor (..),
  Bifunctor.Bifunctor (..),
  Foldable.Foldable (
    fold,
    foldMap,
    foldMap',
    foldr,
    foldl,
    foldl',
    length
  ),
  Witherable.Filterable (..),
  Traversable.Traversable (traverse, sequenceA),
  Witherable.Witherable (wither, filterA, witherMap),
  Applicative.Applicative (..),
  Applicative.Alternative (..),
  Monad.Monad ((>>=), (>>)),

  -- ** Indexed functor hierarchy
  FunctorWithIndex.FunctorWithIndex (..),
  FoldableWithIndex.FoldableWithIndex (..),
  Witherable.FilterableWithIndex (..),
  TraversableWithIndex.TraversableWithIndex (..),
  Witherable.WitherableWithIndex (..),

  -- ** Monadic options
  Monad.MonadFail (..),
  Monad.MonadPlus,
  MonadIO.MonadIO (..),

  -- ** Semialign hierarchy
  Semialign.Semialign (..),
  SemialignWithIndex.SemialignWithIndex (..),
  Semialign.Align (..),
  Semialign.Unalign (..),
  Semialign.Zip (..),
  SemialignWithIndex.ZipWithIndex (..),
  Semialign.Repeat (..),
  SemialignWithIndex.RepeatWithIndex (..),
  Semialign.Unzip (..),

  -- ** Other
  Category.Category (..),
  Exts.IsList (fromList, fromListN),

  -- * Functions

  -- ** Logical
  (Bool.&&),
  (Bool.||),
  Bool.not,
  Bool.bool,
  Foldable.and,
  Foldable.or,
  Foldable.any,
  FoldableWithIndex.iany,
  Foldable.all,
  FoldableWithIndex.iall,
  FoldableWithIndex.none,
  FoldableWithIndex.inone,
  equating,

  -- ** Order
  Ord.comparing,

  -- ** Numerical
  (Semiring.+),
  (Semiring.*),
  (Semiring.-),
  (Field./),
  Semiring.minus,
  Semiring.isZero,
  Semiring.isOne,
  Semiring.fromInteger,
  Semiring.fromIntegral,
  Euclidean.gcdExt,
  Field.fromRational,
  Field.recip,

  -- ** Functor
  (Functor.$>),
  (Functor.<$>),
  (Functor.<&>),
  Functor.void,

  -- ** Foldable
  Foldable.traverse_,
  FoldableWithIndex.itraverse_,
  Foldable.for_,
  FoldableWithIndex.ifor_,
  Foldable.sequenceA_,
  Foldable.asum,
  Foldable.concat,
  Foldable.concatMap,
  FoldableWithIndex.iconcatMap,
  Foldable.find,
  FoldableWithIndex.ifind,
  FoldableWithIndex.itoList,
  Semiring.foldMapP,
  Semiring.foldMapT,
  Semiring.sum,
  Semiring.product,
  Semiring.sum',
  Semiring.product',

  -- ** Filterable
  (Witherable.<$?>),
  (Witherable.<&?>),

  -- ** Traversable
  Traversable.for,
  TraversableWithIndex.ifor,
  Traversable.mapAccumL,
  TraversableWithIndex.imapAccumL,
  Traversable.mapAccumR,
  TraversableWithIndex.imapAccumR,

  -- ** Witherable
  Witherable.ordNub,
  Witherable.ordNubOn,
  Witherable.forMaybe,

  -- ** Applicative
  (Applicative.<**>),
  Applicative.liftA3,
  Monad.forever,
  mapAndUnzipA,
  zipWithA,
  zipWithA_,
  replicateA,
  replicateA_,
  Monad.when,
  Monad.unless,

  -- ** Alternative
  Applicative.optional,
  Monad.guard,

  -- ** Monad
  (Monad.=<<),
  (Monad.>=>),
  (Monad.<=<),
  Monad.join,
  Monad.mfilter,
  Foldable.foldrM,
  FoldableWithIndex.ifoldrM,
  Foldable.foldlM,
  FoldableWithIndex.ifoldlM,
  Monad.foldM_,

  -- ** Semialign
  Semialign.salign,
  Semialign.padZip,
  Semialign.padZipWith,
  Semialign.lpadZip,
  Semialign.lpadZipWith,
  Semialign.rpadZip,
  Semialign.rpadZipWith,
  Semialign.alignVectorWith,

  -- ** Category
  (Category.<<<),
  (Category.>>>),

  -- ** Maybe
  Maybe.maybe,
  Maybe.fromMaybe,
  Maybe.listToMaybe,
  Maybe.maybeToList,

  -- ** Either
  Either.either,
  Either.lefts,
  Either.rights,
  Either.fromLeft,
  Either.fromRight,
  Either.partitionEithers,

  -- ** These
  These.these,
  These.fromThese,
  These.mergeThese,
  These.mergeTheseWith,

  -- ** Text
  ByteString.getLine,
  ByteString.getContents,
  ByteString.readFile,
  ByteString.writeFile,
  ByteString.appendFile,
  TextIO.putStr,
  TextIO.putStrLn,

  -- ** Other
  Bool.otherwise,
  Tuple.fst,
  Tuple.snd,
  Tuple.curry,
  Tuple.uncurry,
  Tuple.swap,
  show,
  (Function.$),
  (Function.&),
  Function.const,
  Function.flip,
  Function.on,
  fromString,
) where

import Control.Applicative (Applicative)
import Control.Applicative qualified as Applicative
import Control.Category qualified as Category
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Data.Bifunctor qualified as Bifunctor
import Data.Bool (Bool)
import Data.Bool qualified as Bool
import Data.ByteString qualified as ByteString
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either qualified as Either
import Data.Eq (Eq ((==)))
import Data.Eq qualified as Eq
import Data.Euclidean qualified as Euclidean
import Data.Field qualified as Field
import Data.Foldable qualified as Foldable
import Data.Foldable.WithIndex qualified as FoldableWithIndex
import Data.Function qualified as Function
import Data.Functor qualified as Functor
import Data.Functor.WithIndex qualified as FunctorWithIndex
import Data.Int (Int)
import Data.Int qualified as Int
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Monoid qualified as Monoid
import Data.Ord qualified as Ord
import Data.Proxy qualified as Proxy
import Data.Semialign qualified as Semialign
import Data.Semialign.Indexed qualified as SemialignWithIndex
import Data.Semigroup qualified as Semigroup
import Data.Semiring qualified as Semiring
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.These qualified as These
import Data.Traversable qualified as Traversable
import Data.Traversable.WithIndex qualified as TraversableWithIndex
import Data.Tuple qualified as Tuple
import Data.Vector qualified as Vector
import Data.Word qualified as Word
import GHC.Exts qualified as Exts
import GHC.Float qualified as Float
import GHC.IO qualified as IO
import GHC.Integer qualified as Integer
import Numeric.Natural qualified as Natural
import Text.Read qualified as Read
import Text.Show (Show)
import Text.Show qualified as Show
import Witherable qualified

{- | Map the first argument over the list, returning the result as a pair of
 lists. Mainly useful for complex state, or the 'State' monad.

 @since Unreleased
-}
{-# INLINEABLE mapAndUnzipA #-}
mapAndUnzipA ::
  forall (a :: Type) (b :: Type) (c :: Type) (f :: Type -> Type).
  Applicative f =>
  (a -> f (b, c)) ->
  [a] ->
  f ([b], [c])
mapAndUnzipA = Monad.mapAndUnzipM

{- | Generalizes 'zipWith' to arbitrary 'Applicative's.

 @since Unreleased
-}
{-# INLINEABLE zipWithA #-}
zipWithA ::
  forall (a :: Type) (b :: Type) (c :: Type) (f :: Type -> Type).
  Applicative f =>
  (a -> b -> f c) ->
  [a] ->
  [b] ->
  f [c]
zipWithA = Monad.zipWithM

{- | As 'zipWithA', but ignores the result: only the effects of @f@ are
 performed.

 @since Unreleased
-}
{-# INLINEABLE zipWithA_ #-}
zipWithA_ ::
  forall (a :: Type) (b :: Type) (c :: Type) (f :: Type -> Type).
  Applicative f =>
  (a -> b -> f c) ->
  [a] ->
  [b] ->
  f ()
zipWithA_ = Monad.zipWithM_

{- | @'replicateA' n act@ performs the action @act@ @'max' 0 n@ times, gathering
 the results.

 @since Unreleased.
-}
{-# INLINEABLE replicateA #-}
replicateA ::
  forall (a :: Type) (f :: Type -> Type).
  Applicative f =>
  Int ->
  f a ->
  f [a]
replicateA = Monad.replicateM

{- | As 'replicateA', but ignores the result: only the effects of @f@ are
 performed.

 @since Unreleased
-}
{-# INLINEABLE replicateA_ #-}
replicateA_ ::
  forall (a :: Type) (f :: Type -> Type).
  Applicative f =>
  Int ->
  f a ->
  f ()
replicateA_ = Monad.replicateM_

{- | Similar to 'comparing', but for 'Eq' instead of 'Ord'.

 @since Unreleased
-}
{-# INLINEABLE equating #-}
equating ::
  forall (a :: Type) (b :: Type).
  Eq a =>
  (b -> a) ->
  b ->
  b ->
  Bool
equating f x y = f x == f y

{- | Outputs a 'Show' as a 'Text'.

 @since Unreleased
-}
{-# INLINEABLE show #-}
show :: forall (a :: Type). Show a => a -> Text
show = Show.show Category.>>> Text.pack

{- | Designed for use with @RebindableSyntax@ to force string literals to be
 'Text's instead.
-}
{-# INLINEABLE fromString #-}
fromString :: [Char] -> Text
fromString = Text.pack
