# Testing without tears

## What is this about?

QuickCheck, and property testing in general, are a quagmire of bad documentation
and folklore. A lot of what material exists on the topic of either, or both,
is old, vague, overly-general or straight-up wrong, sometimes at the same time.
This makes it difficult for newcomers to understand how to use the tools
provided, or leads to common errors that lead to frustration or bad test quality.

To remedy this, we have created this list. It contains a mixture of how-tos and
best practices, alongside warnings; our goal is to reduce friction and
learning curves, while helping our users avoid common pitfalls. We also give a
collection of time-saving tips and tricks to help get good tests faster.

## What we solve already

Some of our project settings already address some common pitfalls of QuickCheck
testing. Specifically, we address the following issues in our Cabal file and our
`cabal.project`:

* Automatically parallelize tests
* Get nicer test output

Furthermore, our standards mandate these.

## Generators

### Be careful of `suchThat` and `suchThatMap`

The following functions are provided by QuickCheck for the purpose of
producing values in the ``Gen`` monad subject to constraints:

```haskell
suchThat :: forall (a :: Type) . Gen a -> (a -> Bool) -> Gen a

suchThatMap :: forall (a :: Type) (b :: Type) . Gen a -> (a -> Maybe b) -> Gen b
```

Both of these are implemented in a rather naive way by QuickCheck, essentially
amounting to something like:

1. Generate a random `a` using the provided generator.
2. Apply the predicate (for `suchThat`), or the Kleisli arrow (for
   `suchThatMap`) to the result from 1.
3. If the predicate is true (for `suchThat`), or the Kleisli arrow 'hits' (for
   `suchThatMap`) in 2, return the result; otherwise, goto 1.

Notably, there is absolutely no retry limit or timeout. What this means is
that if the predicate or Kleisli arrow fail often (or worse, _always_, such as
due to an implementation mistake), it can make generators run for a long time
or possibly forever, without even indicating what might be wrong or why.
Furthermore, QuickCheck doesn't (and indeed, _can't_) be any smarter: that would
require inspecting the provenance of the predicate or Kleisli arrow, and force
the generator to act accordingly. This is difficult at best, and outright
impossible at worst, and currently, QuickCheck doesn't even _attempt_ this.

In general, you should avoid using either of these functions where possible. For
example, instead of writing this:

```haskell
-- Could be slow!
nonEmptyList <- suchThat arbitrary (not . null)
```

Do something like this:

```haskell
-- Much better
x <- arbitrary
xs <- arbitrary
let nonEmptyList = x : xs

-- Or, even better, use the NonEmptyList modifier!
```

However, this isn't always possible. In that case, we provide replacement
versions of both `suchThat` and `suchThatMap` in `quickcheck-extra`. These
versions have a built-in retry limit (100 by default); if this limit is ever
reached, they will 'error out'. While not a perfect solution, this at least
ensures that generators will never run too long, and if they do, you'll know
why (but not which generator). If you want a different limit, you can set it to
the value you want using `suchThatRetrying` and `suchThatMapRetrying`
respectively. Plus, our versions even run faster than QuickCheck's!

### Use modifiers

QuickCheck has an entire
[module](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Modifiers.html)
of _modifiers_: newtypes whose only purpose is to provide the ability to
generate (and shrink) restricted subsets of values of the type being wrapped.
Some notable examples (whose names largely speak for themselves) are:

* `Positive`, `Negative`, `NonZero`, `NonNegative` and `NonPositive` for numeric
  types.
* `NonEmptyList`, `SortedList` and `InfiniteList` for lists.
* `ASCIIString`, `UnicodeString` and `PrintableString` for `String`s.

Others exist as well, both in QuickCheck and in other packages. These allow you
more control over your generators with minimal fuss, while avoiding any issues
with `suchThat` or `suchThatMap`. This is _particularly_ important for
shrinkers, as getting a shrinker right for a restricted type can be quite
tedious.

You can use a modifier in several ways:

```haskell
-- Generation
--
-- Pattern matching with type signature
Positive (ix :: Int) <- arbitrary
-- Pattern matching with TypeApplications
Positive ix <- arbitrary @Int
-- Using an 'unwrapper' with type signature
ix :: Int <- getPositive <$> arbitrary
-- Using an 'unwrapper' with TypeApplications
ix <- getPositive <$> arbitrary @Int
--
-- Shrinking
--
-- Pattern matching
Positive ix' <- shrink . Positive $ ix
-- Using an 'unwrapper' (need parens for precedence)
ix' <- getPositive <$> (shrink . Positive $ ix)
-- Using an 'unwrapper' without parens
ix' <- fmap getPositive . shrink . Positive $ ix
```

### Take extra care with recursive generators

Consider the following example of a [2-3
tree](https://en.wikipedia.org/wiki/2%E2%80%933_tree):

```haskell
data TwoThree (a :: Type) =
   Leaf a |
   Two a (TwoThree a) (TwoThree a) |
   Three a a (TwoThree a) (TwoThree a) (TwoThree a)
```

When writing a generator for this type, you have to be careful. Note that this
is aside from issues of keeping the tree balanced: that's a [separate, much
harder
problem](https://www.cs.tufts.edu/comp/150FP/archive/john-hughes/beginners-luck.pdf).
Something like this is risky:

```haskell
genTwoThree :: forall (a :: Type) . Gen a -> Gen (TwoThree a)
genTwoThree gen = oneof [
    Leaf <$> gen,
    Two <$> gen <*> genTwoThree gen <*> genTwoThree gen,
    Three <$> gen <*> gen <*> genTwoThree gen <*> genTwoThree gen <*> genTwoThree gen
                        ]
```

This has the potential to run forever, or at least a very long time, quite
easily. The reason for this is that `oneof` picks each of its options with equal
probability: since there are two recursive options, and one non-recursive one,
you're much more likely to keep recursing than to stop! On top of that, there is
no 'depth limit' to the recursion either. Together, these two problems are
likely to create absolutely huge values (slowing down the generator), or
possibly even looping forever. In both cases, you'll get no indication of this
from QuickCheck.

To avoid these kinds of problems, QuickCheck provides a _size parameter_; this
is an additional value kept as part of the `Gen` monad, indicating how 'large' a
given generated value should be. We can modify this value each time we recurse,
to ensure that we terminate eventually: when the size gets 'small enough', we
force the use of a non-recursive case.

There are some additional subtleties to consider here:

* For a higher-kinded recursive type, we need to keep the size of the parameter
  independent from the size of the higher-kinded type; otherwise, we will get
  'smaller' values of the parameter as the recursion 'bottoms out', which
  creates bias.
* The size reduction needs to be fairly aggressive: halving is a common
  practice.

An example of the above generator 'done right' is:

```haskell
genTwoThree :: forall (a :: Type) . Gen a -> Gen (TwoThree a)
genTwoThree gen = sized go
  where
    go :: Int -> Gen (TwoThree a)
    go size
      | size <= 0 = Leaf <$> gen
      | otherwise = do
          let halfSize = size `quot` 2
          oneof [
            Leaf <$> gen,
            Two <$> gen <*> go halfSize <*> go halfSize,
            Three <$> gen <*> go halfSize <*> go halfSize <*> go halfSize
            ]
```

This approach has an added benefit: when we increase the size parameter, we also
increase the 'depth' of structures we can produce.

### Use `IsList` and `IsString` to help write generators

In Haskell, there are many 'is basically a list but better' types (such as
`Vector`, `ByteString`, and so on), as well as 'is basically a `String` but
better' types (such as the various flavours of `Text`). These frequently lack
instances of QuickCheck type classes, and thus, generators and shrinkers.
While you could obtain these from external libraries, for more obscure cases,
there may not be support out there. However, in many cases, such types
implement `IsList` or `IsString` (and sometimes, even both): you can use
these to help you define generators and shrinkers. For example, we could
use the `IsList` instance for `ByteString` to define a generator and shrinker:

```haskell
import GHC.Exts (fromList, toList)

genByteString :: Gen ByteString
genByteString = fromList <$> arbitrary

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString = fmap fromList . shrink . toList
```

Similarly, we can use `IsString` to define a generator for `Text`:

```haskell
import GHC.Exts (fromString)

-- Uses a modifier to make sure we get sensible answers!
genText :: Gen Text
genText = fromString . getUnicodeString $ arbitrary
```

### Define `Arbitrary1` (and similar) for higher-kinded types

When generating higher-kinded types (collections, for example), there's two
generation behaviours we are interested in:

* The behaviour of the 'structure' of the type (independent of its parameter);
  and
* The behaviour of the values of the parameter.

For example, consider the following type, representing `n` copies of the same
value:

```haskell
data Repeat (a :: Type) = Repeat Int a
```

We could write an instance such as this:

```haskell
instance (Arbitrary a) => Arbitrary (Repeat a) where
  arbitrary = do
    NonNegative n <- arbitrary
    x <- arbitrary
    pure $ Repeat n x
  -- shrinker not relevant
```

However, this is suboptimal, as it forces the use of whatever the `arbitrary`
instance for any given choice of `a` would produce. This can be a problem if we
want generator behaviour for a given type to be more restrictive for a specific
test.

A better approach is to define an instance of `Arbitrary1` for `Repeat`.
`Arbitrary1` follows the same logic as `Eq1`, `Ord1` and similar: it provides a
way of 'lifting' generation and shrinking to a higher-kinded type. To extend our
earlier analogy, `Arbitrary1` allows you to specify _only_ the generator (and
shrinker) behaviour of the 'structure', and leave the parameter handling to
someone else.

The instance for our type would look something like this:

```haskell
instance Arbitrary1 Repeat where
  liftArbitrary gen = do
    NonNegative n <- arbitrary
    x <- gen
    pure $ Repeat n x
  -- liftShrink not relevant
```

This now allows more precise control of generation for `Repeat`. There is also
`Arbitrary2` for types similar to `Either`.

### Define `CoArbitrary` and `Function` for your types

In addition to generating _values_, QuickCheck is capable of producing arbitrary
_functions_ as well. To see why this might be useful, consider the `local`
method of `MonadReader`:

```haskell
local :: forall (r :: Type) (m :: Type -> Type) (a :: Type) .
    MonadReader r m =>
    (r -> r) ->
    m a ->
    m a
```

This method is known to have the following laws:

```
local id = id

local f >>> local g = local (f >>> g)
```

Suppose we defined an instance of `MonadReader` for a type: we would like to
ensure that it follows its laws. Verifying `local id = id` is fairly
straightforward: it is significantly trickier to verify `local f >>> local g =
local (f >>> g)`, as to be thorough, we need to verify that _arbitrary_ `f` and
`g` will work. Furthermore, it would help us if we could 'minimize' failing
cases, to see why particular `f` and `g` don't work.

QuickCheck has the ability to do both of these. To generate arbitrary functions,
we have the following instance:

```haskell
instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
```

Where the `CoArbitrary` type class is as follows:

```haskell
class CoArbitrary (a :: Type) where
    coarbitrary :: forall (b :: Type) . a -> Gen b -> Gen b
```

The `coarbitrary` method gives us the ability to 'perturb' or 'affect' a
generator for any other type based on values of `a`. This is what enables
QuickCheck to generate arbitrary functions: by using 'perturbations' of the
generator of the function's result, we can have arguments affect outcomes in a
way that an actual function would. Typically, this would be defined using
`variant`:

```haskell
variant :: forall (n :: Type) (a :: Type) . Integral n => n -> Gen a -> Gen a
```

where `variant`'s first argument is a 'perturbation value'. We can use this to
construct `CoArbitrary` instances for sum types as follows:

```haskell
newtype These (a :: Type) (b :: Type) = This a | That b | These a b

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b) where
  coarbitrary x = case x of
    This y -> variant (0 :: Int) >>> variant y
    That z -> variant (1 :: Int) >>> variant z
    These y z -> variant (2 :: Int) >>> variant y >>> variant z
```

Essentially, we 'rank' the 'arms' of the sum, assigning them a unique `Int`
index, 'perturb' using that 'rank', then combine that with 'perturbations' using
the data in the 'arms'. The 'rank' ensures that different 'arms' produce
different 'perturbations'. For product types, we can use function composition:

```haskell
newtype Pair (a :: Type) = Pair a a

instance (CoArbitrary a) => CoArbitrary (Pair a) where
  coarbitrary (Pair x y) = variant x >>> variant y
```

However, `CoArbitrary` does not provide any way to shrink an
arbitrarily-generated function, or even to show it as a failing case. To enable
this, we need a separate type class `Function`:

```haskell
class Function (a :: Type) where
    function :: forall (b :: Type) . (a -> b) -> a :-> b
```

`a :-> b` can be thought of as an 'explicit partial function', represented as a
list of input-output pairs. The `function` method projects a regular function
into this form (lazily); the easiest way to define `function` for a custom type
is to use `functionMap`:

```haskell
functionMap :: forall (b :: Type) (a :: Type) (c :: Type) .
    Function b =>
    (a -> b) -> (b -> a) -> (a -> c) -> a :-> c
```

This essentially provides a reversible transformation from a type `a` (without a
`Function` instance) to a type `b` (with a `Function` instance, which we
borrow). Consider the previous examples of `Pair` and `These`: we could define
`Function` for them as follows:

```haskell
instance (Function a) => Function (Pair a) where
  function = functionMap (\(Pair x y) -> (x, y)) (\(x, y) -> Pair x y)

instance (Function a, Function b) => Function (These a b) where
  function = functionMap outOf into
  where
    outOf :: These a b -> Either (a, Maybe b) b
    outOf = \case
      This x -> Left (x, Nothing)
      That y -> Right y
      These x y -> Left (x, Just y)
    into :: Either (a, Maybe b) b -> These a b
    into = \case
      Left (x, y) -> case y of
        Nothing -> This x
        Just y' -> These x y'
      Right y -> That y
```

We can then use this type class to enable shrinking and showing using the helper
`Fun` type and pattern matching:

```haskell
forallShrinkShow arbitrary shrink show $ \(Fun f) -> somePropertyBasedOn f
```

To enable the widest variety of tests, `CoArbitrary` and `Function` should be
defined for any type that has an `Arbitrary` instance if possible.

## Shrinkers

### Define the `shrink` method for `Arbitrary` instances

By default, the `Arbitrary` type class does not require the `shrink` method; in
that case, QuickCheck will not shrink cases of that type. This is a reasonable
default behaviour from the point of view of the implementers of QuickCheck, but
it can rapidly render QuickCheck completely unusable when writing properties for
the type in question.

To see why, suppose that you define a data type to represent
[bitvectors](https://en.wikipedia.org/wiki/Bit_array):

```haskell
newtype Bitvector = Bitvector (Vector Word64)

-- some functionality

instance Arbitrary Bitvector where
  arbitrary = fromList arbitrary
```

This definition will compile, and will generate the values we expect. However,
when a property _fails_, the value it fails on could be quite large. By default,
QuickCheck can generate lists (and in our case, bitvectors) of length 100,
filled with `Word64`s. Additionally, it is quite unlikely that a large value
would be needed to trigger the failure. This creates the problem that, for us to
figure out what went wrong, we have to shovel through a large (quite frequently,
overly large) value to find the issue.

Shrinking helps us by trying to produce a 'minimal failing case': without it, we
can't really benefit from QuickCheck very much. Thus, it's essential that we
define shrinkers, but only where it makes sense. For a counter-example, consider
a type representing `SHA256` hashes: no shrinker really makes much sense for
them.

### Use the `Alternative` interface for shrinkers (especially `guard`)

A QuickCheck shrinker has the type `a -> [a]`. The intended meaning is that,
given a value, a shrinker non-deterministically produces a 'smaller' or
'simpler' value based on it. Since we're using the list monad (for
non-determinism), we have access to its API; in particular, lists are an
instance of `Alternative`, which lets us use this function:

```haskell
guard :: forall (f :: Type -> Type) . Alternative f => Bool -> f ()
guard cond = if cond then pure () else empty
```

In our context, this translates to 'cut off this branch of non-determinism if
the condition does not hold', or, more specifically to us, 'ensure that any
values we shrink into have this property hold'. We can use this to ensure that
shrinkers don't produce values we don't want: for example, suppose we wanted a
shrinker for `Int`s that wouldn't produce values below 5. We can define it like
this:

```haskell
shrinkFivePlus :: Int -> [Int]
shrinkFivePlus x = do
  x' <- shrink x
  guard (x' > 5)
  pure x'
```

This is quite convenient in itself; furthermore, `guard cond1 *> guard cond2 =
guard (cond1 && cond2)`, which means that you can write multiple conditions in
`do` blocks like this:

```haskell
shrinkMe x = do
  x' <- shrink x
  guard (cond1 x)
  guard (cond2 x)
  ...
  guard (condN x)
  pure x'
```

Instead of like this:

```haskell
shrinkMe x = do
  x' <- shrink x
  guard (all . fmap ($ x) $ [cond1, cond2, ..., condN])
  pure x'
```

This is both easier to read and easier to write. `Alternative` also provides
some functionality that can be useful for shrinkers in some cases (such as
`asum`), but these are more specialized than `guard`, and `guard` is useful even
in simple cases.

### Use `liftShrink` for more precise control of higher-order shrinks

Much as with generators, when shrinking a value of a higher-order type, we have
both a 'structural' shrink and a 'content' shrink. For example, consider lists:
we can shrink them in two ways:

1. By removing items from the list; or
2. By shrinking items without removing them.

Option 1 is 'structural': it doesn't really need to know what kind of values the
list contains; option 2 is 'content'-based: the type of the items is
significant. Much as with generators, we sometimes want to control these two
kinds of shrinks individually.

We can use the `liftShrink` method to do this. Its type is:

```haskell
liftShrink :: forall (a :: Type) (f :: Type -> Type) .
    Arbitrary1 f =>
    (a -> [a]) -> f a -> [f a]
```

Much as with `liftArbitrary`, the first argument is a shrinker for the
'content', which you can choose to be anything. This means, for example, you can
shrink _only_ structurally by using `liftShrink (const [])`, or choose to shrink
in a more constrained way, without having to define a specialized shrinker each
time.

Many common 'container'-style types (lists, `Vector`s, `Map`s and so on) have
suitable instances. There are also instances for types with kinds similar to
`Either`.

## Properties

### Define 'case types' for more intricate tests

[TODO]

### Prefer `forAllShrinkShow`

[TODO]

### Do not use `discard` or `==>`

[TODO]

### Use `A` and similar types for polymorphic properties

Consider the `Functor` type class law `fmap id = id`. The type of either side of
the equation is `forall (f :: Type) (a :: Type) . Functor f => f a -> f a`. If
we wanted to test the law for a particular implementation of `Functor`, the `f`
would be fixed to the type under test, but what should `a` be? While we could
choose any type, the choice is fairly arbitrary, and _any_ such choice could
potentially bias our testing. To avoid this problem, QuickCheck supplies some
'dummy' types `A`, `B` and `C` in `Test.QuickCheck.Poly`, designed to fulfil
exactly this role. Thus, instead of choosing a 'meaningful' type (like `Int`)
for tests such as this one, we can use `A` instead.

These 'dummy' types have a range of useful instances, such as `Arbitrary`,
`CoArbitrary` and `Function`, as well as `Eq`, `Ord` and `Show`, allowing their
use with most QuickCheck machinery. In any cases where we test over polymorphic
types, their use is preferred.

### Use `property` for more flexible definitions

[TODO]

### Use coverage

[TODO]

### Use `label` for feedback on generated data

[TODO]

### Use `PropertyT` for monadic tests

QuickCheck is most straightforward to use for testing pure functions: that is,
functions without effects. Since we only have values, and there's no context
that can affect them, we can 're-run' the test as many times as we like, and
verify its properties in parallel if we choose to. However, we often want to
test _effectful_ functionality as well; to be specific, we also want to be able
to use QuickCheck when the _result_ of the functionality being tested is
effectful, but the inputs are not (necessarily).

For some effects, QuickCheck alone is sufficient (for example, `Maybe`); but for
many others, using QuickCheck 'directly' is awkward. Consider a function with
type `forall (s :: Type) (a :: Type) (b :: Type) . a -> State s b`: when
testing this, we would like to ensure not only that different `a`s behave
correctly, but also different `s`s. This is doable in 'baseline' QuickCheck, but
it's fiddly. Additionally, there are some effects (like `IO` or `ST`) for which
we'd have absolutely no way to proceed without special help.

We can write such tests by using the `PropertyT` transformer. In a sense,
`PropertyT m a` can be thought of as a 'property builder' monad, with `m`
representing the effect the test's results will be in. The typical workflow for
writing such a test is as follows:

1. Define the property you're interested in using a combination of `assert` (for
   the properties) and `run` (to lift effectful values into the test
   definition).
2. Once defined, use `monadic`, `monadicIO` or `monadicST` to transform a
   `PropertyT` into a `Property`.
3. Pass the resulting `Property` as an argument to `forAllShrinkShow`.

All of the functionality needed for this is in `Test.QuickCheck.Monadic`. The
only complex case is `monadic`, which has the type

```haskell
monadic :: forall (a :: Type) (m :: Type -> Type) . (Monad m, Testable a) => (m
Property -> Property) -> PropertyT m a -> Property
```

The first argument is an 'eliminator' function to resolve the effect, while the
type `a` can be any `Testable` (typically this will be `Bool`). In our example
of `State s`, we would use `evalState` or something similar as the 'eliminator'.
For the cases of `IO` and `ST`, we have `monadicIO` and `monadicST`, which work
similarly, but whose 'eliminator' is implicit.

Additionally, there are several other helpers available, the most notable of
which is `monitor`, which allows injecting 'property transformations' like
`label` into `PropertyT`.

## Test definitions

### Pre-emptively solve encoding problems

When using `tasty-quickcheck`, especially in a Nix shell, you can run into
encoding issues, which will cause your tests to fail. To avoid this, you want
to define your `main` as follows:

```haskell
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . testGroup "My tests" $ [
        -- tests go here
        ]
```

This ensures that, whatever the other settings, the tests will run.

### Tell QuickCheck to run (many) more tests

Unless coverage is in use, QuickCheck (and `tasty-quickcheck` by extension) will
only run 100 tests for any property. This is a relatively small number,
especially if the type being generated for the tests is large (or infinite).
This can easily cause a false negative: the tests will pass, but only because we
didn't search hard enough for a counter-example. While QuickCheck is
probabilistic, which means such a thing can happen in any case, running so few
tests makes it much more likely.

To fix this, the easiest method is to use `adjustProperty` from `tasty` with
`tasty-quickcheck`'s `QuickCheckTests`:

```haskell
defaultMain . testGroup $ [
   adjustOption go . testProperty "Some property" $ myProperty
   ]
   where
     go :: QuickCheckTests -> QuickCheckTests
     go = max 10_000
```

This example illustrates a few things:

* You can change the number of tests per-property. If you want to do multiple
  properties at once, use `adustOption` on their parent `TestTree`.
* `QuickCheckTests` has a `Num` instance, allowing you to use numerical syntax
  to say how many tests you want.
* `adjustOption`, together with `max`, allow you to set the _minimum_ number of
  tests you want run, but also allow someone to increase it if they choose (via
  the CLI for `tasty`).

This leads to the question of 'how many tests should I set?'. While it would be
easy to answer 'as many as possible', the real answer is more complicated. While
more tests is better than fewer, you run the risk of making tests take too long,
which leads to them not being used. There's also a possibility that you run a
lot of redundant tests if the type being generated is small. A good starting
point is either 'between 10,000 and 100,000 tests' or '2 to 4 times the
generated type's cardinality': these can be adjusted if the time required is too
long.

### Use `tasty-expected-failure` to mark tests that should fail

Sometimes, it can be useful to mark tests as expected failures: usually, this
is for in-progress work for test-driven development. We can do this using
[tasty-expected-failure](https://hackage.haskell.org/package/tasty-expected-failure-0.12.3/docs/Test-Tasty-ExpectedFailure.html).
