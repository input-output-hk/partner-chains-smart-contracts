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
respectively.

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
way that an actual function would.

For product types, we can use function composition on the `coarbitrary` methods
of their fields:

```haskell
data Pair (a :: Type) = Pair a a

instance (CoArbitrary a) => CoArbitrary (Pair a) where
  coarbitrary (Pair x y) = coarbitrary x >>> coarbitrary y
```

For sum types, this is a bit more difficult: we need to 'perturb' the generator
not only based on the fields in any given 'arm' of the sum, but also based on
_which_ arm we are in. Consider the following data type:

```haskell
data These (a :: Type) (b :: Type) = This a | That b | These a b
```

The following `CoArbitrary` instance is problematic:

```haskell
-- Close, but not right!
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b) where
    coarbitrary = \case
        This x -> coarbitrary x
        That y -> coarbitrary y
        These x y -> coarbitrary x >>> coarbitrary y
```

This instance would give bad results with cases where the `a` and `b` type
parameters are the same type (for instance, `These Int Int`), as it does not
distinguish between 'arms', only the values in the 'arms'. Thus, we need to
treat sums as 'tagged products'; essentially, we introduce an additional
'perturbation' based on which 'arm' we are in. To do this, we use `variant`:

```haskell
variant :: forall (n :: Type) (a :: Type) . Integral n => n -> Gen a -> Gen a
```

The first argument to `variant` is a 'perturbation value'. We can use this to
address the issue with the instance for `These`:

```haskell
-- The right way
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b) where
  coarbitrary x = case x of
    This y -> variant (0 :: Int) >>> coarbitrary y
    That z -> variant (1 :: Int) >>> coarbitrary z
    These y z -> variant (2 :: Int) >>> coarbitrary y >>> coarbitrary z
```

Essentially, we 'rank' the 'arms' of the sum, assigning them a unique `Int`
index, 'perturb' using that 'rank', then combine that with 'perturbations' using
the data in the 'arms'. The 'rank' ensures that different 'arms' produce
different 'perturbations'.

While definitely helpful, `CoArbitrary` alone _only_ handles generation of
arbitrary functions: it cannot shrink them, or even show failing cases. To
enable both of these, we need a separate type class:

```haskell
class Function (a :: Type) where
    function :: forall (c :: Type) . (a -> c) -> a :-> c
```

`a :-> b` can be thought of as an 'explicit partial function': 'explicit' in the
sense that we represent it as a collection of input-output pairs, and 'partial'
in the sense that not all input-output pairs are known immediately. The explicit
partial function is generated lazily: whenever an output is needed for an input
we have no pairing for, we generate one, but if we already generated an output
matching a given input, we use the existing output. This representation method
enables both shrinking (by shrinking the collection of pairs) and showing (by
showing the collection of pairs). To enable this, we have the `function` method:
this essentially takes a transformation from `a` to any other type, and
constructs an explicit partial function representation based on it (lazily).

The easiest way to define `function` for a custom type is to use `functionMap`:

```haskell
functionMap :: forall (b :: Type) (a :: Type) (c :: Type) .
    Function b =>
    (a -> b) -> (b -> a) -> (a -> c) -> a :-> c
```

`functionMap` requires two arguments to construct a valid definition of
`function` for a type of our choice: a way to transform the type we want an
instance for (`a` in our case) to another type which already has an instance
(`b` in our case), and a way to 'undo' that transformation. Put another way, for
`functionMap f g`, we expect that `f . g = g . f = id`. By doing this, we can
'borrow' the `Function` instance of `b`, but use it for `a`. Consider the
previous examples of `Pair` and `These`: we could define `Function` for them
using `functionMap` as follows:

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

Here, the 'borrowed' instances are those of `Function a => Function (a, a)`
for `Pair a`, and `(Function a, Function b) => Function (Either (a, Maybe b) b)`
for `These a b`. Note that both of the arguments given to `functionMap` together
form bijections between our type and the type whose instance we are 'borrowing'.
For many types, there are multiple valid instances we could 'borrow': for
example, we could have borrowed `(Function a, Function b) => Function (Either a
(Either b (a, b))` for `These a b` instead.

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

### Prefer `forAllShrinkShow`

When defining properties, there is a range of options to combine _preparation_
tasks (generation of inputs, methods of shrinking, what to show when cases need
to be displayed) versus the property itself (what to do with the generated
data). The most general of these is `forAllShrinkShow`:

```haskell
forAllShrinkShow :: forall (prop :: Type) (a :: Type) .
    Testable prop =>
    Gen a ->
    (a -> [a]) ->
    (a -> String) ->
    (a -> prop) ->
    Property
```

Essentially, all the preparation tasks are handled via explicit, user-specified
arguments. There exist some variants of `forAllShrinkShow` where some, or all,
of the preparation tasks are implicit through type classes (`Arbitrary` and
`Show`). While these appear convenient, there are reasons to prefer
`forAllShrinkShow`:

1. `show` is rarely a good way to display failing cases, especially for more
   complex data. By allowing us to specify this manually, `forAllShrinkShow`
   enables us to use a better method (such as a prettyprinter) without having to
   use `Show` for that purpose.
2. We can write tests even when the type we need lacks an `Arbitrary` instance
   directly (such as by way of `Arbitrary1`).
3. If we need to modify any of the preparation, we can do that inline, instead
   of having to change type class instances.
4. `forAll` (the most simplified option) has the habit of not shrinking.

Thus, in general, `forAllShrinkShow`, while slightly more verbose, is the better
option for long-term clarity and maintenance.

### Do not use `discard` or `==>`

Consider a `TextMap a` data type representing a mapping from `Text` keys to
values of type `a`, with the following functions:

```haskell
lookup :: forall (a :: Type) . Text -> KeyMap a -> Maybe a

fromList :: forall (a :: Type) . [(Text, a)] -> KeyMap a
```

An initial attempt at a test for these functions would be similar to this:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    if (k, v) `elem` kvs
    then lookup k (fromList kvs) === Just v
    else discard
```

QuickCheck even provides a shorthand way of writing something similar with the
`==>` operator:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    ((k, v) `elem` kvs) ==> (lookup k (fromList kvs) == Just v)
```

However, this test is not very useful. Because we generate the key, value and
source list independently, it is highly improbable that we happen to generate a
key-value pair that exists in the source list. In such a case, QuickCheck will
simply not check anything, and continue. In the best case scenario, your test
will pass, but with a large number of 'discarded' cases: for example, even if
you ran 100,000 tests, but your discard rate was 90%, you really only ran 10,000
tests: a far smaller number than expected. In worse cases, the discard rate will
be so high that the test will fail (although this is unreliable). The biggest
problem, however, is that even if the test passes, it's lying to you: it checks
only the case where the 'precondition' holds, even though when the precondition
_fails_, something else should hold instead.

For all of these reasons, use of `discard` and `==>` is incorrect: it fosters
bad practices and tests that don't say what we think. Instead, you should use
'case types' and coverage, described in more detail below.

### Use coverage

Continuing with our example from before, suppose we re-wrote our test as follows
to avoid `discard` and `==>`:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    if (k, v) `elem` kvs
    then lookup k (fromList kvs) === Just v
    otherwise lookup k (fromList kvs) === Nothing
```

While this eliminates a problem, it arguably introduces an even worse one: now,
our test is _silently_ lying to us about the correctness of our code.
Essentially, the problem remains the same: it is far more likely that
independently-generated `(k, v)` will _not_ be part of `kvs`, which means that
we test the `Nothing` case far more than the `Just` case. While in the current
situation, this flaw is clear, with more complex tests and conditions, this
error may appear silently.

To avoid this problem, QuickCheck provides a range of tools. The first of these
is `label`:

```haskell
label :: forall (prop :: Type) .
    Testable prop =>
    String -> prop -> Property
```

This function takes a label, supposed to be constructed from the test data, and
as the tests run, counts how many unique labels it sees. At the end, QuickCheck
displays what percentage of cases ended up with which label. To use this
functionality in our test, we could do something like this:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    let cond = (k, v) `elem` kvs in
      label ("keyval in source: " <> show cond) $
      if cond
      then lookup k (fromList kvs) === Just v
      else lookup k (fromList kvs) === Nothing
```

When running, we will see something similar to the following:

```
      our test description:                                  OK (6.19s)
        +++ OK, passed 10000 tests:
        99.24% keyval in source: False
         0.76% keyval in source: True
```

As `label` returns a 'property transformation' function, you can sequence these
together for multiple `label`s for different conditions:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    let cond (k, v) `elem` kvs
        keyLength = Text.length k in
      label ("keyval in source: " <> show cond) .
      label ("key size " <> show keyLength) $
      if cond
      then lookup k (fromList kvs) === Just v
      else lookup k (fromList kvs) === Nothing
```

While `label` is one way of finding out the distribution of cases, it's usually
too specific to be useful: a more general tool is `classify`:

```haskell
classify :: forall (prop :: Type) .
    Testable prop =>
    Bool ->
    String ->
    prop ->
    Property
```

`classify` is different from `label` in that, instead of the `String` argument
being used to separate generated data into cases, the `Bool` argument (again,
supposedly derived from the test data) determines if we 'match the case' or not.
Instead of using `label`, we can rewrite our test using `classify`:

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    let cond = (k, v) `elem` kvs in
      classify cond "keyval in source" $
      if cond
      then lookup k (fromList kvs) === Just v
      else lookup k (fromList kvs) === Nothing
```

Just as with `label`, we can 'chain together' multiple uses of `classify`, and
indeed, uses of `classify` and `label`.

Both `label` and `classify` are strictly informative: they will only indicate
what distribution(s) our generated cases have. If we want to ensure our tests
only pass if certain distributions have been achieved, we instead need to use
the following functions:

```haskell
cover :: forall (prop :: Type) .
    Testable prop =>
    Double -> Bool -> String -> prop -> Property

checkCoverage :: forall (prop :: Type) .
    Testable prop =>
    prop -> Property
```

`cover` behaves similarly to `classify`, but with an extra `Double` argument,
which is a 'target percentage' (from 0 to 100), and we can use it in a similar
way. However, `cover` also changes how tests are run. Normally, QuickCheck will
simply try as many cases as we request (100 by default), but with `cover`,
QuickCheck will instead track both what percentage of generated cases satisfy
the condition, and how many tests we've run so far, until one of the following
happens:

1. QuickCheck finds that we have achieved (almost) the requested distribution,
   and that running more cases won't cause the distribution to get worse.
2. QuickCheck finds that we have not achieved the requested distribution, and
   that running more cases won't cause the distribution to improve.

In either case, QuickCheck will stop running the tests at that point (regardless
of how many we requested), and in case 2, will indicate that it failed to
achieve the requested coverage. This helps us resolve the 'how many tests to
run' problem, as QuickCheck can handle this for us. Additionally, if we also use
`checkCoverage` to 'modify' our property, case 2 will cause the tests to fail.
Thus, the correct way to write our test would be

```haskell
forAllShrinkShow arbitrary shrink show $ \(k, v, kvs) ->
    let cond = (k, v) `elem` kvs in
      checkCoverage .
      cover 50.0 cond "keyval in source" $
      if cond
      then lookup k (fromList kvs) === Just v
      else lookup k (fromList kvs) === Nothing
```

This will ensure that, if half of our cases aren't 'hits', the test will fail,
as well as ensuring we run exactly enough tests to ensure this happens (or never
can).

Whenever possible, we should use these tools (referred to as 'coverage
checking') to make sure our tests are, in fact, telling us what we think they
are. It is worth noting, however, that QuickCheck cannot be told two conditions
are mutually exclusive. Specifically, if we have:

```haskell
cover 50.0 cond1 "something" . cover 50.0 cond2 "something else"
```

QuickCheck will assume that `cond1` and `cond2` could be true simultaneously.
While this won't cause problems with logic if they _are_ mutually-exclusive, it
will mean more tests will be run than would be needed if this information could
be made available to QuickCheck. This is not usually a serious limitation,
unless we have a large number of conditions.

### Define 'case types' for more intricate tests

The above examples with the `TextMap` test show a major problem that can often
arise: properties frequently require multiple inputs, which may not be
completely independent of one another. In our running example, we have a
dependency between the key-value pair and the source list, which is meant to
yield two different outcomes, in roughly equal proportion. However, the standard
approach to generating 'bundled property data' assumes that each 'piece' is
independent of the others: if that's not your situation, this creates a whole
host of problems.

While this is [generally a very hard
problem](https://www.cs.tufts.edu/comp/150FP/archive/john-hughes/beginners-luck.pdf),
we can simplify it somewhat by use of 'case types': wrappers around combination
arguments with custom `Arbitrary` instances and a range of helpers. For example,
in our case, we could define a 'case type' like so:

```haskell
data LookupCase (a :: Type) =
    LookupHitCase Text a [(Text, a)]) |
    LookupMissCase Text a [(Text, a)])
    deriving stock (Eq, Show)

instance (Arbitrary a) => Arbitrary (LookupCase a) where
    arbitrary = oneof [genHitCase, genMissCase]
    shrink = \case
        LookupHitCase k v kvs -> do
            ...
            pure . LookupHitCase k' v' $ kvs'
        LookupMissCase k v kvs -> do
            ...
            pure . LookupMissCase k' v' $ kvs'

toCaseData :: forall (a :: Type) .
    LookupCase a -> (Text, a, [(Text, a)])
toCaseData = ...

isHitCase :: forall (a :: Type) .
    LookupCase a -> Bool
isHitCase = \case
    LookupHitCase{} -> True
    _ -> False

showLookupCase :: forall (a :: Type) .
    LookupCase a -> String
showLookupCase = ...
```

We can see several components to this pattern:

1. A custom sum type, which has one 'arm' for each case, together with all the
   data needed for that case.
2. An `Arbitrary` instance, which generates each case with equal probability,
   and also ensures that shrinkers do not 'shrink out of case'.
3. A 'disassembler' function that pulls out our test data irrespective of our
   case.
4. A 'discriminator' function which informs us which case we're in.
5. A way of displaying the data in a more human-readable way.

With such a type at our disposal, we can rewrite our test like this:

```haskell
forAllShrinkShow arbitrary shrink showLookupCase $ \lookupCase ->
    let cond = isHitCase lookupCase
        (k, v, kvs) = toCaseData lookupCase in
      checkCoverage .
      cover 50.0 cond "keyval in source" $
      if cond
      then lookup k (fromList kvs) === Just v
      else lookup k (fromList kvs) === Nothing
```

This has a range of advantages:

* All the complexity of generation and shrinking is hidden away, which means
  that the person writing tests doesn't have to worry about it.
* The coverage conditions are no longer dependant on the test writer getting
  them correct.
* We have the ability to change representations, or improve the efficiency of
  generators and shrinkers, without breaking any tests relying on them.
* We can 'hack' our generators to ensure we don't fail coverage requirements,
  and also ensure that our shrinkers don't take us 'out of case'.
* We can bundle useful metadata into the 'case type' to avoid having the
  property definition re-derive it, possibly at some expense (for example, list
  lengths).

In effect, we're making the generation and shrinking opaque to the property
definition by strictly separating concerns. This allows us to use coverage more
easily, and improves test organization.

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
