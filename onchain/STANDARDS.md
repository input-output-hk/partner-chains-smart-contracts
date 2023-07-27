# Introduction

This document describes a set of standards for Haskell code in this project. It
also explains our reasoning for these choices, and acts as a living document of
our practices for current and future contributors. We intend for this document
to evolve as our needs change.

# Changelog

## 18/07/23

### Added

* `DuplicateRecordFields` and `OverloadedLabels` are now mandatory extensions.
* New section on records.

## 15/06/23

### Added

* `-fexpose-all-unfoldings` is now a mandatory GHC flag.

## 14/06/23

### Added

* Requirement to define `toEncoding` in addition to `toJSON` for instances of
  `ToJSON`.

## 23/05/23

### Added

* Start changelogging

# Motivation

The desired outcomes from the standards defined in this document are as follows.

## Increase consistency

Inconsistency is worse than _any_ standard, as it requires us to track a large
amount of case-specific information. Software development is already a difficult
task due to the inherent complexities of the problems we seek to solve, as well
as the inherent complexities foisted upon us by _decades_ of bad historical
choices we have no control over. For newcomers to a project and old hands alike,
increased inconsistency translates to developmental friction, resulting in
wasted time, frustration and ultimately, worse outcomes for the code in
question.

To avoid putting ourselves into this boat, both currently and in the future, we
must strive to be _automatically consistent_. Similar things should look
similar; different things should look different; as much as possible, we must
pick some rules _and stick to them_; and this has to be clear, explicit and
well-motivated. This will ultimately benefit us, in both the short and the long
term. The standards described here, as well as this document itself, is written
with this foremost in mind.

## Limit non-local information

There is a limited amount of space in a developer's skull; we all have bad days,
and we forget things or make decisions that, perhaps, may not be ideal at the
time. Therefore, limiting cognitive load is good for us, as it reduces the
amount of trouble we can inflict due to said skull limitations. One of the worst
contributors to cognitive load (after inconsistency) is _non-local information_
- the requirement to have some understanding beyond the scope of the current
unit of work. That unit of work can be a data type, a module, or even a whole
project; in all cases, the more non-local information we require ourselves to
hold in our minds, the less space that leaves for actually doing the task at
hand, and the more errors we will introduce as a consequence.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided; as much locality as possible must be
present everywhere; needless duplication of effort or result must be avoided.
Thus, our work must be broken down into discrete, minimal, logical units, which
can be analyzed, worked on, reviewed and tested in as much isolation as
possible. This also applies to our external dependencies.

Thus, many of the decisions described here are oriented around limiting the
amount of non-local knowledge required at all levels of the codebase.
Additionally, we aim to avoid doing things 'just because we can' in a way that
would be difficult for other Haskellers to follow, regardless of skill level.

## Minimize impact of legacy

Haskell is a language that is older than some of the people currently writing
it; parts of its ecosystem are not exempt from it. With age comes legacy, and
much of it is based on historical decisions which we now know to be problematic
or wrong. We can't avoid our history, but we can minimize its impact on our
current work.

Thus, we aim to codify good practices in this document _as seen today_. We also
try to avoid obvious 'sharp edges' by proscribing them away in a principled,
consistent and justifiable manner.

## Automate away drudgery

As developers, we should use our tools to make ourselves as productive as
possible. There is no reason for us to do a task if a machine could do it for
us, especially when this task is something boring or repetitive. We love Haskell
as a language not least of all for its capability to abstract, to describe, and
to make fun what other languages make dull or impossible; likewise, our work
must do the same.

Many of the tool-related proscriptions and requirements in this document are
driven by a desire to remove boring, repetitive tasks that don't need a human to
perform. By removing the need for us to think about such things, we can focus on
those things which _do_ need a human; thus, we get more done, quicker.

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119]. Specifically:

* MUST, as well as its synonyms REQUIRED or SHALL, describe an absolute
  requirement.
* MUST NOT, as well as its synonym SHALL NOT, describe an absolute prohibition.
* SHOULD, as well as the adjective RECOMMENDED, describe an ‘ideal-world’
  requirement. Unless there exist specific reasons not to follow this
  requirement, it should be followed, and going against this requirement
  should be understood and carefully weighed in its consequences.
* SHOULD NOT, as well as its synonym NOT RECOMMENDED, describe an 'ideal-world'
  prohibition, with similar caveats to SHOULD.
* MAY, as well as the adjective OPTIONAL, describe a 'take-it-or-leave-it'
  situation; it can be followed, or not, and neither is given preference.

Throughout, we refer to _script_ modules and _non-script_ modules. In a _script
module_, we primarily define or use onchain Plutus code; a _non-script_ module
instead primarily contains ordinaly Haskell code. This document places some
requirements on both kinds of module, and some only on one kind: unless stated
otherwise, assume the requirement applies to both.

# Tools

## Compiler settings

The following flags MUST be enabled for all stanzas in the `ghc-options` section
of the Cabal file:

* ``-Wall``
* ``-Wcompat``
* ``-Wincomplete-record-updates``
* ``-Wincomplete-uni-patterns``
* ``-Wredundant-constraints``
* ``-Werror``
* ``-fexpose-all-unfoldings``

Additionally, ``-Wredundant-constraints`` SHOULD be enabled for all stanzas, in
the `ghc-options` section. Exceptions are allowed when the additional
constraints are designed to ensure safety, rather than due to reliance on any
method. If this compiler option is to be disabled, it MUST be disabled in the
narrowest possible scope: this SHOULD be a single module.

Additionally, for script modules, the following flags MUST be enabled:

* ``-fobject-code``
* ``-fno-ignore-interface-pragmas``
* ``-fno-omit-interface-pragmas``
* ``-fplugin-opt PlutuxTx.Plugin:defer-errors``

Additionally, for `test-suite` and `executable` stanzas, the following flags
MUST be enabled:

* ``-O2``
* ``-threaded``
* ``-rtsopts``
* ``-with-rtsopts=-N``

Additionally, for `benchmark` stanzas, the following flag MUST be enabled:

* ``-O2``

Any other compiler settings MUST be specified in the narrowest possible scope
(that is, one module), using an `{-# OPTIONS_GHC #-}` pragma.

### Justification

These options are suggested by [Alexis King][alexis-king-options] - the
justifications for them can be found at the link. These fit well with our
motivations, and thus, should be used everywhere. The ``-Werror`` ensures that
warnings _cannot_ be ignored: this means that problems get fixed sooner. The
more lax enforcement of the use of ``-Wredundant-constraints`` is due to cases
where the constraint is necessary, but GHC can't prove it, as no type class
method use is involved. A classic example is ``HasCallStack``; while the type
class does have a method, there's almost never cause to use it, and GHC can rule
the constraint redundant without it.

Plutus script definitions (due to how they are compiled) require some additional
flags to be set, which produce confusing error messages if not present. We set
these to ensure that we don't have such issues; the ``-fplugin-opt`` avoids
problems with Haddock specifically.

``-fexpose-all-unfoldings`` is a bruteforce hammer of an optimization option: we
mostly enforce its use due to Plutus scripts needing it in some cases to avoid
compilation issues revolving around type class methods. It also helps offchain
Haskell code, especially in cases revolving around type class dictionaries.
While it does increase compile time somewhat, the alternative would be to use it
piecemeal in only those modules whose Plutus scripts require it, which is a
tedious task and leads to confusion as to why this flag is on in some modules,
and not others. Therefore, we enable it globally.

It benefits everyone if tests run as fast as possible: since parallel
capabilities are available (and can be used automatically with Tasty), we should
do that. The flags specified guarantee that parallelism is automatically used;
furthermore, we use ``-O2`` to make sure we get the benefit of most
optimizations. We do something similar for benchmarks, but because parallelism
can interfere with benchmark measurements, we don't require the same flag set.
For executables, we also want to use whatever parallelism is available: hence,
we require the same settings as for tests.

## Linting

Every source file SHOULD be free of warnings as produced by [HLint][hlint], with
default settings; the CI MUST enforce this. The only exception granted is when
enforcing the recommendation would cause the code to no longer compile; in that
case, the warning MUST be disabled on a per-module basis, using an annotation.

### Justification

HLint automates away the detection of many common sources of boilerplate and
inefficiency. It also describes many useful refactors, which in many cases make
the code easier to read and understand. As this is fully automatic, it saves
effort on our part, and ensures consistency across the codebase without us
having to think about it. At times, HLint may offer suggestions that no longer
compile, especially in cases where compiler plugins (like Plutus) are involved:
we allow disabling these suggestions in such cases, but only in a specific
scope.

## Code formatting

Every source file MUST be formatted according to [Fourmolu][fourmolu], with the
following settings (as per its settings file):

* ``indentation: 2``
* ``comma-style: leading``
* ``record-brace-space: true``
* ``indent-wheres: true``
* ``diff-friendly-import-export: true``
* ``respectful: true``
* ``haddock-style: multi-line``
* ``newlines-between-decls: 1``

Each source code line MUST be at most 100 characters wide, and SHOULD
be at most 80 characters wide.

The project's Cabal file MUST be formatted with `cabal-fmt`.

### Justification

Consistency is the most important goal of readable codebases. Having a single
standard, automatically enforced, means that we can be sure that everything will
look similar, and not have to spend time or mind-space ensuring that our code
complies. Additionally, as Ormolu is opinionated, anyone familiar with its
layout will find our code familiar, which eases the learning curve.

Lines wider than 80 characters become difficult to read, especially when viewed
on a split screen. Sometimes, we can't avoid longer lines (especially with more
descriptive identifiers), but a line length of over 100 characters becomes
difficult to read even without a split screen. We don't _enforce_ a maximum of
80 characters for this exact reason; some judgment is allowed.

Cabal files can become quite large and unwieldy, and thus, similarly to code,
it helps to have a fixed formatting convention.

## CI

The project MUST have CI. The CI MUST ensure the following:

* All stanzas in the project compile; namely, that the equivalent of ``cabal
  build --enable-tests --enable-benchmarks`` completes without error.
* The formatting requirements described in 'Code formatting' are enforced.
* The linting requirements described in 'Linting' are enforced.
* All tests pass; namely, that the equivalent of `cabal test all` completes
  without error.

### Justification

CI is an important tool in modern software development practice. It ensures
that reviewers don’t have to worry about machine-checkable issues, helps hold
up standards, and can potentially alert us to issues that arise outside of a
specific developer’s machine. Having the CI not only build the project, but
also run its tests, can help ensure that we don’t accidentally create
regressions, and also reduces reviewer cognitive load.

# Code practices

## Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as
'JSON', 'API', etc) SHOULD be downcased; thus ``repairJson`` and
``fromHttpService`` are correct. Exceptions are allowed for external libraries
(Aeson's ``parseJSON`` for example).

If a name would contain the word 'Transaction' or 'transaction', it SHOULD be
shortened to 'Tx' and 'tx' respectively.

### Justification

camelCase for non-type, non-data-constructor names is a long-standing convention
in Haskell (in fact, HLint checks for it); TitleCase for type names or data
constructors is _mandatory_. Obeying such conventions reduces cognitive load, as
it is common practice among the entire Haskell ecosystem. There is no particular
standard regarding acronym casing: examples of always upcasing exist (Aeson) as
well as examples of downcasing (``http-api-data``). One choice for consistency
(or as much as is possible) should be made however.

The word ‘transaction’ (in both capitalized and non-capitalized form) comes up
often in the context of Plutus-adjacent code; ‘tx’ is not ambiguous, and is
much shorter. Furthermore, this is already a convention in Plutus, so
following it is sensible from the point of view of consistency. In some cases,
it may make more sense not to abbreviate, so we don’t mandate it.

## Imports

All modules MUST use one of the following conventions for imports:

* ``import Foo (Baz, Bar, quux)``
* ``import Foo qualified as F``
* ``import Foo qualified``

We allow an exception for a prelude module: see the Prelude section.

Data types from qualified-imported modules SHOULD be imported unqualified by
themselves:

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as Vector
```

The main exception is if such an import would cause a name clash:

```haskell
-- no way to import both of these without clashing the Vector type name
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VStorable
```

Data constructors SHOULD be imported individually. For example, given the
following data type declaration:

```haskell
module Quux where

data Foo = Bar Int | Baz
```

Its corresponding import should be:

```haskell
import Quux (Foo(Bar, Baz))
```

Record fields MUST be imported alongside their record:

```haskell
import Data.Monoid (Endo (appEndo))
```

For type class methods, the type class and its methods MUST be imported
as so:

```haskell
import Data.Aeson (FromJSON (fromJSON))
```

We grant an exception if only the method is needed, in which case it MUST be
imported like any other function.

Qualified imports SHOULD use the entire module name (that is, the last component
of its hierarchical name) as the prefix. For example:

```haskell
import qualified Data.Vector as Vector
```

Exceptions are granted when:

* The import would cause a name clash anyway (such as different ``vector``
  modules); or
* We have to import a data type qualified as well; or
* The last component of the hierarchical name wouldn't be meaningful (for
  example, ``Data.ByteString.Lazy``).

Qualified imports of multiple modules MUST NOT be imported under the same name.
Thus, the following is wrong:

```haskell
-- Do not do this!
import qualified Foo.Bar as Baz
import qualified Foo.Quux as Baz
```

### Justification

One of the biggest challenges for modules which depend on other modules
(especially ones that come from the project, rather than an external library) is
knowing where a given identifier's definition can be found. Having explicit
imports of the form described helps make this search as straightforward as
possible. This also limits cognitive load when examining the sources (if we
don't import something, we don't need to care about it in general). Lastly,
being explicit avoids stealing too many useful names. We grant an exception for
a prelude, for two reasons:

* It's used everywhere, and thus you have to be familiar with it anyway; and
* Having to qualify it, or import each part you use, is horribly tedious and
  gains very little.

In general, type names occur far more often in code than function calls: we have
to use a type name every time we write a type signature, but it's unlikely we
use only one function that operates on said type. Thus, we want to reduce the
amount of extra noise needed to write a type name if possible. Additionally,
name clashes from function names are far more likely than name clashes from type
names: consider the number of types on which a ``size`` function makes sense.
Thus, importing type names unqualified, even if the rest of the module is
qualified, is good practice, and saves on a lot of prefixing.

Multi-imports under the same qualification is arguably a severe misfeature: in
general, qualified imports must uniquely identify the module that the
identifier comes from to be useful. In any case, this leads to considerable
confusion, as now, to determine the source of an identifier requires checking
multiple modules, rather than just one, and there’s no good reason to do this.

## Exports

All modules MUST have explicit export lists; that is, every module must
explicitly state what exactly it exports. Export lists SHOULD be separated
using Haddock headings:

```haskell
module Foo.Bar (
-- * Types
Baz,
Quux (..),

-- * Construction
mkBaz,
quuxFromBaz,

-- etc
) where
```

We allow exceptions when any of the following hold:

* If Haddocks wouldn't be required for a module according to these standards; or
* If the number of exported identifiers is low; or
* The exported identifier serve a single purpose, apparent from the module's
  name.

In the specific case where a module only provides instances ('compatibility
orphans' for example), the export list MUST be empty.

A module SHOULD NOT re-export identifiers from outside of the project: for
example, the following is not allowed:

```haskell
module Foo.Bar (Value) where

import Data.Aeson (Value)
```

The only exception is for a prelude module.

If a module re-exports identifiers from a different module in the project, the
identifiers MUST be exported individually; thus, ``module`` exports are not
allowed. Furthermore, re-exporting re-exports MUST NOT be done; you can only
export something you define directly, or re-export something defined directly
by a module you import.

### Justification

Explicit export lists are an immediate, clear and obvious indication of what
public interface a module provides. It gives us stability guarantees (namely,
we know that we can change things that aren’t exported without breaking
downstream code at compile time), and tells us where to go looking first when
inspecting or learning the module. Additionally, it means there is less chance
that implementation details ‘leak’ out of the module due to mistakes on the
part of developers, especially those who may not be familiar with the code in
question.

Allowing wildcard exports while disallowing wildcard imports is justified on
grounds of locality of information. Seeing a wildcard import of all a type’s
data constructors or fields doesn’t tell us what these are without looking up
the module from where they are exported; having such an import be explicit
reduces how much searching we have to do, as well as telling us clearly where
certain identifiers come from. However, if we are reading an export list, we
have the type definition in the same file we’re already looking at, making
it fairly easy to check.

Re-exports are a useful feature, allowing us to have 'internal' versus
'external' modules, which make for useful interface stability guarantees.
However, transitive re-exports are an unnecessary obfuscation, both for
people familiarizing themselves with the code, and people trying to use the
code (if it’s a library). Having to follow a daisy chain of multiple re-exports
to get to a definition is tedious, and ultimately not necessary: the only
distinction required are 'internal' modules and 'external' modules, where
'external' modules import 'internal' modules, and then selectively re-export.
The transitive re-export issue counts double when dealing with identifiers
from outside the project: in addition to being even more tedious, this is
arguably an abstraction boundary violation. We forbid module re-exports on the
same grounds as we restrict imports: without knowing exactly what we’re
re-exporting, it makes it very difficult to see what exact public interface we
are presenting to the world. The exception for a custom prelude is a necessity,
but a unique one.

## LANGUAGE pragmata

The following pragmata MUST be enabled for all stanzas:

* ``BangPatterns``
* ``BinaryLiterals``
* ``DataKinds``
* ``DeriveTraversable``
* ``DerivingVia``
* ``DuplicateRecordFields``
* ``EmptyCase``
* ``FlexibleContexts``
* ``FlexibleInstances``
* ``GeneralizedNewtypeDeriving``
* ``HexFloatLiterals``
* ``ImportQualifiedPost``
* ``InstanceSigs``
* ``KindSignatures``
* ``LambdaCase``
* ``MultiParamTypeClasses``
* ``NoStarIsType``
* ``NumericUnderscores``
* ``OverloadedLabels``
* ``OverloadedStrings``
* ``PackageImports``
* ``RebindableSyntax``
* ``ScopedTypeVariables``
* ``StandaloneDeriving``
* ``TupleSections``
* ``TypeApplications``

Any other LANGUAGE pragmata MUST be enabled per-file. All language pragmata MUST
be at the top of the source file, written as ``{-# LANGUAGE PragmaName #-}``.

Furthermore, the following pragmata MUST NOT be used, or enabled, anywhere:

* ``DeriveDataTypeable``
* ``PartialTypeSignatures``
* ``PostfixOperators``

### Justification

Our choice of default extensions is driven by the following considerations:

* How useful or widely-used is this extension?
* Does this extension support other priorities or requirements from our
  standards?
* Would we need to enable this extension in most places anyway, due to
  requirements from our own code (or our dependencies)?
* Does this extension iron out a bad legacy of Haskell, or does it extend the
  language in new (or surprising) ways?
* Does it make GHC Haskell behave similarly to other languages, particularly in
  smaller and narrower-context issues of syntax?
* Is this extension something that needs ‘signposting’, in that it involves
  additional thought or context that wouldn’t be needed otherwise?

Extensions that are widely-used or highly useful, iron out bad legacy,
increase similarity in certain syntactical features common to many languages,
and that don’t require signposting are the primary candidates for inclusion by
default. Additionally, some other standards defined in this document mandate
certain extensions anyway: in this case, having them on by default saves us
having to enable them per-module, which we would have to do otherwise. Lastly,
some of our dependencies (most notably Plutus) make certain extensions
required everywhere, or almost everywhere, just to use them: making these
always-on doesn’t really do anything other than save us needless typing.

``BangPatterns`` are a much more convenient way to force evaluation than
repeatedly using ``seq``. They are not confusing, and are considered
ubiquitous enough for inclusion into the ``GHC2021`` standard. Having this
extension on by default simplifies a lot of performance tuning work, and
doesn’t really require signposting.

``BinaryLiterals``, ``HexFloatLiterals`` and ``NumericUnderscores`` all
simulate syntactic features that are found in many (or even most) other
programming languages. Furthermore, these syntactic features are extremely
convenient in many settings, ranging from dealing with large numbers to
bit-twiddling. If anything, it is more surprising and annoying when these
extensions aren’t enabled, and arguably should have been part of Haskell’s
syntax from the beginning. Enabling these extensions project-wide actually
encourages better practices and readability, and costs almost nothing.
Additionally, use of ``NumericUnderscores`` is suggested by HLint, which makes
not enabling it actively confusing in the context of our project.

``DerivingVia`` provides two benefits. Firstly, it implies
``DerivingStrategies``, which is good practice to use (and in fact, is required
by this document): this avoid ambiguities between different derivations, and
makes the intent of a derivation clear on immediate reading. This reduces the
amount of non-local information about derivation priorities. Secondly,
``DerivingVia`` enables considerable savings in boilerplate in combination
with other extensions that we enable either directly or by implication. While
technically, only ``DerivingStrategies`` would be sufficient for our
requirements, since ``DerivingVia`` is not mandatory and is clearly
signposted, while having no effects beyond its use sites, we enable it
globally for its usefulness.

``DeriveTraversable``, together with ``GeneralizedNewtypeDeriving``, allows us
considerable savings on boilerplate. Firstly, it allows a stock derivation of
``Functor`` (by implication): this is completely safe and straightforward, due
to [parametricity](https://www.schoolofhaskell.com/user/edwardk/snippets/fmap),
and saves us effort in many cases. Secondly, ``GeneralizedNewtypeDeriving``
allows us to 'borrow' any instance from the 'underlying' type: this is also
completely safe and straightforward, as there is no ambiguity as to what that
instance could be. This allows powerful examples such as this:

```haskell
newtype FooM (a :: Type) =
	FooM (ReaderT Int (StateT Text IO) a)
	deriving (
		Functor,
		Applicative,
		Monad,
		MonadReader Int,
		MonadState Text,
		MonadIO
		) via (ReaderT Int (StateT Text IO))
```

The savings in code length alone make this worthwhile; in combination with
their locality and good behaviour, as well as their lawfulness, this makes them
a good candidate for being always on. ``DeriveTraversable`` is, in part,
required to solve the problem of
[not being able to coerce through a ``Functor``](https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/).
While ``Traversable`` is lawful, multiple different law-abiding
implementations are possible in many cases. This combination of factors makes
even ``newtype`` or ``via`` derivations of ``Traversable`` impossible,
requiring special support from GHC, which is exactly what
``DeriveTraversable`` is. This is a historically-motivated inconsistency in GHC
which should really not exist at all: while this only papers over the problem
(as with this extension, only ``stock`` derivations become possible), it at
least means such derivations can be done at all. Having this globally enabled
makes this inconsistency slightly less visible, and due to ``Traversable``’s
lawfulness, is completely safe. While this merely provides _a_ derivation for a
lawful ``Traversable``, rather than _the_ lawful ``Traversable``, this is
still useful and requires no signposting.

``DuplicateRecordFields`` is part of the constellation of extensions designed to
address the deficiencies of Haskell2010 records. In particular, it allows
defining two records in the same scope with identically-named fields:

```haskell
-- Won't work without DuplicateRecordFields
data Foo = Foo {
    bar :: Int,
    baz :: Int
    }

data Bar = Bar {
    bar :: Int,
    baz :: Int
    }
```

We use `DuplicateRecordFields`, together with `OverloadedLabels` and a custom
type class, to address our needs for records: see the Records section for more
information. Since this extension must be enabled both at use sites and at
definition sites, we'd have to enable it almost everywhere: thus, making it
global poses no real issues.

``EmptyCase`` resolves an inconsistency in Haskell2010, as the report allows
us to _define_ an empty data type (that is, one with no constructors), but not
pattern match on it. This should really be in the language, and enabling this
globally resolves a strange inconsistency in the language at no real cost.

``FlexibleContexts`` and ``FlexibleInstances`` paper over a major deficiency in
Haskell2010, which isn’t well-motivated in general. There is no real reason to
restrict type arguments to variables in either type class instances or
constraints: the reasons for this restriction in Haskell2010 are entirely for
the convenience of compiler writers. Having such a capability produces no
ambiguities, and in many ways, the fact that such things are _not_ allowed by
default is more surprising than anything. Furthermore, many core ecosystem
libraries, and even boot libraries (``mtl`` being the most obvious example)
rely on one, or both, of these extensions being enabled. Enabling these
globally is both logical and harmless.

``ImportQualifiedPost`` allows us to write ``import Foo.Bar.Baz qualified as
Quux`` instead of ``import qualified Foo.Bar.Baz as Quux``, as well as the
corresponding ``import MyPackage qualified`` instead of ``import qualified
MyPackage``. This reads more naturally in English, and is more consistent, as
the structure of imports always starts with the module being imported,
regardless of qualification.

``InstanceSigs`` are harmless by default, and introduce no complications. It
is in fact quite strange that by default, we _cannot_ put signatures on type
class method definitions. Furthermore, in the presence of type arguments,
especially higher-rank ones, such signatures are practically mandatory anyway.
Enabling this is harmless, useful, and resolves a strange language
inconsistency.

``LambdaCase`` eliminates a lot of code in the common case of analysis of
single sum type values as function arguments. Without this extension, we
either have to write a dummy ``case`` statement:

```haskell
foo s = case s of
	-- rest of code here
```

Or, alternatively, we need multiple heads:

```haskell
foo Bar = -- the Bar case
foo (Baz x y) = -- the Baz case here
-- etc
```

``LambdaCase`` is shorter than both of these, and avoids us having to bind a
variable only to pattern match it away immediately. It is convenient, clear
from context (especially with our requirement for explicit type signatures),
and doesn’t cause any bad interactions.

``MultiParamTypeClasses`` are required for a large number of common Haskell
libraries, including ``mtl`` and ``vector``, and in many situations. Almost
any project of non-trivial size must have this extension enabled somewhere,
and if the code makes significant use of ``mtl``-style monad transformers, or
defines anything non-trivial for ``vector``, ``MultiParamTypeClasses`` must be
enabled. Furthermore, Plutus makes use of several multi-parameter type classes
(such as ``FromBuiltin`` and ``ToBuiltin``), which also need
``MultiParamTypeClasses`` on to even make use of. Additionally, the original
restriction in Haskell2010 solved by this extension is, much like
``FlexibleInstances`` and ``FlexibleContexts``, put in place for no reason
other than the convenience of compiler writers. Lastly, although having this
extension enabled can introduce ambiguity into type checking, this only
applies in one of two situations:

* When we want to define multi-parameter type classes ourselves, which is rarely
  necessary; or
* If we're using a multi-parameter type class that lacks functional
  dependencies, which we can't do much about anyway.

Enabling ``MultiParamTypeClasses`` globally is practically a necessity given
all of these, and is clear enough that it doesn’t need signposting.

``NoStarIsType`` is a perfect example of a GHC extension covering a legacy
choice, and a questionable one even then. ``*`` being used to refer to ‘the
kind of types’ stems from Haskell’s background in academia: for someone not
aware of the STLC and its academic notational conventions, it’s a fairly
meaningless symbol. Furthermore, ``*`` looks like a type operator, and in fact
clashes with the operator of the same name from ``GHC.TypeNats``, which can
produce some very odd-looking error messages. ``NoStarIsType`` resolves this
problem, particularly in error messages, by replacing ``*`` with ``Type``,
which is more informative and more consistent. There’s no reason not to have
this enabled by default.

``OverloadedLabels``, together with ``DuplicateRecordFields`` and a custom type
class, is our solution for the problems of Haskell2010 records; see the Records
section for more information. As it must be on at every use site of our custom
type class, we would have to enable it everywhere anyway: making it global saves
us some effort.

``OverloadedStrings`` deals with the problem of ``String`` being a suboptimal
choice of string representation for basically _any_ problem, but at the same
time being forced on us by ``base``. This has led to a range of replacements,
of which ``Text`` is generally the most advised; Plutus instead provides its
own ``BuiltinString`` specifically for on-chain use. Having
``OverloadedStrings`` enabled allows us to use string literal syntax for both
``Text`` and ``BuiltinString``, which is convenient for the first and
practically mandatory for the second; it is also fairly obvious in intent. It
is not, however, without problems:

* ``ByteStrings`` are treated as ASCII strings by their ``IsString`` instance,
  which makes its string literal behaviour somewhat surprising. This is widely
  considered a mistake; even worse, ``BuiltinByteString`` mindlessly repeats
  this mistake, then blindly translates it to various ``newtype`` wrappers
  around it.
* Overly polymorphic behaviour in many functions (especially in the presence
  of type classes) often forces either type signatures or type arguments
  together with ``IsString``.
* ``IsString``, similarly to ``IsList``, attempts to handle two arguably quite
  different problems: compile-time resolution of _constants_, and runtime
  conversion of _values_. It also does both badly.
* ``IsString`` is essentially lawless, and cannot be given any laws that make
  sense.

However, these problems (aside from the last one) aren’t usually caused by
``OverloadedStrings`` itself, but by other libraries and their implementations,
either of ``IsString`` itself, or overly-polymorphic use of type classes
without appropriate (or any) laws. A particularly egregious offender is
``KeyValue`` from ``aeson``, which has all the above problems in spades.
However, the convenience of this extension, combined with the fact that for
``BuiltinString`` there basically isn’t another way to form literals, makes
this worth enabling by default. Additionally, we solve the problem of dodgy
``IsString`` instances by use of ``RebindableSyntax`` below.

``PackageImports`` is an extension designed to address a fairly niche, but
quite annoying problem: different packages providing the same module. While
this doesn’t happen often, when it does happen, it’s annoying and practically
impossible to resolve any other way. Additionally, its use is clearly marked
and expresses its intent well. Enabling this globally is harmless.

``RebindableSyntax`` provides us with the benefit of ``NoImplicitPrelude``,
which we have to use due to our policy on preludes. Additionally, it allows us
to correct several bad legacies in our custom prelude:

* ``Num``, and its entire hierarchy, which is lawless and ill-behaved, can now
  be replaced with the hierarchy from `semirings`.
* `ByteString`'s problematic `IsString` instance can now be bypassed.

These don't significantly impact the logic of code, but make for _significantly_
fewer issues in the long run.

``ScopedTypeVariables`` needs to be enabled globally for several reasons. The
primary reason is, when combined with our requirement for explicit signatures
for both type and kind arguments, _not_ having ``ScopedTypeVariables`` on
would produce _extremely_ weird behaviour. Consider the case below:

```haskell
foo :: a -> b
foo = bar . baz
	where
		bar :: String -> b
		bar = ...
		baz :: a -> String
		baz = ...
```

This would cause GHC to produce a _fresh_ ``a`` in the ``where``-bind of
``baz``, and a _fresh_ ``b`` in the ``where``-bind for ``bar``. This is
confusing and makes little sense - if the user wanted a fresh type variable,
they would have named it differently (for example, ``c`` or ``a'``). Worse
still, if this code fails to type check due to errors in the ``where``-binds,
the type error makes no sense, except for those who have learned to spot this
exact situation. This becomes really bad when the type variable is constrained:

```haskell
foo :: (Monoid m) => m -> String
foo = bar . baz
	where
		baz :: m -> Int
		baz = ... -- this has no idea that m is a Monoid, since m is fresh!
```

Furthermore, with the availability of ``TypeApplications``, as well as
possible ambiguities stemming from multi-parameter type classes (which Plutus
has some of), we need to know the order of type variable application. While
there _is_ a default, having to remember it, especially in the presence of
type class constraints, is tedious and error-prone. This becomes even worse
when dealing with skolems, since GHC cannot in general properly infer them.
This leads to hard-to-diagnose, and extremely strange, error messages, which
once again are meaningless to people who aren't specifically familiar with this
problem. Explicit ordering of type variables for this purpose can only be done
with ``ScopedTypeVariables`` enabled, and this also has the benefit of making it
unambiguous and explicit.

Lastly, it could be argued that ``ScopedTypeVariables`` is the way Haskell
ought to work in the first place. If we name a _value_, it propagates
'downward' into _where_-binds - why should types work any differently? The
default behaviour of 'silently refreshing' type variables is thus both
surprising and counter-intuitive. All of these, together with our requirement
for explicit signatures, make having ``ScopedTypeVariables`` being on by
default inevitable, and arguably even the right thing to do.

``StandaloneDeriving``, while not being needed often, is quite useful when
using ``via``-derivations with complex constraints, such as those driven by
type families, or for GADTs. This can pose some syntactic difficulties
(especially with ``via`` derivations), but the extension is not problematic in
and of itself, as it doesn’t really change how the language works, and is
self-signposting. Having ``StandaloneDeriving`` enabled by default is thus
not problematic.

``TupleSections`` smooths out an oddity in the syntax of Haskell2010 regarding
partial application of tuple constructors. Given a data type like this:

```haskell
data Pair a = Pair a a
```

We accept it as natural that we can partially-apply ``Pair`` by writing
``Pair "foo"`` to get something of type ``String -> Pair String``. However,
without ``TupleSections``, the same does not extend to tuple constructors. As
special cases are annoying to keep track of, and this particular special case
serves no purpose, it makes sense to enable ``TupleSections`` by default. This
also smooths out an inconsistency that doesn’t apply to anything else.

``TypeApplications`` is so widely used that it would likely be enabled everywhere
anyway. Not only does Plutus require TypeApplications in many cases (
``ToBuiltin``, ``FromBuiltin``), it’s also common for modern Haskell APIs to
take type arguments directly, rather than passing a ``Proxy`` or an
``undefined`` (as was done before). Furthermore, ``TypeApplications`` is
self-telegraphing, and poses no problems for us, as we consider type arguments
and their order to be part of the API, and require explicit ``foralls`` and
signatures.

We exclude ``DeriveDataTypeable``, as ``Data`` is a strictly-worse legacy
version of ``Generic``, while ``Typeable`` derivations are not needed anymore.
The only reason to enable this extension, and indeed, use either derivation,
is for compatibility with legacy libraries, which we don’t need any of, and
the number of which shrinks every year. Furthermore, ``Data`` is conflictingly
named with the ``Data`` from Plutus Core, which we use at least some of the
time. If we’re using this extension at all, it’s probably a mistake.

``PartialTypeSignatures`` is a misfeature. Allowing leaving in type holes
(to be filled by GHC’s inference algorithm) in finished code is an
anti-pattern for the exact same reasons as not providing top-level type
signatures: while it is (mostly) possible for GHC to infer signatures, we lose
considerable clarity and active documentation by doing so, in return for
(quite minor) convenience. While the use of typed holes during _development_
is a good practice, they should not remain in the final code: to make matters
worse, ``PartialTypeSignatures`` actually works _against_ typed-hole-driven
development, as once GHC has enough information to infer the hole, it won’t
emit any more information. Furthermore, once you start to engage with
higher-rank types, or indeed, practically anything non-trivial at the type
level, GHC’s inference for such holes is often wrong, if it’s decidable at
all. There is no reason to leave behind typed holes instead of filling them
in, and we shouldn’t encourage this.

``PostfixOperators`` are arguably a misfeature. Infix operators already
require a range of special cases to support properly (such as what symbols
create an infix operator, how to import them at the value and type level,
etc); postfix operators make all these special cases even worse. Furthermore,
postfix operators are seldom, if ever, used, and typically aren’t worth the
trouble. Haskell is not Forth, none of our dependencies rely on postfix
operators, and defining our own would create more problems than it would solve.

## Prelude

Script modules MUST use ``TrustlessSidechain.PlutusPrelude``, which MUST be imported
unqualified. If off-chain prelude functionality is required, it MUST come from
``TrustlessSidechain.Prelude``, which MUST be imported qualified as
``TSPrelude``. Other preludes MUST NOT be used.

Non-script modules MUST use ``TrustlessSidechain.Prelude``, which MUST be
imported unqualified. If on-chain prelude functionality is required, it MUST come
from ``TrustlessSidechain.PlutusPrelude``, which MUST be imported qualified as ``PTPrelude``.
Other preludes MUST NOT be used.

### Justification

Haskell is a 35-year-old language, and the ``Prelude`` is one of its biggest
sources of legacy. A lot of its defaults are questionable at best, and often
need replacing. As a consequence of this, a range of 'better ``Prelude``s' have
been written, with a range of opinions: while there is a common core, a large
number of decisions are opinionated in ways more appropriate to the needs of the
authors, rather than other users. This means that, when a non-``base``
``Prelude`` is in scope, it often requires familiarity with its specific
decisions, in addition to whatever cognitive load the current module and its
other imports impose. Furthermore, the dependency footprint of many alternative
``Prelude``s is _highly_ non-trivial, and we need to weigh the cost of
potentially adding many things to our dependency tree.

At the same time, the needs of this project necessitate depending on Plutus, as
well as writing on-chain code. This means that we _already_ have Plutus'
dependencies as mandatory (which includes quite a lot), but also have to use
Plutus-provided tools for writing large parts of the code we deal with. In
particular, for modules that define Plutus scripts, we _must_ use the Plutus
prelude. However, for code where there are no scripts (or very few), such as
tests, the Plutus prelude isn't very useful. This suggests we need _two_
preludes: one for on-chain use, and one for off-chain functionality.

We have to ensure that each of the following goals is met:

1. In both script and non-script modules, there's a minimal amount of friction
   when writing the code they're mostly focused on.
2. If we need functionality from 'the other side' (offchain for script modules,
   onchain for non-script modules), it should be clearly designated.
3. Our choices for the non-script module prelude must be well-motivated for both
   our needs and best practices, _in that order_.

Points 1 and 2 are well-covered by our policy of unqualified import of one
prelude, with possibly qualified use of the other prelude. With regard to point
3, we follow the lead of [What I Wish I Knew When Learning
Haskell](https://smunix.github.io/dev.stephendiehl.com/hask/tutorial.pdf#section*.94):

1. Avoid `String`.
2. Use `fmap` instead of `map`.
3. Use `Foldable` and `Traversable` instead of the `Control.Monad` and
  `Data.List` versions of traversals.
4. Avoid partial functions like `head` and `read` or use their total variants.
5. Avoid exceptions; use `ExceptT` or `Either` instead.
6. Avoid boolean blind functions.

Many of these (specifically 1, 4 and 6, as well as 3 to some extent) are already
mandated or strongly encouraged by the standards described here. We also extend
this with the following:

1. Use the most general version of a function available.
2. If something is mis-named for historical reasons (such as `filterM`), rename
   it.
3. Use a less problematic numerical hierarchy than the one from `base`.
4. Make list alternatives as easy to work with as possible.

To this end, we incorporate a few non-`base` libraries into our prelude: as
these are all transitive dependencies of Plutus, we incur no cost for doing so.
These, along with their reasoning, are:

* ``bytestring``: The ``ByteString`` type, as well as ``ByteString``-based IO.
* ``containers``: The ``Set`` and ``Map`` types.
* ``indexed-traversable``: Various generalizations of traversals,
  particularly of key-value containers like ``Map``.
* ``semialign``: Generalizations of ``zip`` and ``zipWith`` that are better
  than ``MonadZip`` in ``base``.
* ``semirings``: Better numerical hierarchy.
* ``text``: The ``Text`` type, as well as ``Text``-based IO, ``show`` and
  ``fromString`` for literals.
* ``these``: Required for ``semialign`` to be useful.
* ``vector``: The ``Vector`` type.
* ``witherable``: Generalizations of filtering, as well as ``mapMaybe`` and
  similar functions.

We also have to 'shim' some Plutus prelude functionality to work correctly with
some of our extensions (notably `RebindableSyntax`), thus our second wrapper
prelude. We can also use this for stability, as Plutus is known to change
rapidly without warning.

## Records

Records for onchain use SHOULD have `HasField` instances defined for each field.
Record accesses and modifications onchain SHOULD NOT use built-in fields;
instead, they SHOULD use `get`, `put` and `modify` using the appropriate
`HasField` instances. The labels for `HasField` instances SHOULD match the field
names.

Records for onchain use MUST NOT have prefixed fields:

```haskell
-- This is wrong
data Foo = Foo {
    fooBar :: Integer,
    fooBaz :: BuiltinByteString
    }

-- This is correct
data Quux = Quux {
    bar :: Integer,
    baz :: BuiltinByteString
    }
```

### Justification

Records in Haskell, whether 98, 2010, or GHC, are an unmitigated disaster. Due
to a range of factors both historical and priority-related, there is a soup of
extensions designed to address various limitations in Haskell record syntax,
rather than a general language-level fix. These issues include, but are not
limited to:

* Two records with the same field name can’t be used or defined at the same
  time, even if they come from different modules and their use would not be
  ambiguous. Confusingly, they can be in scope if not used or defined!
* Field selectors for _reads_ behave like functions, but for _writes_ are
  awkward, especially for nested use.
* Field selector syntax is different to basically any other language.
* Field selector use is technically optional: records are syntactic sugar over
  ordinary ADTs, and can still be used as ordinary ADTs via pattern matching.
* Field selectors 'clash' with local bindings of the same name. This is even
  worse when you deal with module re-exports or qualified imports; this
  problem can even arise within a module which imports no clashes, even if the
  types would give an unambiguous solution.
* Stability of interface with field selectors is basically impossible if you
  want to change representations. Record patterns help, but their interactions
  with many record-related extensions are brittle and confusing, if they work
  at all.

The extensions that exist to address different subsets of these issues are
many, and their interactions are quite unpredictable and annoying. Some
extensions are required at site-of-declaration, some are required at
site-of-use, some are required at both; error messages for these tend to be
extremely unhelpful, especially for `OverloadedRecordDot` due to the syntactic
clash with function composition and whitespace sensitivity; anti-clashing
extensions don’t resolve all clashes, as field selectors share the namespace
with functions and identifiers some of the time even with these on; different
combinations of extensions are required in different places depending on
imports, local definitions, and possibly external dependencies, in
hard-to-reason-about ways; and many more issues besides. This in itself is a
rat’s nest of problems, which do nothing but eat up developer time and
headspace, when all we want to do is use records in a way even the C
programming language doesn’t create complications for.

What’s worse than all the above is that, as a result of this diversity of
'solutions', every project (and sometimes, every _module_ in a project) ends
up requiring different combinations of extensions. This is confusing, hard to
refactor or clean up, and makes the use of records far more annoying and
tedious than it needs to be, as every developer now has to juggle fifteen
special cases in their heads to understand what exactly they need to have
enabled where. This leads to 'extension soup' as developers just 'blanket
enable' everything in frustration, creating yet more interactions which may
have issues far down the line due to the non-locality of the behaviour of many
of these. This is a completely ridiculous and untenable situation.

While some solutions exist which avoid most, if not all, of these problems,
onchain code presents additional challenges. Firstly, not every valid construct
in Haskell is also valid in Plutus; secondly, even if a construct is _valid_, it
might not be _efficient_, especially from the point of view of size. Both of
these considerations rule out most solutions that work for 'regular' Haskell
(such as optics), requiring us to design something which gives us a similar
degree of usability, without sacrificing onchain costs. ``HasField`` as defined
by our custom Plutus prelude is that solution, as follows:

```haskell
class HasField (l :: Symbol) (r :: Type) (a :: Type) | l r -> a where
  get :: r -> a
  modify :: (a -> a) -> r -> r

-- Allows us this definition
put :: forall (l :: Symbol) (r :: Type) (a :: Type) .
  HasField l r a ->
  a -> r -> r
put x = modify @l (const x)
```

An example of some instances for this type class follows.

```haskell
data Vector3D (a :: Type) = Vector3D {
  x :: a,
  y :: a,
  z :: z
  }

-- | Direct field access
instance HasField "x" (Vector3D a) a where
  {-# INLINE get #-}
  get (Vector3D x _ _) = x
  {-# INLINE modify #-}
  modify f (Vector3D x y z) = Vector3D (f x) y z

-- | 'View'-style
instance HasField "asTuple" (Vector3D a) (a, a, a) where
  {-# INLINE get #-}
  get (Vector3D x y z) = (x, y, z)
  {-# INLINE modify #-}
  modify f (Vector x y z) = let (x', y', z') = f (x, y z) in
    Vector3D x' y' z'
```

While somewhat 'boilerplate-y' in the need to define instances for every record
field, ``HasField`` has numerous advantages:

* `get` and `modify` compose like any other function, including for nested
  access and modification, as well as multiple modifications at once.
* The use of labels to direct which field we are interested in removes any
  ambiguity. Furthermore, this can be stable to refactors, such as field
  renaming or representation changes.
* Both access and modification are driven by the type class resolution
  mechanism: as long as a given record is in scope, so is is the ability to
  access and modify its fields. Furthermore, error messages are clear and
  inference is good, due to careful use of functional dependencies.
* Multiple different 'APIs' over the same record can exist simultaneously for
  the same record, using different `HasField` instances. It is also possible to
  'pre-compose' access and modification for frequently-used combinations for
  convenience.
* The 'sum types over records' problem can be ameliorated by careful definition
  of instances which return `Maybe` for fields that might not be available in
  each variant.
* The cost of `HasField`-based `get`, `modify` and `put` doesn't exceed that of
  pattern matching and manual reconstruction on-chain.

The only outstanding issue is that of record pattern matching and construction,
as well as duplicate field names for different records. `DuplicateRecordFields`
addresses this problem well, provided we don't need to use field accessors. As
that functionality [is due to be removed anyway][accessors], we can enable
`DuplicateRecordFields` and use `HasField` for record access and modification
instead.

## Versioning and changelogging

A project MUST use the [PVP][pvp]. Three, and only three, version numbers MUST be
used: a major version and two minor versions.

Any changes MUST be logged in `CHANGELOG.md`, which MUST comply with [Keep A
Changelog](https://keepachangelog.com/en/1.1.0/) requirements. Each entry SHOULD
provide two additional pieces of information:

* A link to Github issue(s) that the entry corresponds to; and
* A marker (either "onchain" or "offchain") explaining which part of the
  codebase it relates to.

Exceptions are given when:

* The change did not have a Github issue associated with it; or
* The change affects both onchain and offchain.

An example entry is below:

```
* Demons no longer fly out your nose
  ([666](https://github.com/mlabs/trustless-sidechain/issues/666), onchain)
```

### Justification

The [Package Versioning Policy][pvp] is the conventional Haskell versioning
scheme, adopted by most packages on Hackage. It is clearly described, and even
automatically verifiable by use of tools like [``policeman``][policeman]. Thus,
adopting it is both in line with community standards (making it easier to
remember), and simplifies cases such as Hackage publication or open-sourcing in
general. Three version numbers are typically used for most packages, thus making
it the sensible choice for the largest familiarity to most developers.

Changelogs are critical for several reasons: they allow downstream users to
quickly see what's changed, provide context for incoming developers, and
maintain an easier-to-read record of project changes over time. Keep A Changelog
is a relatively common format, which is designed for easy reading, which suits
our needs. In order to add context to our changes, we require 'linking' to
Github issues, as well as a marker of whether the change affects onchain or
offchain: this helps both users and developers get more context easily if they
need to.

## Documentation

Every publically-exported definition MUST have a Haddock comment, detailing its
purpose. If a definition is a function, it SHOULD also have examples of use
using [Bird tracks][bird-tracks]. The Haddock for a publically-exported
definition SHOULD also provide an explanation of any caveats, complexities of
its use, or common issues a user is likely to encounter.

If the code project is a library, these Haddock comments SHOULD carry an
[``@since``][haddock-since] annotation, stating what version of the library they
were introduced in, or the last version where their functionality or type
signature changed.

For type classes, their laws MUST be documented using a Haddock comment.

### Justification

Code reading is a difficult task, especially when the 'why' rather than the
'how' of the code needs to be deduced. A good solution to this is documentation,
especially when this documentation specifies common issues, provides examples of
use, and generally states the rationale behind the definition.

For libraries, it is often important to inform users what changed in a given
version, especially where 'major bumps' are concerned. While this would ideally
be addressed with accurate changelogging, it can be difficult to give proper
context. ``@since`` annotations provide a granular means to indicate the last
time a definition changed considerably, allowing someone to quickly determine
whether a version change affects something they are concerned with.

As stated elsewhere in the document, type classes having laws is critical to our
ability to use equational reasoning, as well as a clear indication of what
instances are and aren't permissible. These laws need to be clearly stated, as
this assists both those seeking to understand the purpose of the type class, and
also the expected behaviour of its instances.

## `let` versus `where`

For function-local definitions, if we require re-use of bindings outside of the
function's arguments, or, in the case of type class methods, type class instance
type variable bindings, `let` MUST be used. Otherwise, if the definition needs
to be used in other `where`-bindings, or if the definition is a function,
`where` MUST be used. Otherwise, `let` MUST be used.

### Justification

`let` and `where` both serve a similar purpose, namely definitions within the
scope of a single function. In this regard, they are _almost_ equivalent; the
only differences between them are as follows:

* `let` allows re-use of bindings in any of its enclosing scopes, while `where`
  allows re-use of function arguments, other `where`s, and for type class
  instances, type class instance variable bindings.
* `let` [should not be
  generalized](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf).
* `let` bindings must be defined 'inline' before their first use, while
  `where` bindings are put at the bottom of the function scoping them.

In many, if not most, cases, which to use is a matter of taste: both can serve
the purpose of introducing most function-local binds. However, in some cases, we
really have to use one or the other: `let` for cases where we need to re-use
bindings that aren't function arguments, `where` for cases where we need to
share among multiple `where`s. At the same time, their _syntactic_ differences
provide the biggest difference in their use: as `let`s must be 'inline' before
their first use, they become unsuitable when the binding is complex, as they end
up 'cluttering' the definition amid the rest of the function's logic. On the
other hand, a `where` gets its own section, similarly to a footnote in text.
Ideally, we should specify a 'complexity boundary' for when a `where` would be
more appropriate.

While exactly what qualifies as 'too complicated for a `let`' is also a matter
of some debate, we choose 'function versus non-function' as a boundary. This
works in most cases, as this leaves `let`-bindings for function call results and
constants, while `where`s contain anything more complex, like a helper function.
At the same time, we don't want to force the use of either in cases where it
would be more convoluted or longer: on that basis, we allow use of `let` or
`where` in cases they're absolutely required, regardless of what they define.
This appears to be a good tradeoff for us between syntactic clarity and
capability, without being too complex to decide between.

## Type and kind signatures

All module-level definitions, as well as ``where``-binds, MUST have explicit
type signatures. Type variables MUST have an explicit ``forall`` scoping them,
and all type variables MUST have explicit kind signatures. Thus, the following
is wrong:

```haskell
-- Do not do this
data Foo a = Bar | Baz [a]

quux :: (Monoid m) => [m] -> m -> m
```

Instead, these should look as follows:

```haskell
data Foo (a :: Type) = Bar | Baz [a]

quux :: forall (m :: Type) . (Monoid m) => [m] -> m -> m
```

Kind signatures must be specified (using ``(a :: Type)``), not inferred
(using ``{a :: Type}``). See
[here](https://www.tweag.io/blog/2020-03-12-expl-spec/) for an explanation of
the difference.

Each explicit type signature MUST correspond to one definition only. Thus, the
following is wrong:

```haskell
bar :: Int
baz :: Int
(bar, baz) = someOtherFunction someOtherValue
```

Instead, write it like this:

```haskell
bar :: Int
bar = fst . someOtherFunction $ someOtherValue

baz :: Int
baz = snd . someOtherFunction $ someOtherValue
```

### Justification

Explicit type signatures for module-level definitions are a good practice in
Haskell for several reasons: they aid type-driven (and typed-hole-driven)
development by providing better compiler feedback; they act as 'active
documentation' describing what we expect a function to (and not do); and they
help us plan and formulate our thoughts while we implement. While GHC can
infer some type signatures, not having them significantly impedes readability,
and can easily go wrong in the presence of more advanced type-level features.

Furthermore, the existence of ``TypeApplications`` now requires us to care not
just about _which_ type variables we have, but also the _order_ in which they
are defined. While there is an algorithm for determining this precisely, this
leads to three unpleasant consequences:

* Those trying to use our definitions with ``TypeApplications`` now have to
  remember the algorithm. Its interactions, especially with type classes, are
  not very straightforward, and typically quite tedious to work through.
* An invisible change at the value level (such as reordering constraints) can
  be an API break.
* The type variables that need to be applied with ``TypeApplications`` may not
  be well-positioned in declaration order, requiring long 'snail trains' of
  inference holes (``@_``).

We avoid all of these problems by explicitly ``forall``ing all of our type
variables: reading the type signature is now sufficient to determine what
order is in use, changes in that order are stable, and we can choose an
optimal ordering with ``TypeApplications`` in mind. This is not only
convenient, but also much easier to understand, and much less brittle.

According to our standards for `let` versus `where`, `where` bindings tend to
contain functions or non-trivial logic. Furthermore, `where`s are often used a
development tool, by creating a hole, to be filled with a `where`-bound helper
function definition. In both of these situations, having a type signature for
`where`-binds helps: for the first case, you get extra clarity, and for the
second case, you can use the type to assist you. This is also advantageous in
cases where we want to refactor by 'promoting' where-binds to the top level,
as the signature is already there for us. This is both helpful and not
particularly onerous, so there's little reason not to do it.

While in theory, it would be beneficial to mandate signatures on `let`-bindings
as well, based on our standards for `let` versus `where`, `let` bindings tend to
be both rarer and simpler, and tend not to get 'promoted' to the top level like
`where`-binds are, due to their re-use of more scoped arguments. Furthermore, in
more complex cases, there's nothing stopping us from putting a signature on a
`let`-binding if needed. Thus, we keep `let` signatures optional.

While it is possible to provide definitions for multiple signatures at once,
it is almost never a good idea. Even in fairly straightforward cases (like the
example provided), it can be confusing, and in cases where the 'definition
disassembly' is more complex (or involves other language features, like named
field puns or wildcards), it is _definitely_ confusing. Furthermore, it’s
almost never really required: it _can_ be more concise, but only at the cost
of clarity, which is never a viable long-term tradeoff. Lastly, refactoring
and documenting such multi-definitions is more difficult. Keeping strictly to
a 'one signature, one definition' structure aids readability and maintenance,
and is almost never particularly verbose anyway.

## Other

In non-script modules, lists SHOULD NOT be field values of types; this extends
to ``String``s. Instead, ``Vector``s (``Text``s) SHOULD be used, unless a more
appropriate structure exists. We allow exceptions for `newtype`s over lists or
``String``s.

In non-script modules, partial functions MUST NOT be defined, and SHOULD NOT be
used, except to ensure that another function is total (and the type system
cannot be used to prove it).

Derivations MUST use an explicit [strategy][deriving-strategies]. Thus, the
following is wrong:

```haskell
newtype Foo = Foo (Bar Int)
    deriving (Eq, Show, Generic, FromJSON, ToJSON, Data, Typeable)
```

Instead, write it like this:

```haskell
newtype Foo = Foo (Bar Int)
    deriving stock (Generic, Data, Typeable)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON, ToJSON)
```

Deriving via SHOULD be preferred to ``newtype`` derivation, especially where the
underlying type representation could change significantly. In non-script
modules, `Show` instances MUST be stock derived, and `Eq` or `Ord` instances
SHOULD be `via`-derived or `newtype`-derived where possible.

``type`` MUST NOT be used.

Sum types containing record fields MUST NOT be defined. Thus, the following is
not allowed:

```haskell
-- Do not do this!
data Foo = Bar | Baz { quux :: Int }
```

### Justification

Haskell lists are a substantial example of the legacy of the language: they (in
the form of singly-linked lists) have played an important role in the
development of functional programming (and for some 'functional' languages,
continue to do so). However, from the perspective of data structures, they are
suboptimal except for _extremely_ specific use cases. In almost any situation
involving data (rather than control flow), an alternative, better structure
exists. Although it is both acceptable and efficient to use lists within
functions (due to GHC's extensive fusion optimizations), from the point of view
of field values, they are a poor choice from an efficiency perspective, both in
theory _and_ in practice. For almost all cases where you would want a list field
value, a ``Vector`` field value is more appropriate, and in almost all others,
some other structure (such as a ``Map`` is even better). We exempt newtypes, as
they don't actually change the runtime representation of either type (and are
useful for avoiding orphan instances), as well as script modules, since there's
no alternative to lists in Plutus.

Partial functions are runtime bombs waiting to explode. The number of times the
'impossible' happened, especially in production code, is significant in our
experience, and most partiality is easily solvable. Allowing the compiler to
support our efforts, rather than being blind to them, will help us write more
clear, more robust, and more informative code. Partiality is also an example of
legacy, and it is legacy of _considerable_ weight. Sometimes, we do need an
'escape hatch' due to the impossibility of explaining what we want to the
compiler; this should be the _exception_, not the rule. This is somewhat relaxed
for script modules, as Plutus makes heavy use of partiality, especially in its
builtins, and the idiomatic way of defining validators or minting policies is as
partial functions; however, even there, we should aim for totality where
possible.

Derivations are one of the most useful features of GHC, and extend the
capabilities of Haskell 2010 considerably. However, with great power comes great
ambiguity, especially when ``GeneralizedNewtypeDeriving`` is in use. While there
_is_ an unambiguous choice if no strategy is given, it becomes hard to remember.
This is especially dire when ``GeneralizedNewtypeDeriving`` combines with
``DeriveAnyClass`` on a newtype. Explicit strategies give more precise control
over this, and document the resulting behaviour locally. This reduces the number
of things we need to remember, and allows more precise control when we need it.
Lastly, in combination with ``DerivingVia``, considerable boilerplate can be
saved; in this case, explicit strategies are _mandatory_.

The only exception to the principle above is newtype deriving, which can
occasionally cause unexpected problems; if we use a newtype derivation, and
change the underlying type, we get no warning. Since this can affect the effect
of some type classes drastically, it would be good to have the compiler check
our consistency.

``type`` is generally a terrible idea in Haskell. You don't create an
abstraction boundary with it (any operations on the 'underlying type' still work
over it), and compiler output becomes _very_ inconsistent (sometimes showing the
``type`` definition, sometimes the underlying type). If your goal is to create
an abstraction boundary with its own operations, ``newtype`` is both cost-free
and clearer; if that is _not_ your goal, just use the type you'd otherwise
rename, since it's equivalent semantically.

The combination of record syntax and sum types, while allowed, causes
[considerable issues](https://stackoverflow.com/questions/37652243/record-syntax-and-sum-types/37657296#37657296).
The main issue here is that such a combination silently sneaks partiality in
'via the back door', which, given our stance against partiality, is definitely
not desirable. While arguably useful in some cases, this extra trouble
doesn’t make it worth its weight.

# Design practices

## Parse, don't validate

[Boolean blindness][boolean-blindness] SHOULD NOT be used in the design of any
function or API. Returning more meaningful data SHOULD be the preferred choice.
The general principle of ['parse, don't validate'][parse-dont-validate] SHOULD
guide design and implementation.

### Justification

The [description of boolean blindness][boolean-blindness] gives specific reasons
why it is a bad choice from a design and usability point of view. In many cases,
it is possible to give back a more meaningful result than 'yes' or 'no, and we
should aim to do this where we can. Designs that avoid boolean blindness are
more flexible, less bug-prone, and allow the type checker to assist us when
writing. This reduces cognitive load, improves our ability to refactor, and
means fewer bugs from things the compiler could have checked had the function
not been boolean-blind.

'Parse, don’t validate' as a design philosophy can be thought of as an
extension of 'no boolean blindness'. Its
[description](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
specifies its benefits and explores this connection.

## No multi-parameter type-classes without functional dependencies

Any multi-parameter type class MUST have a functional dependency restricting its
relation to a one-to-many at most. In cases of true many-to-many relationships,
type classes MUST NOT be used as a solution to the problem.

### Justification

Single-parameter type classes can be seen as subsets of `Hask` (the class of
Haskell types); by this logic, multi-parameter type classes describe
_relations_ on `Hask`. While useful, and a natural extension,
multi-parameter type classes make type inference extremely flakey, as the
global coherence condition can often lead to the compiler being unable to
determine what instance you mean. This can happen even if all the type
parameters are concrete, as anyone can add a new instance at any time.
This comes directly from the assumption by the compiler that a
multi-parameter type class effectively represents an arbitrary many-to-many
relation.

When we do _not_ have arbitrary many-to-many relations, multi-parameter type
classes are useful and convenient. We can indicate this using functional
dependencies, which inform the type checker that our relationship is not
arbitrarily many-to-many: more precisely, we specify that certain type
variables determine others, making the relation more restricted. This is
standard practice in many libraries (``mtl`` being the most ubiquitous
example), and allows us the benefits of multi-parameter type classes without
making type checking more confusing and difficult.

In general, many-to-many relationships pose difficult design choices, for which
type classes are not the correct solution. If we cannot reduce our problem away
from a many-to-many relationship, it suggests that either the design is
incomplete, or it truly needs a many-to-many. This means we must either rethink
the design to eliminate the many-to-many, or deal with it in some other, more
appropriate, way.

## Visible type classes must have laws

Any publically-exported type class MUST have laws. These laws MUST be
documented in a Haddock comment on the type class definition, and all
instances MUST follow these laws.

Type classes which are internal to a module or package, and not exposed
publically, SHOULD have laws. If such laws exist, they MUST be
documented.

### Justification

Type classes are a powerful (and arguably, defining) feature of Haskell, but
can also be its most confusing. As they allow arbitrary ad-hoc polymorphism,
and are globally visible, we must limit the confusion this can produce.
Additionally, type classes without laws inhibit both people seeking to use the
type class method, and also the people who want to define instances of it. The
first group have no idea what to expect - they can’t use equational reasoning,
one of Haskell’s biggest strengths - in a setting where it’s arguably at its
most necessary; the second group have no idea what their instance 'ought to
do'.

Additionally, type classes with laws allow the construction of _provably_
correct abstractions on top of them. This is also a common feature of Haskell:
everything from monadic control structures to profunctor optics are evidence of
this. If we define our own type classes, we want to be able to abstract on top
of them with total confidence that we are going to have correct results.
Lawless type classes make this difficult or outright impossible: consider the
number of abstractions built atop of ``Monad``, as opposed to ``IsString``.

Therefore, by ensuring that all our publically-visible type classes have laws,
we make life easier for both people using their instances, and also defining
new instances. We gain ease of understanding, additional flexibility, and
greater abstractive power.

At the same time, we do occasionally have to define type classes purely for
internal use: a good example is providing _Generic_ derivations. These classes
often cannot have laws, as they’re imposed by another framework, or are
required in order to present a lawful interface publically. While obviously
not ideal, they should be allowed where required, provided that they remain
purely internal.

# Libraries and frameworks

## Use `Type.Reflection` instead of `Data.Typeable`

``Data.Typeable`` from ``base`` SHOULD NOT be used; the only exception is for
interfacing with legacy libraries. Whenever its capabilities are required,
``Type.Reflection`` SHOULD be used instead.

### Justification

``Data.Typeable`` was the first attempt at bringing runtime type information
to GHC; such a mechanism is necessary, as GHC normally performs type erasure.
The original design of ``Data.Typeable.Typeable`` required the construction of
a ``TypeRep``, which could be user-specified, representing the 'structure' of
a type. This led to issues of correctness, as a user-specified ``TypeRep``
could easily violate assumptions, as well as coherency, given that for any
given type, there was no guarantee that its ``TypeRep`` would be unique. These
problems later led to the development of the ``DeriveDataTypeable`` extension,
which made it impossible to define ``Data.Typeable.Typeable`` except through
the mechanisms provided by GHC.

Additionally, as ``Data.Typeable`` predates ``TypeApplications``, its API
requires a value of a specific type to direct which ``TypeRep`` to provide.
This suffers from similar problems as ``Data.Storable.sizeOf``, as there
frequently isn’t a sensible value to provide. This forced developers to write
code like

```haskell
typeOf (undefined :: a)
```

This looks strange, and isn’t the approach taken by modern APIs. Lastly,
``Data.Typeable`` had to be derived for any type that wanted to use its
mechanisms, which forced developers to 'pay' for these instances, whether they
wanted to or not, in case someone needed them.

``Type.Reflection`` has been the go-to API for the purpose of runtime type
information since GHC 8.2. It improves the situation with ``Data.Typeable`` by
replacing the old mechanism with a compiler-generated singleton. Furthermore,
deriving ``Typeable`` is now unnecessary, for the same reason that deriving
``Coercible`` is not necessary: GHC handles this for us. Additionally, the API
is now based on ``TypeApplications``, which allows us to write

```haskell
typeRep @a
```

This system is also entirely pay-as-you-go. Instead of the responsibility
being placed on whoever _defines_ the data types (requiring paying the cost of
the instance whether you need it or not), the responsibility is now placed on
the use sites: if you specify a ``Typeable a`` constraint, this informs GHC
that you need the singleton for ``TypeRep a``, but not anywhere else.

Since ``Type.Reflection`` can do everything ``Data.Typeable`` can, has a more
modern API, and is also lower-cost, there is no reason to use
``Data.Typeable`` anymore except for legacy compatibility reasons.

## Import QuickCheck definitions from `Test.QuickCheck` instead of
`Test.Tasty.Quickcheck`

Identifiers exported from both `Test.Tasty.QuickCheck` and `Test.QuickCheck`
MUST be imported from `Test.QuickCheck`.

### Justification

`Test.Tasty.QuickCheck` re-exports `Test.QuickCheck` in its entirety. This is
designed to be convenient, but is in fact problematic for the same reasons that
led us to forbid the re-export of external dependencies. The re-export is of
whatever version of QuickCheck `tasty-quickcheck` was built against: this could
be older than the version you want to use, which can lead to [unpleasant
surprised](https://github.com/UnkindPartition/tasty/issues/208) together with
`tasty-quickcheck`'s loose bound. To avoid this issue, we require any
QuickCheck-provided identifiers to come from QuickCheck itself.

## Define both `toEncoding` and `toJSON` for instances of `ToJSON`

For any type with a manually specified (that is, not derived via Template
Haskell or `Generic`) `ToJSON` instance, its `toEncoding` method MUST be
defined. Additionally, it MUST be the case that `toEncoding = toEncoding .
toJSON`: that is, the type's encoding must be the same, regardless of method
used.

### Justification

Aeson is a library prone to performance issues. In particular, in older
versions, its serialization routine required first converting a type to `Value`
(essentially a runtime representation of a JSON document), followed by
serializing it. While this approach has advantages, it is extremely inefficient:
an intermediate `Value` gets constructed, only to immediately throw it away. To
avoid this problem, more recent versions of Aeson provide a method for direct
encoding into (effectively) a `ByteString` builder: `toEncoding`, which is part
of the `ToJSON` type class.

Unfortunately, for reasons of backwards compatibility, `toEncoding` is not a
mandatory method. Thus, unless we explicitly define `toEncoding` for every
instance, we don't receive its benefits. Worse, the problem is transitive: if
any type `a` is composite, and includes a value of type `b`, if `b`'s `ToJSON`
instance does not define `toEncoding`, we don't benefit from defining
`toEncoding` for `a`! This is a subtle, but significant performance issue, which
can be significant, in a way that's difficult to detect or pinpoint.

While we cannot ensure that types we don't control define `toEncoding`, we
should ensure that any type we define does. Fortunately, we only need to define
`toEncoding` when we write a manual instance: instances derived via
TemplateHaskell or `Generic` will define `toEncoding` correctly, and 'borrowing'
instances via something like via-derivation will also have a `toEncoding` if the
'underlying' type has it. When we define `toEncoding`, we must ensure that it is
consistent with `toJSON`: it shouldn't matter whether we use the intermediate
`Value` or not.

[pvp]: https://pvp.haskell.org/
[policeman]: https://hackage.haskell.org/package/policeman
[haddock-since]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#since
[bird-tracks]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#code-blocks
[hedgehog-classes]: http://hackage.haskell.org/package/hedgehog-classes
[hspec-hedgehog]: http://hackage.haskell.org/package/hspec-hedgehog
[property-based-testing]: https://dl.acm.org/doi/abs/10.1145/1988042.1988046
[hedgehog]: http://hackage.haskell.org/package/hedgehog
[deriving-strategies]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies
[functor-parametricity]: https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
[alexis-king-options]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build
[hlint]: http://hackage.haskell.org/package/hlint
[fourmolu]: http://hackage.haskell.org/package/fourmolu
[rfc-2119]: https://tools.ietf.org/html/rfc2119
[boolean-blindness]: http://dev.stephendiehl.com/hask/#boolean-blindness
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[hspec]: http://hackage.haskell.org/package/hspec
[rdp]: https://hackage.haskell.org/package/record-dot-preprocessor
[accessors]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst
