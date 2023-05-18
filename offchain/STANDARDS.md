# Introduction

This document describes a set of standards for PureScript code under this
project. It also explains our reasoning for these choices, and acts as a living
document of our practices for current and future contributors to the project. We
intend for this document to evolve as our needs change, as well as act as a
single point of truth for standards.

# Motivation

The desired outcomes from the standards specified in this document are as
follows.

## Increased consistency

Inconsistency is worse than _any_ standard, as it requires us to track a large
amount of case-specific information. Software development is already a difficult
task, due to the inherent complexities of the problems we seek to solve, as well
as the inherent complexities foisted on us by _decades_ of bad historical
choices we have no control over. For newcomers to a project and old hands alike,
increased inconsistency translates to developmental friction, resulting in
wasted time, added frustration, and ultimately, worse outcomes for the code in
question.

To avoid putting ourselves into this boat, both now and in the future, we must
aim to be _automatically_ consistent. Similar things should look similar;
different things should look different; as much as possible, we must pick some
rules _and stick to them_; and this has to be clear, explicit, and
well-motivated. This will ultimately benefit us, both in the short and long
term. The standards described here, as well as this document, are written with
this foremost in mind.

## Limited non-local information

There is a limited amount of space in a developer's skull; we all have bad days,
and we forget things or make decisions that, perhaps, may not be ideal at the
time. Therefore, limiting cognitive load is good for us, as it reduces the
amount of trouble we can cause due to said skull limitations. One of the worst
contributors to cognitive load (after inconsistency) is _non-local information_
- the requirement to have some understanding beyond the scope fo the current
unit of work. That unit of work could be a data type, a module, or even a whole
project; in all cases the more non-local information we have to hold in our
minds, the less space we have left for actually doing the task at hand, and the
more mistakes we will make as a consequence.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided; as much locality as possible must be
present everywhere; needless duplication of effort or result must be avoided.
Thus, our work must be broken down into discrete, minimal, logical units, which
can be analyzed, worked on, reviewed and tested in as much isolation as
possible. This also applies to our external dependencies.

Thus, many of the decisions described here are designed to limit the amount of
non-local information needed at all levels of the codebase. Additionally, we
avoid doing things 'just because we can' that would be different for others to
follow, regardless of skill level.

## Minimized legacy impact

Every language, framework, library, or indeed any code at all, contains legacy
decisions. These decisions are frequently outdated, problematic, or outright
wrong. Furthermore, PureScript, like many languages, contains embedded issues or
choices that we must live with, as they cannot be fixed by us, either easily, or
at all. We can't avoid these problems altogether, but we _can_ minimize their
impact on our current work.

Thus, we codify good practices as seen by us, _today_. We also try to avoid
obvious 'sharp edges' by proscribing them away in a principled, consistent and
justifiable manner.

## Drudgery automated away

As developers, we should use our tools to make ourselves as productive as
possible. There's no reason for us to do a task if a machine can do it for us,
especially when this task is something boring or repetitive. We love PureScript
as a language not least of all for its capability to abstract, to describe, and
to make fun what other languages make dull or impossible: our work should do the
same.

Many of the tool-related requirements here are driven by a desire to overcome
boring and repetitive tasks that don't need human intervention. By removing the
need to think about such things, we can focus on those things which _do_ need a
human; thus, we get more done, quicker.

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119].

# Tools

## Compiler warning settings

The `--strict` mode of [`psa`](https://github.com/natefaubion/purescript-psa)
MUST be used for any build; this does not apply to dependencies. Furthermore,
errors MUST be stashed. Lastly, `UserDefinedWarning` MUST be disabled. The
following command illustrates how to set this:

```
spago build --purs-args="--stash --censor-lib --censor-codes=UserDefinedWarning
--strict
```

### Justification

PureScript, by default, permits a lot of questionable behaviours with only
warnings, including:

* Unused top-level declarations
* Missing type declarations
* Use of deprecated identifiers
* Implicit re-exports
* Deriving `Generic` for types whose constructors aren't exposed

The listed cases are explicitly counter to the described standards here, and
should be detected. Furthermore, other warnings indicate problems, which should
be fixed, rather than left be. Forcing them to become errors ensures that they
don't remain behind. While it would be ideal to have our dependencies be
similarly tidy, we can't enforce this in general.

By default, neither Spago nor the PureScript compiler persist errors (or
warnings): if something successfully compiles, or fails but doesn't change,
errors will not be re-displayed unless their source module changes. This can be
annoying, especially when working across multiple modules: thus, we use `psa`'s
stashing support.

We ignore `UserDefinedWarning` due to a set of warnings from CTL which are
benign, specifically warnings like these:

```
This function returns only one `PaymentPubKeyHash` even in case multiple `PaymentPubKeysHash`es are available. Use `ownPaymentPubKeysHashes` instead
```

While it would be better to disable these on a per-file basis, currently,
PureScript doesn't allow this, hence our solution.

## Linting

Every source file MUST be free of warnings as produced by [ESLint][eslint], with
default settings.

### Justification

ESLint automates away the detection of many common sources of boilerplate and
inefficiency. It also describes many useful refactors, which in many cases make
the code easier to read and understand. As this is fully automatic, it saves
effort on our part, and ensures consistency across the codebase without us
having to think about it.

## Code formatting

Every PureScript source file MUST be formatted according to `purs-tidy`. Every
Dhall source file MUST be formatted according to `dhall lint`.

Each PureScript or JavaScript source line MUST be at most 100 characters wide,
and SHOULD be at most 80 characters wide.

Type class instance definitions MUST be separated by one blank line. Thus, the
following is wrong:

```purescript
derive instance Generic Foo _
derive instance Newtype Foo _
```

### Justification

Consistency is the most important goal of readable codebases. Having a single,
automatically-enforced standard means that we can be sure everything will look
similar, and not have to spend time or mind-space ensuring that our code
complies. The tools we use are either standard in their respective communities
(`purs-tidy` for PureScript), or are actually part of the language itself
(`dhall lint`); this ensures that anyone familiar with any of these kinds of
source will find our code easy to follow.

Lines wider than 80 characters become difficult to read, especially when viewed
on a split screen. Sometimes, we can't avoid longer lines (especially with more
descriptive identifiers or long string constants), but a line length of over 100
characters becomes difficult to read even without a split screen. We don't
_enforce_ a maximum of 80 characters for this exact reason; some judgment is
allowed.

Keeping good whitespace separation between significant units of meaning
(function definitions, type definitions and so on) makes it visually much
clearer where one unit ends and another unit begins. This applies just as much
to type class instances, if not more so, as they range in length from a single
line (typical `Newtype` or `Generic` instances) to many lines, with possibly
their own helper binds (many others). Keeping them all separated with one blank
line ensures that it's clear where each instance ends.

# Code practices

## Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as
'JSON', 'API', etc) SHOULD be downcased; thus ``repairJson`` and
``fromHttpService`` are correct. Exceptions are allowed for external libraries
(Aeson's ``parseJSON`` for example).

### Justification

cameCase for non-type, non-data-constructor names is a convention in PureScript
and JavaScript; TitleCase for type names or data constructors in _mandatory_ in
PureScript. Following these conventions reduces cognitive load, as it is common
practice in the entire ecosystem. There is no particular standard regarding
acronym casing: we made the given choice for consistency, but there's no
particular reason for this.

## Imports

Each import line for any module MUST use one of the following conventions:

* ``import Foo (Baz, class Bar, quux)``
* ``import Foo as OurFoo``
* ``import Foo (Baz (Baz), quux) as ExportFoo``

Data types from named imports SHOULD be imported by themselves:

```purescript
import Data.Array (Array)
import Data.Array as Array
```

An exception is granted if such an import would cause a name clash.

Data constructors MUST be imported individually. For example, given the
following data type declaration:

```purescript
module Quux (Foo (..)) where

data Foo = Bar Int | Baz
```

Its corresponding import is:

```purescript
import Quux (Foo (Bar, Baz))
```

Named imports SHOULD use the entire module name (that is, the last component of
its hierarchical name) as the prefix. For example:

```purescript
import Data.Array as Array
```

Exceptions are granted when:

* The import would cause a name clash anyway; or
* We have to import a data type qualified as well.

Each module imported named MUST have a unique prefix. Thus, the following is not
allowed:

```purescript
import Foo as Foo
import Bar as Foo
```

All modules MUST import `Contract.Prelude` from `ctl` as so:

```purescript
import Contract.Prelude
```

This is a specific, singular exception to the policy of imports. Other preludes
MUST NOT be used.

Import sections MUST be a singular block with no blank lines separating import
lines. Thus, the following is wrong:

```purescript
import Foo as Foo

import Bar as Bar
```

The `Contract.Prelude` import MUST be listed first, with a single blank line
between it and other imports: this is enforced by our tooling, and is the only
exception to the above.

### Justification

One of the biggest challenges for modules which depend on other modules
(especially ones that come from the project, rather than an external library) is
knowing where a given identifier's definition can be found. Having explicit
imports of the form described helps make this search as straightforward, and as
localized, as possible. This also limits cognitive load when examining the
sources (if we don't import something, we don't need to worry about it in
general). Lastly, being explicit avoids stealing too many useful names.

In general, type names occur far more often in code than function calls. We have
to use a type name every time we write a type signature, but it's unlikely we
use only one function that operates on said type. Thus, we want to reduce the
amount of extra noise needed to write a type name if possible. Additionally,
name clashes from function names are far more likely than name clashes from type
names: consider the number of types on which a ``size`` function makes sense.
Thus, preferring un-named imports of type names, even if the rest of the module
uses a named import, is good practice, and saves on a lot of prefixing.

The Purescript prelude is extremely limited: it fails to include many
frequently-used types (such as `Maybe`) and functions (such as `pure`).
Furthermore, as our project uses CTL in a fairly fundamental way, we would have
to import many CTL-related identifiers anyway. `Contract.Prelude` solves both
problems, as it includes both useful builtin identifiers and useful CTL
identifiers with only a single import. For reasons of clarity and consistency,
we only allow the CTL prelude, and allow it to be imported without either naming
or restriction. We also 'single it out' in the import block for clarity.

Separating imports into 'sections' seems initially like a logical idea. However,
it actually creates more problems than it solves: firstly, it makes git diffs
much more difficult, and secondly, it means that imports are no longer
alphabetically organized, even with automatic formatting. The first is a problem
because it makes reviews and merges harder for no reason; the second is a
problem because now instead of having _one_ place to search for an identifier,
you now have _n_ places, where _n_ is the number of such 'sections'. Lastly, any
such 'sectioning' is extremely subjective, and difficult to keep consistent
between modules even in theory, much less in practice. Thus, we forbid it
outright: one automatically-alphabetically-sorted module block simplifies diffs
and also makes finding identifier sources easy.

## Exports

All modules MUST have explicit export lists.

Modules which export identifiers MUST export only the following:

* Identifiers defined in themselves; and
* Identifiers defined in a project module they themselves import.

Furthermore, 'export length' MUST NOT exceed 1. 'Direct' exports (that is,
exports of identifiers defined in the exporting module) are considered to have
an 'export length' of 0 for this purpose, while a re-export has length _n + 1_,
where _n_ is the 'export length' of the identifier in the module it is being
imported from.

When re-exporting identifiers from another module, the module where the
re-exports are coming from MUST be imported using a convention like so:

```purescript
import Foo (Bar (Bar), quux) as ExportFoo
```

More precisely: each exported identifier MUST be defined explicitly, the prefix
MUST begin with `Export`. Module re-exports MUST be only of modules imported in
this way.

### Justification

Explicit export lists are an immediate, clear and obvious indication of what
publically visible interface a module provides. It gives us stability guarantees
(namely, we know we can change things that aren't exported and not break
downstream code at compile time), and tells us where to go looking first when
inspecting or learning the module. Additionally, it means there is less chance
that implementation details 'leak' out of the module due to errors on the part
of developers, especially new developers.

Once you discover where an import comes from, nothing is more confusing than
having to dig through a long 'chain' of imports to find a definition. It's even
worse when an external dependency is being re-exported through a
project-specific module: not only do you end up having to chase your way through
(possibly several) re-export modules, you then find you have to look up external
documentation anyway. Having such 'chains' of re-exports isn't really useful
anyway, as you can get 'private modules' with one 'level' of re-exports. We lose
nothing by prohibiting long 'export chains', or re-exporting external dependency
identifiers, but gain considerable clarity and ease of reading.

Re-exports from another module need to be clearly designated, especially when
the module also exports some of its own identifiers. To help this, we require
naming any module from which we re-export an identifier, and using a qualified
name.

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

This is identical to the standard for Haskell code (described in its own
document); we adopt this for consistency and simplicity.

## Documentation

Every exported identifier MUST have a documentation comment, detailing its
purpose. If the identifier is a function, it SHOULD also have examples of use.
The documentation comment for any identifier SHOULD also provide explanations of
any caveats, complexities of its use, or common issues a user is likely to
encounter. An exception is granted when an identifier's name alone is clear
enough, and no such caveats exist.

For type classes, their laws MUST be documented using a documentation comment,
in a separate section:

```purescript
{- | Describes an 'addition' operation.

     Laws

     1. `x + y = y + x`
     2. `x + (y + z) = (x + y) + z`
-}
class AdditiveSemigroup a where
...
```

### Justification

Code reading is a difficult task, especially when the 'why' rather than the
'how' of the code needs to be deduced. A good solution to this is documentation,
especially when this documentation specifies common issues, provides examples of
use, and generally states the rationale behind the definition.

As stated elsewhere in the document, type classes having laws is critical to our
ability to use equational reasoning, as well as a clear indication of what
instances are and aren't permissible. These laws need to be clearly stated, as
this assists both those seeking to understand the purpose of the type class, and
also the expected behaviour of its instances.

## Type and kind signatures

Every module-level identifier, and `where` binding, MUST have a type signature.
Any type variables MUST have kind signatures when first introduced.
`let`-bindings CAN have a type signature.

Type holes MUST NOT be left in finished code: they CAN be used during
development, but MUST be filled in before code is committed.

Each identifier MUST have its own signature; thus, the following is wrong:

```purescript
foo, bar :: String
```

### Justification

The power of PureScript's type system is significant: it allows us to describe
(and actively document) functionality in a way other type systems can't, which
helps us not only avoid mistakes, but can even guide us towards a solution.
While PureScript _can_ infer (many) types even if we don't provide a signature,
this is suboptimal for several reasons:

* Having a signature acts as 'active documentation' which both informs the
  reader what the function can do, and is checkable by the compiler.
* Signatures together with [typed
  holes](https://github.com/purescript/purescript/issues/1283#issuecomment-423122704)
  can be a huge development aid, as they allow us to figure out what we need to
  'fill the hole'.
* Many signatures cannot be inferred (especially involving row types or type
  classes, or where types would be ambiguous).

All of the above are advantageous enough that we make module-level signatures
mandatory. We also mandate signatures for `where` binds, as these often define
complex functionality such as helper functions, while `let` binds are usually
for simpler cases (though they can benefit from signatures in any case).

Typed holes are a useful development tool, but they shouldn't be left in
finished code: even if the signature is inferrable, it removes the benefit of
having a signature in the first place. Thus, we allow their use, but any code
that gets committed cannot contain them.

## Other

``type`` SHOULD NOT be used. The only exception is abbreviation of large row
types. In particular, ``type`` MUST NOT be used to create an abstraction
boundary.

### Justification

``type`` is generally a terrible idea in PureScript. Firstly, unlike `newtype`,
it doesn't create an abstraction boundary: any function accepting values of the
original type will also accept values of the synonym, as to the type system,
they're identical. Secondly, the compiler will _always_ expand a type synonym in
any error messages: this creates dissonance between the code and the error
message, which creates unnecessary mental work to resolve. Thus, if your goal is
to create an abstraction boundary which shares a representation, `newtype` is
both cost-free and clearer; if that's _not_ your goal, it's clearer to use the
type you'd otherwise rename, since it's semantically equivalent.

The only reasonable (or at least justifiable) use of `type` is for large row
types, as these can become extremely long. However, even this is somewhat
questionable, as the problem of conflict between code and error messages
remains. At the same time, whether this is beneficial or not is left to
individual judgment.

# Design practices

## Parse, don't validate

[Boolean blindness][boolean-blindness] SHOULD NOT be used in the design of any
function or API. Returning more meaningful data SHOULD be the preferred choice.
The general principle of ['parse, don't validate'][parse-dont-validate] SHOULD
guide design and implementation.

### Justification

The [description of boolean blindness][boolean-blindness] gives specific reasons why it is a poor
design choice; additionally, it runs counter to the principle of ['parse, don't
validate][parse-dont-validate]. While sometimes unavoidable, in many cases, it's
possible to give back a more meaningful response than 'yes' or 'no, and we
should endeavour to do this. Designs that avoid boolean blindness are more
flexible, less bug-prone, and allow the type checker to assist us when writing.
This, in turn, reduces cognitive load, improves our ability to refactor, and
means fewer bugs from things the compiler _could_ have checked if a function
_wasn't_ boolean-blind.

## Type classes must have laws

Any type class which is exported, and not from an external dependency, MUST have
laws. These laws MUST be documented in a documentation comment accompanying the
type class definition. All instances of any type class MUST follow its laws.

### Justification

Type classes are a powerful feature, but can also cause confusion. As they allow
arbitrary ad-hoc polymorphism, and are globally visible, we have to carefully
limit the resulting confusion. While PureScript is better in this regard than
Haskell (orphan instances are forbidden, for example), many of the issues that
type classes bring still remain. Additionally, type classes without laws inhibit
equational reasoning, which is a big strength of PureScript, _especially_ in the
presence of what amounts to arbitrary ad-hoc polymorphism.

Type classes having laws thus benefit both implementers of the class (clear and
verifiable abstractions) and implementers of instances (it's clear whether the
instance is 'correct' or not). Additionally, they allow the construction of
_provably_ correct abstractions on top of them. PureScript, like Haskell, is
filled with examples, ranging from `pure` to `traverse`. Without laws, it's
difficult, or outright impossible, to do this. Therefore, we mandate that
publically visible classes have laws, and that any instances we write must
follow laws, if they exist. We permit non-exported type class instances to lack
laws, as these are often 'helpers' for functionality and aren't otherwise
visible.

## `Generic` derivations require fully open constructors

If a type has a `Generic` derivation, it MUST export its data constructors.

### Justification

`Generic` gives us two capabilities for any instance:

* The ability to convert any value of that type into a 'general' representation;
  and
* The ability to take that representation and convert it back into a value of
  that type.

These two capabilities basically mean that the data constructors of that type
are de-facto exposed: hiding them away doesn't stop people constructing whatever
values of that type its data constructors permit. If this is undesirable for
some reason (such as invariants that the type system doesn't enforce), a
`Generic` instance should not be defined for the type.

[pvp]: https://pvp.haskell.org/
[property-based-testing]: https://dl.acm.org/doi/abs/10.1145/1988042.1988046
[functor-parametricity]: https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
[alexis-king-options]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build
[eslint]: https://eslint.org/
[rfc-2119]: https://tools.ietf.org/html/rfc2119
[boolean-blindness]: http://dev.stephendiehl.com/hask/#boolean-blindness
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[rdp]: https://hackage.haskell.org/package/record-dot-preprocessor
