Data Versioning System
======================

1 The Problem
=============

trustless-sidechain currently implements a script update/versioning system
designed in [SIP-01](./01-UpdateStrategy.md).  We need to create a similar
system for storing and updating data on-chain, so that we can store sidechain
parameters in a modular and way.  Examples include:

  * the D parameter
  * list of permissioned candidates

These two parameters currently have their own dedicated validator scripts, datum
definitions, and minting policies for a distinguished token that certifies
datum's authenticity.  This approach does not scale though, since every time we
want to add a new parameter we are required to create a new set of definitions
(minting policy, validator, datum).

Thus the goal of this SIP: **create a data versioning system that allows to
insert, update, and remove arbitrary data.**

2 Proposed design
=================

We propose to re-use the design of the already existing script versioning system
with several adjustments.

2.1 Version oracle validator and token
--------------------------------------

Data versioning system requires a version oracle script that stores the
versioned data, and a version oracle token attached to each entry.  Version
oracle validator and token already exists for the script versioning system.  We
can take one of two approaches here.


### 2.1.1 Extend existing version oracle to include data in addition to scripts

The currently implemented version oracle is specialized to store only the
versioned scripts.  We could extend it so that it can also handle storing the
data.  The required changes should be minimal and focus on the redeemers, which
are currently specialized to `VersionOracle` data type designed for handling
scripts.  See Section 2.2 below for discussion of the datum itself.

Pros:

  * little extra code required, thus less code to maintain

Cons:

  * slightly higher runtime overhead, since now the oracle will store both
    scripts and data.  This means more data to process at runtime.

To elaborate on the above con, as of this writing we have twelve versioned
scripts and two versioned parameters (data).  To recover one of those two
versioned parameters we would have to iterate through a total of fourteen
entries in the versioning system.  At the moment this seems of little concern
due to small number of versioned scripts and data, but could potentially become
more problematic at some point in the future.  Note also that at the moment we
don't have accurate means of measuring actual performance so this problem is
highly speculative.

Note also that at the moment versioned data is primarily consumed through
db-sync.  See Section 4 below for db-sync related concerns.


### 2.1.2 Create a separate data version oracle by duplicating existing code

Alternatively, we can duplicate existing code and specialize it to work with
data versioning system.  We will thus have two, nearly identical, versioning
systems: one for the scripts and the other for data.

Pros:

  * less data to process at runtime, since each validator only stores its
    dedicated contents (scripts or data)

Cons:

  * code duplication, thus higher maintenance and further development costs


### Discussion

While it might seem that the second proposed solution is the more efficient one,
the actual performance benefits are unknown since we have no way of measuring
the exact costs.  At the moment it seems to make more sense to optimize the
development costs, not the speculative runtime costs.  Thus the author of this
document opts for the first proposed solution.  If we ever discover that
performance improvements need to be made in the versioning system we will split
it into two, or perhaps consider other optimizations.  Until then let's just
make development faster.

The rest of this document is written under the assumption that we use the first
proposed approach, although if we decide to use the second design the required
changes will be minimal.


2.2 Version oracle datum
------------------------

Assuming we implement solution proposed in 2.1.1, we will be in a situation
where the version oracle validator stores UTxOs with two different types of
data.  One type of datum will be attached to versioned scripts, the other to
versioned data.  It is important that the two types of data are distinguishable
by their Plutus encodings.

The currently existing script versioning system attaches the following datum to
each versioned script:

```haskell
data VersionOracle = VersionOracle
  { -- | Version of the script.
    version :: Integer
  , -- | Unique identifier of the validator.
    scriptId :: Integer
  }
```

Data versioning system requires a different version oracle datum.  Obviously, we
want a separate set of data identifiers, thus we'd like to replace `scriptId`
field with `dataId`.  Note however, that if both fields are represented using an
`Integer` their encoding becomes indistinguishable.

Then there is the question of what to do with the `version` field?  In the
currently existing use cases (D parameter and the permissioned candidates list)
we only ever store a single version, we never care what that version is, and we
primarily use db-sync to recover current and historical values.  Two approaches
are then possible here:

  1. Keep the `version` field and hide its existence behind the user interface.

     Pros:
     - future-proof design that supports versioning if necessary

     Cons:
     - lots of extra unnecessary work with the interface

  2. Drop the `version` field and only add it in the future if necessary.

     Pros:
     - simple and exactly what we need now

     Cons:
     - Possibly breaking changes in the future

In the rest of this document we make a working assumption of using the second
approach.

All that being said, we propose to rename `VersionOracle` to
`ScriptVersionOracle` and create a separate datum for the data versioning
system:

```haskell
data DataVersionOracle = DataVersionOracle
  { -- | Unique identifier of the datum.
    dataId :: Integer
  , -- | Versioned data.
    versionedData :: BuiltinData
  }
```

The `versionedData` field stores a serialized data that the client of the data
versioning system must know how to decode.

Note that the versioned data should be stored as `DatumInline`.  This has the
added benefit of being able to use these entries as reference datum shall the
need ever arise.


3 User Interface
================

Even though the proposed system is a general one, i.e. it is agnostic about what
data is being stored, we need to provide a concrete user interface in the
`trustless-sidechain` CLI.  Two options come to mind.

3.1 A General Interface
-----------------------

We can create a general interface that accepts serialized value of the
`versionedData` field to put into the versioning system.  An example usage would
look like this (omitting common parameters such as `--sidechain-id` etc. and
leaving only the ones relevant for this proposal):

```
nix run .#sidechain-main-cli -- insert-data \
  --data-id 42
  --seralized-data "[Constr 0 [Constr 1 [B \"\\196F\\250\\240\\232\\DC1tB\\193\\235\\188\\154:V\\146\\226\\156\\225\\DC3]\\244\\\\]u\\235c\\214r\"], Constr 1 []]]"
```

Such an interface could easily version any data.  So if we decide we need to
store a new parameter in the data versioning system no extensions in the CLI is
necessary.  An obvious drawback of this proposal is the need to provide a
serialized datum on the command line, and then ensure all special symbols in the
string are masked correctly so that the shell doesn't interpret them in a
special way.  This is difficult and problematic.

3.2 A specialized interface
---------------------------

Instead of creating a general-purpose interface we keep the existing specialized
interface.  With this approach each versioned parameter needs its own set of
insert/update/remove commands.  This adds a non-trivial implementation burden
when adding a new parameter to the versioning system: one needs to add new
commands and their responses, as well as create parsers for all the new CLI
flags.  An obvious advantage is the ease of use by end users.

3.3 Implement both interfaces
-----------------------------

We can also implement both interfaces, under the assumption that whenever we add
a new parameter to be versioned we also extend the CLI (per 3.2 above).  At the
same time in emergency situations the general interface will allow us to version
a parameter.


4 Retrieving historical values
==============================

Currently existing uses cases for data versioning require that we also have
access to historical values.  For example, we need to be able to tell what was
the value of the D parameter at a given epoch.  Currently this is implemented by
recovering historical values from db-sync.

This proposal makes no attempt to provide any built-in mechanism for storing
historical values, other than permitting to store multiple versions of the same
datum, assuming we implement bullet 1) from section 2.2.  At the same time
implementing this proposal will complicate retrieving historical data from
db-sync.  In the currently existing implementation each versioned parameter
(i.e. D parameter and permissioned candidate list) has its own script address.
To find all historical values it suffices to query all UTxOs that were stored at
the dedicated script address and filter them by currency symbol that represents
a token identifying valid entries.

If this proposal is implemented this method will not suffice, since multiple
different data will be stored at the same validator address and identified by
the same currency symbol.  In order to distinguish between different data we
will need to deserialize the datum and filter by `dataId` field, which at the
db-sync level could be a problem.

An alternative is to store the `dataId` inside the token name.  This should
allow filtering by `dataId` in db-sync without the need to deseralize the actual
data.  If we implement 2.1.1 then putting ID into token name would have a
knock-on effect on the script versioning system, but this might not be a bad
thing.  Perhaps storing ID in the token name will allow us to drop the
`dataId`/`scriptId` field from the datum?


5 Closing thoughts
==================

The proposed design assumes that the data to be stored inside the datum is small
enough to fit into a single UTxO.  This need not be the case in practice.  For
example, in theory the list of permissioned candidates could grow to a size
where it no longer fits into the datum.  The design above makes no provision for
covering such a use case.  We will address this problem if it ever arises in
practice.
